// Document-highlight provider (US-417, slice B / AC2).
//
// Highlights the other occurrences of the symbol under the cursor within the
// file — scope-aware, not a naive same-text match:
//   - a message selector -> its sends (the selector token spans),
//   - a variable -> its same-name references within the nearest binding scope
//     (block/method params + temporaries), or file-wide if unbound (class/global).
// Variable scoping is shared with US-426 rename via parser/scope.ts. Pure
// (vscode-languageserver-types only); selector spans come from the token stream.

import { type DocumentHighlight, DocumentHighlightKind, type Range } from 'vscode-languageserver-types';
import { NodeKind, type MessageNode, type ProgramNode } from '../parser/ast';
import type { Token } from '../parser/token';
import { visit } from '../parser/walk';
import { bindingScope, pathToOffset, variableOccurrences } from '../parser/scope';
import { resolveQueryInAst } from './definition';

/** The first token within [from, to) byte offsets matching `pred`. */
function tokenIn(tokens: Token[], from: number, to: number, pred: (t: Token) => boolean): Token | undefined {
  for (const t of tokens) {
    if (t.start >= from && t.end <= to && pred(t)) {
      return t;
    }
  }
  return undefined;
}

/** The source range(s) of a message's selector token(s), derived from the gaps
 *  between the receiver and the arguments (so nested sends aren't included). */
function selectorRangesOf(message: MessageNode, tokens: Token[]): Range[] {
  const out: Range[] = [];
  if (message.messageType === 'keyword') {
    const parts = message.selector.match(/[^:]+:/g) ?? [];
    let from = message.receiver.end;
    for (let i = 0; i < parts.length; i++) {
      const arg = message.arguments[i];
      const to = arg ? arg.start : message.end;
      const tok = tokenIn(tokens, from, to, (t) => t.text === parts[i]);
      if (tok) {
        out.push({ start: tok.startPos, end: tok.endPos });
      }
      from = arg ? arg.end : to;
    }
  } else {
    const arg = message.arguments[0];
    const to = message.messageType === 'binary' && arg ? arg.start : message.end;
    const tok = tokenIn(tokens, message.receiver.end, to, (t) => t.text === message.selector);
    if (tok) {
      out.push({ start: tok.startPos, end: tok.endPos });
    }
  }
  return out;
}

export function documentHighlightsAt(ast: ProgramNode, tokens: Token[], offset: number): DocumentHighlight[] {
  const query = resolveQueryInAst(ast, offset);
  if (!query) {
    return [];
  }

  if (query.target === 'selector') {
    const out: DocumentHighlight[] = [];
    visit(ast, (node) => {
      if (node.kind === NodeKind.Message && node.selector === query.name) {
        for (const range of selectorRangesOf(node, tokens)) {
          out.push({ range, kind: DocumentHighlightKind.Read });
        }
      }
    });
    return out;
  }

  // Variable / name reference: scope to the nearest binding, else file-wide.
  const scope = bindingScope(pathToOffset(ast, offset), query.name);
  return variableOccurrences(scope, query.name).map((occ) => ({
    range: { start: occ.startPos, end: occ.endPos },
    kind: occ.kind === 'write' ? DocumentHighlightKind.Write : DocumentHighlightKind.Read,
  }));
}
