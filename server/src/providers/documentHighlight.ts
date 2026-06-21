// Document-highlight provider (US-417, slice B / AC2).
//
// Highlights the other occurrences of the symbol under the cursor within the
// file — scope-aware, not a naive same-text match:
//   - a message selector -> its sends (the selector token spans),
//   - a variable -> its same-name references within the nearest binding scope
//     (block/method params + temporaries), or file-wide if unbound (class/global).
// Pure (vscode-languageserver-types only); derives selector spans from the token
// stream so the parser/AST are untouched.

import { type DocumentHighlight, DocumentHighlightKind, type Range } from 'vscode-languageserver-types';
import { NodeKind, type MessageNode, type Node, type ProgramNode } from '../parser/ast';
import type { Position, Token } from '../parser/token';
import { childNodes, visit } from '../parser/walk';
import { resolveQueryInAst } from './definition';

interface Ranged {
  readonly start: number;
  readonly end: number;
  readonly startPos: Position;
  readonly endPos: Position;
}

function toRange(r: Ranged): Range {
  return { start: r.startPos, end: r.endPos };
}

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
        out.push(toRange(tok));
      }
      from = arg ? arg.end : to;
    }
  } else {
    const arg = message.arguments[0];
    const to = message.messageType === 'binary' && arg ? arg.start : message.end;
    const tok = tokenIn(tokens, message.receiver.end, to, (t) => t.text === message.selector);
    if (tok) {
      out.push(toRange(tok));
    }
  }
  return out;
}

/** Root → deepest chain of nodes whose range contains `offset`. */
function pathToOffset(root: Node, offset: number): Node[] {
  const path: Node[] = [];
  let node: Node | undefined = root;
  while (node) {
    path.push(node);
    node = childNodes(node).find((c) => c.start <= offset && offset <= c.end);
  }
  return path;
}

/** The nearest enclosing scope (block/method/program) that binds `name`, else the program. */
function bindingScope(path: Node[], name: string): Node {
  for (let i = path.length - 1; i >= 0; i--) {
    const node = path[i] as Node;
    if (node.kind === NodeKind.Block || node.kind === NodeKind.MethodDefinition) {
      const declares =
        node.parameters.some((p) => p.name === name) || node.temporaries.some((t) => t.name === name);
      if (declares) {
        return node;
      }
    } else if (node.kind === NodeKind.Program && node.temporaries.some((t) => t.name === name)) {
      return node;
    }
  }
  return path[0] as Node;
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
  const out: DocumentHighlight[] = [];
  const seen = new Set<string>();
  const push = (r: Ranged, kind: DocumentHighlightKind): void => {
    const key = `${r.start}:${r.end}`;
    if (!seen.has(key)) {
      seen.add(key);
      out.push({ range: toRange(r), kind });
    }
  };
  // The declaration(s) in the binding scope.
  if (scope.kind === NodeKind.Block || scope.kind === NodeKind.MethodDefinition) {
    for (const p of scope.parameters) {
      if (p.name === query.name) push(p, DocumentHighlightKind.Write);
    }
  }
  if (scope.kind === NodeKind.Block || scope.kind === NodeKind.MethodDefinition || scope.kind === NodeKind.Program) {
    for (const t of scope.temporaries) {
      if (t.name === query.name) push(t, DocumentHighlightKind.Write);
    }
  }
  // References within the scope subtree (assignment targets are writes).
  visit(scope, (node) => {
    if (node.kind === NodeKind.Assignment && node.target.name === query.name) {
      push(node.target, DocumentHighlightKind.Write);
    } else if (node.kind === NodeKind.Variable && node.name === query.name) {
      push(node, DocumentHighlightKind.Read);
    }
  });
  return out;
}
