// Go-to-definition provider (US-412, slice C / AC3).
//
// Resolves what is under the cursor from the AST — a class reference or a
// message selector — then returns every matching definition from the workspace
// index. Smalltalk is dynamically typed, so a selector yields *all* implementor
// candidates (returned as a `Location[]`, same-file first). Pure
// (vscode-languageserver-types only).

import { type Location } from 'vscode-languageserver-types';
import { NodeKind, type Node } from '../parser/ast';
import { parse } from '../parser/parser';
import { SymbolKind } from '../parser/symbols';
import type { IndexEntry, WorkspaceIndex } from './workspaceIndex';

export interface DefinitionQuery {
  /** `class` → a class/namespace reference; `selector` → a message send. */
  readonly target: 'class' | 'selector';
  readonly name: string;
}

function isNode(value: unknown): value is Node {
  return (
    typeof value === 'object' &&
    value !== null &&
    typeof (value as { kind?: unknown }).kind === 'string' &&
    typeof (value as { start?: unknown }).start === 'number'
  );
}

/** Visit every AST node (generic — walks any Node-valued property/array). */
function visit(node: Node, fn: (n: Node) => void): void {
  fn(node);
  for (const key of Object.keys(node)) {
    const value = (node as unknown as Record<string, unknown>)[key];
    if (Array.isArray(value)) {
      for (const item of value) {
        if (isNode(item)) {
          visit(item, fn);
        }
      }
    } else if (isNode(value)) {
      visit(value, fn);
    }
  }
}

/** What does the cursor (byte `offset`) point at — a selector send or a name reference? */
export function resolveDefinitionQuery(text: string, offset: number): DefinitionQuery | undefined {
  const { ast } = parse(text);
  let best: { query: DefinitionQuery; start: number; end: number } | undefined;
  const consider = (query: DefinitionQuery, start: number, end: number): void => {
    // Prefer the most specific (deepest) node: latest start, then earliest end.
    if (!best || start > best.start || (start === best.start && end < best.end)) {
      best = { query, start, end };
    }
  };

  visit(ast, (node) => {
    if (offset < node.start || offset > node.end) {
      return;
    }
    if (node.kind === NodeKind.Message && node.selector !== '' && offset >= node.receiver.end) {
      // Cursor is in the selector/argument region — this send's selector.
      consider({ target: 'selector', name: node.selector }, node.start, node.end);
    } else if (node.kind === NodeKind.Variable) {
      consider({ target: 'class', name: node.name }, node.start, node.end);
    }
  });

  return best?.query;
}

/** All index definitions matching the query, same-file-first. */
export function findDefinitions(index: WorkspaceIndex, query: DefinitionQuery, currentUri: string): Location[] {
  const matches = index.all().filter((e: IndexEntry) => {
    if (e.name !== query.name) {
      return false;
    }
    return query.target === 'class'
      ? e.kind === SymbolKind.Class || e.kind === SymbolKind.Namespace
      : e.kind === SymbolKind.Method;
  });
  matches.sort((a, b) => Number(b.uri === currentUri) - Number(a.uri === currentUri));
  return matches.map((e) => ({ uri: e.uri, range: e.selectionRange }));
}
