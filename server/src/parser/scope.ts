// Scope resolution for variables (extracted from US-417 documentHighlight, reused
// by US-426 rename). Pure: AST + offsets only. Shadow-aware — it never descends
// into a nested block/method that re-binds the name, so an inner `:name` param can
// never be conflated with an outer temp/ivar of the same name.

import { NodeKind, type Node } from './ast';
import type { Position } from './token';
import { childNodes } from './walk';

export interface Ranged {
  readonly start: number;
  readonly end: number;
  readonly startPos: Position;
  readonly endPos: Position;
}

export type OccurrenceKind = 'write' | 'read';
export interface Occurrence extends Ranged {
  readonly kind: OccurrenceKind;
}

/** Root → deepest chain of nodes whose range contains `offset`. */
export function pathToOffset(root: Node, offset: number): Node[] {
  const path: Node[] = [];
  let node: Node | undefined = root;
  while (node) {
    path.push(node);
    node = childNodes(node).find((c) => c.start <= offset && offset <= c.end);
  }
  return path;
}

/** Whether `node` binds `name` as a parameter or temporary (its own declaration). */
export function bindsName(node: Node, name: string): boolean {
  if (node.kind === NodeKind.Block || node.kind === NodeKind.MethodDefinition) {
    return node.parameters.some((p) => p.name === name) || node.temporaries.some((t) => t.name === name);
  }
  if (node.kind === NodeKind.Program) {
    return node.temporaries.some((t) => t.name === name);
  }
  return false;
}

/** The nearest enclosing scope in `path` that binds `name`, else the outermost (program). */
export function bindingScope(path: Node[], name: string): Node {
  for (let i = path.length - 1; i >= 0; i--) {
    const node = path[i] as Node;
    if (bindsName(node, name)) {
      return node;
    }
  }
  return path[0] as Node;
}

/** True if `name` is a parameter/temporary bound by some scope along `path` (a local, not an ivar/global). */
export function isLocallyBound(path: Node[], name: string): boolean {
  return path.some((n) => bindsName(n, name));
}

/** Occurrences of `name` within `scope`: its declaration(s) + reads + assignment-target writes,
 *  excluding any nested scope that re-binds the name (shadowing). */
export function variableOccurrences(scope: Node, name: string): Occurrence[] {
  const out: Occurrence[] = [];
  const seen = new Set<string>();
  const push = (r: Ranged, kind: OccurrenceKind): void => {
    const key = `${r.start}:${r.end}`;
    if (!seen.has(key)) {
      seen.add(key);
      out.push({ start: r.start, end: r.end, startPos: r.startPos, endPos: r.endPos, kind });
    }
  };

  // Declarations bound by the scope itself.
  if (scope.kind === NodeKind.Block || scope.kind === NodeKind.MethodDefinition) {
    for (const p of scope.parameters) {
      if (p.name === name) push(p, 'write');
    }
  }
  if (scope.kind === NodeKind.Block || scope.kind === NodeKind.MethodDefinition || scope.kind === NodeKind.Program) {
    for (const t of scope.temporaries) {
      if (t.name === name) push(t, 'write');
    }
  }

  const recurse = (node: Node): void => {
    // Stop at a nested scope that re-binds the name — those occurrences are a different variable.
    if (node !== scope && bindsName(node, name)) {
      return;
    }
    if (node.kind === NodeKind.Assignment && node.target.name === name) {
      push(node.target, 'write');
    } else if (node.kind === NodeKind.Variable && node.name === name) {
      push(node, 'read');
    }
    for (const child of childNodes(node)) {
      recurse(child);
    }
  };
  recurse(scope);
  return out;
}
