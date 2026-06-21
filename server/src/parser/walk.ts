// Generic AST traversal (US-417). Visits every Node-valued property/array,
// so providers don't enumerate node kinds. Pure.

import type { Node } from './ast';

export function isNode(value: unknown): value is Node {
  return (
    typeof value === 'object' &&
    value !== null &&
    typeof (value as { kind?: unknown }).kind === 'string' &&
    typeof (value as { start?: unknown }).start === 'number'
  );
}

/** Pre-order visit of `node` and every descendant Node. */
export function visit(node: Node, fn: (n: Node) => void): void {
  fn(node);
  for (const child of childNodes(node)) {
    visit(child, fn);
  }
}

/** The immediate child Nodes of `node` (any Node-valued property or array element). */
export function childNodes(node: Node): Node[] {
  const out: Node[] = [];
  for (const key of Object.keys(node)) {
    const value = (node as unknown as Record<string, unknown>)[key];
    if (Array.isArray(value)) {
      for (const item of value) {
        if (isNode(item)) {
          out.push(item);
        }
      }
    } else if (isNode(value)) {
      out.push(value);
    }
  }
  return out;
}
