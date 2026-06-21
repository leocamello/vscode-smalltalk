// Folding-range provider (US-417, slice A / AC1).
//
// Semantic folds from the US-411 AST + token stream: class/namespace bodies,
// method bodies, blocks, and multi-line comments. Pure
// (vscode-languageserver-types only); reuses the shared AST walker.

import { type FoldingRange, FoldingRangeKind } from 'vscode-languageserver-types';
import { NodeKind, type Node } from '../parser/ast';
import { TokenKind, type Token } from '../parser/token';
import { visit } from '../parser/walk';

const FOLDABLE = new Set<NodeKind>([NodeKind.Definition, NodeKind.MethodDefinition, NodeKind.Block]);

/** Fold ranges for definitions, method bodies, blocks, and multi-line comments.
 *  Only constructs spanning >= 2 lines fold. `endLine` is the construct's last
 *  line; the editor keeps the start line visible. */
export function toFoldingRanges(ast: Node, tokens: Token[]): FoldingRange[] {
  const ranges: FoldingRange[] = [];

  visit(ast, (node) => {
    if (FOLDABLE.has(node.kind) && node.startPos.line < node.endPos.line) {
      ranges.push({ startLine: node.startPos.line, endLine: node.endPos.line });
    }
  });

  for (const token of tokens) {
    if (token.kind === TokenKind.Comment && token.startPos.line < token.endPos.line) {
      ranges.push({ startLine: token.startPos.line, endLine: token.endPos.line, kind: FoldingRangeKind.Comment });
    }
  }

  return ranges;
}
