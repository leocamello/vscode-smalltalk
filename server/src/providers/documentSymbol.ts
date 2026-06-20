// Document-symbol provider mapping (US-412, slice A / AC1).
//
// Maps the US-411 symbol tree to LSP `DocumentSymbol`s for the Outline view and
// breadcrumbs. Pure — imports only `vscode-languageserver-types`, so it unit
// tests under tsx without the server runtime.

import { type DocumentSymbol, type Range, SymbolKind as LspSymbolKind } from 'vscode-languageserver-types';
import { SymbolKind, type SymbolNode, type SymbolRange } from '../parser/symbols';

const KIND_MAP: Record<SymbolKind, LspSymbolKind> = {
  [SymbolKind.Namespace]: LspSymbolKind.Namespace,
  [SymbolKind.Class]: LspSymbolKind.Class,
  [SymbolKind.Method]: LspSymbolKind.Method,
  [SymbolKind.InstanceVariable]: LspSymbolKind.Field,
  [SymbolKind.ClassVariable]: LspSymbolKind.Field,
  [SymbolKind.Temporary]: LspSymbolKind.Variable,
};

function toRange(r: SymbolRange): Range {
  return { start: r.startPos, end: r.endPos };
}

/** Map the symbol tree to LSP document symbols. Method-local temporaries and
 *  parameters are omitted — outlines list declarations, not locals. */
export function toDocumentSymbols(nodes: SymbolNode[]): DocumentSymbol[] {
  const out: DocumentSymbol[] = [];
  for (const node of nodes) {
    if (node.kind === SymbolKind.Temporary) {
      continue;
    }
    const symbol: DocumentSymbol = {
      name: node.name,
      kind: KIND_MAP[node.kind],
      range: toRange(node.range),
      selectionRange: toRange(node.selectionRange),
      children: toDocumentSymbols(node.children),
    };
    if (node.detail !== undefined) {
      symbol.detail = node.detail;
    }
    out.push(symbol);
  }
  return out;
}
