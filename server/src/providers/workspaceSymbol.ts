// workspace/symbol provider mapping (US-412, slice B / AC2).
// Maps workspace index entries to LSP WorkspaceSymbols. Pure
// (vscode-languageserver-types only).

import { SymbolKind as LspSymbolKind, type WorkspaceSymbol } from 'vscode-languageserver-types';
import { SymbolKind } from '../parser/symbols';
import type { IndexEntry } from './workspaceIndex';

const KIND_MAP: Partial<Record<SymbolKind, LspSymbolKind>> = {
  [SymbolKind.Namespace]: LspSymbolKind.Namespace,
  [SymbolKind.Class]: LspSymbolKind.Class,
  [SymbolKind.Method]: LspSymbolKind.Method,
};

export function toWorkspaceSymbols(entries: IndexEntry[]): WorkspaceSymbol[] {
  const out: WorkspaceSymbol[] = [];
  for (const entry of entries) {
    const kind = KIND_MAP[entry.kind];
    if (kind === undefined) {
      continue;
    }
    const symbol: WorkspaceSymbol = {
      name: entry.name,
      kind,
      location: { uri: entry.uri, range: entry.selectionRange },
    };
    if (entry.containerName !== undefined) {
      symbol.containerName = entry.containerName;
    }
    out.push(symbol);
  }
  return out;
}
