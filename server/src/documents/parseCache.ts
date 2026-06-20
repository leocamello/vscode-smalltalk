// Version-keyed symbol cache (US-412, slice A).
//
// Parsing + symbol-table building is cheap (the whole GST kernel parses in a
// blink), but the providers can be hit repeatedly for the same unchanged
// document, so memoize by (uri, version). The TextDocuments manager bumps
// `version` on every edit, which naturally invalidates a stale entry.

import { buildSymbolTable, type SymbolNode } from '../parser/symbols';
import { parse } from '../parser/parser';

interface TextLike {
  readonly uri: string;
  readonly version: number;
  getText(): string;
}

interface CacheEntry {
  readonly version: number;
  readonly symbols: SymbolNode[];
}

const cache = new Map<string, CacheEntry>();

/** The document's symbol table, parsed at most once per (uri, version). */
export function getSymbols(doc: TextLike): SymbolNode[] {
  const hit = cache.get(doc.uri);
  if (hit && hit.version === doc.version) {
    return hit.symbols;
  }
  const symbols = buildSymbolTable(parse(doc.getText()).ast);
  cache.set(doc.uri, { version: doc.version, symbols });
  return symbols;
}

/** Drop a document's cached entry (e.g. on close/delete). */
export function invalidate(uri: string): void {
  cache.delete(uri);
}
