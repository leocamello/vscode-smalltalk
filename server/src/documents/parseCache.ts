// Version-keyed parse cache (US-412, extended in US-417).
//
// Parsing + symbol-table building is cheap, but the providers can be hit
// repeatedly for the same unchanged document, so memoize by (uri, version).
// One entry holds the AST, the full token stream (incl. comments), and the
// lazily-built symbol table. The TextDocuments manager bumps `version` on every
// edit, which naturally invalidates a stale entry.

import { buildSymbolTable, type SymbolNode } from '../parser/symbols';
import { parse } from '../parser/parser';
import { tokenize } from '../parser/lexer';
import type { ProgramNode } from '../parser/ast';
import type { Token } from '../parser/token';

interface TextLike {
  readonly uri: string;
  readonly version: number;
  getText(): string;
}

interface CacheEntry {
  readonly version: number;
  readonly ast: ProgramNode;
  readonly tokens: Token[];
  symbols?: SymbolNode[];
}

const cache = new Map<string, CacheEntry>();

function entryFor(doc: TextLike): CacheEntry {
  const hit = cache.get(doc.uri);
  if (hit && hit.version === doc.version) {
    return hit;
  }
  const text = doc.getText();
  const entry: CacheEntry = { version: doc.version, ast: parse(text).ast, tokens: tokenize(text).tokens };
  cache.set(doc.uri, entry);
  return entry;
}

/** The document's AST, parsed at most once per (uri, version). */
export function getAst(doc: TextLike): ProgramNode {
  return entryFor(doc).ast;
}

/** The document's full token stream (including comment trivia). */
export function getTokens(doc: TextLike): Token[] {
  return entryFor(doc).tokens;
}

/** The document's symbol table (built once, then cached on the entry). */
export function getSymbols(doc: TextLike): SymbolNode[] {
  const entry = entryFor(doc);
  if (!entry.symbols) {
    entry.symbols = buildSymbolTable(entry.ast);
  }
  return entry.symbols;
}

/** Drop a document's cached entry (e.g. on close/delete). */
export function invalidate(uri: string): void {
  cache.delete(uri);
}
