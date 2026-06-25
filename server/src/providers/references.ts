// References + plural go-to-definition over the two-tier engine (US-423, Slice B).
//
// Thin, pure adapter: maps the merged `ResolvedRef[]` (xref/resolve.ts) onto LSP
// `Location[]` / `LocationLink[]`. The selector under the cursor is resolved by
// the existing US-412 query (`resolveQueryInAst`); server.ts assembles the
// `XrefSources` from the live index + workspace xref + the cartridge Console.
//
// AC1: `references` = the de-duplicated union (implementors ∪ senders).
// AC3: go-to-definition on a send returns ALL implementors — plural, never one.

import { type Location, type LocationLink, type Range } from 'vscode-languageserver-types';
import { resolveImplementors, resolveReferences, type ResolvedRef, type XrefSources } from '../xref/resolve';

function toRange(ref: ResolvedRef): Range {
  return { start: ref.range.start, end: ref.range.end };
}

export function toLocation(ref: ResolvedRef): Location {
  return { uri: ref.uri, range: toRange(ref) };
}

/** `textDocument/references`: the flat union of every send-site + definition. */
export function referenceLocations(sources: XrefSources, selector: string): Location[] {
  return resolveReferences(sources, selector).map(toLocation);
}

/** Plural go-to-definition (AC3) for a client without `linkSupport`: every
 *  implementor as a plain `Location`, still plural — never collapsed to one. */
export function definitionLocations(sources: XrefSources, selector: string): Location[] {
  return resolveImplementors(sources, selector).map(toLocation);
}

/** Plural go-to-definition (AC3): every implementor as a `LocationLink`, so VS
 *  Code shows a picker rather than collapsing to a single target. */
export function definitionLinks(sources: XrefSources, selector: string): LocationLink[] {
  return resolveImplementors(sources, selector).map((ref) => {
    const range = toRange(ref);
    return { targetUri: ref.uri, targetRange: range, targetSelectionRange: range };
  });
}
