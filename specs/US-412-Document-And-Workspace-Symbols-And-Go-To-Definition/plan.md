# Implementation Plan: Document/Workspace Symbols + Go-To-Definition

**ID**: US-412 | **Date**: 2026-06-20 | **Spec**: ./spec.md | **Branch**: `feature/US-412-document-and-workspace-symbols-and-go-to-definition`

## Summary
Wire the US-411 front end (`parse` + `buildSymbolTable`) into three LSP providers — document symbols,
workspace symbols, go-to-definition — over a version-keyed parse cache and a lazy workspace index.
No new analysis; the work is the LSP boundary + indexing + tests. Delivered in **three slices** so
each is reviewable and demoable: **A** documentSymbol (outline), **B** workspace/symbol, **C**
definition + cache freshness. The §6 manual matrix (`verification.md`) is the 0.4.0 gate.

## Approach
Reuse existing patterns: the server is `vscode-languageserver/node` (US-410); the front end is pure
and already emits LSP-shaped `{line,character}` ranges. New files live under `server/src/`:

- **`documents/parseCache.ts`** — `getSymbols(uri, version, text) → SymbolNode[]`, memoized by
  `(uri, version)`; the cache entry is invalidated when the `TextDocuments` version changes. A small
  debounce window coalesces rapid edits (the providers read the latest committed version).
- **`providers/documentSymbol.ts`** — `toDocumentSymbols(SymbolNode[]) → DocumentSymbol[]`: map our
  `SymbolKind` → LSP `SymbolKind` (Class→Class, Method→Method, Instance/ClassVariable→Field,
  Namespace→Namespace, Temporary→Variable), `detail` through, `range`/`selectionRange` through
  (already positions), children recursed.
- **`providers/workspaceIndex.ts`** — enumerate `**/*.{st,gst}` in the workspace folders (Node `fs`,
  honoring `files.exclude` from client config), parse + flatten to a `{ name, kind, selector?,
  classSide?, location }[]` index; rebuild incrementally on `didChange`/`didCreate`/`didDelete`.
- **`providers/workspaceSymbol.ts`** — filter the index by the query (case-insensitive substring; LSP
  client does the fuzzy ranking) → `WorkspaceSymbol[]`.
- **`providers/definition.ts`** — resolve the identifier/selector under the cursor (re-tokenize the
  line or reuse the token stream) → class refs match index Class entries; selectors match all Method
  entries → `Location[]`, same-file first.
- **`server.ts`** — advertise `documentSymbolProvider`, `workspaceSymbolProvider`,
  `definitionProvider`; register the handlers; receive `workspaceFolders` + `files.exclude` on init.

**Testing layers** (fastest first):
1. **Unit (tsx)** — pure mappers/resolvers over fixture strings + `test-cases/*.st`. New
   `server/test/providers.test.ts`, run under `test:parser`'s runner (rename concept: it is the
   server unit runner).
2. **LSP server test** — extend the `handshake.test.mjs` pattern: spawn the **bundled** server, run
   `initialize` → `didOpen` → `documentSymbol`/`definition`/`workspaceSymbol`, assert real responses.
   Fast, real, no Electron; runs in CI as `test:server`.
3. **Integration (Electron)** — `@vscode/test-cli` config + a fixture workspace; assert
   `vscode.executeDocumentSymbolProvider` / `executeDefinitionProvider` end-to-end (DoD requirement).
4. **Manual** — the `verification.md` matrix (the release gate).

## Steps (slices)
**Slice A — documentSymbol / outline (AC1)** ← start here
1. `documents/parseCache.ts` + unit test.
2. `providers/documentSymbol.ts` (mapping) + unit test over brace + chunk fixtures.
3. Wire `onDocumentSymbol` into `server.ts`; advertise the capability.
4. Extend the LSP server test: `documentSymbol` request returns the class→methods hierarchy.
5. `check-types`/`lint`/tests green → PR.

**Slice B — workspace/symbol (AC2)**
6. `providers/workspaceIndex.ts` (scan + flatten, `files.exclude`, incremental updates).
7. `providers/workspaceSymbol.ts` (query filter) + `onWorkspaceSymbol`; advertise capability.
8. Unit + LSP server tests (multi-file fixture dir).

**Slice C — definition + freshness (AC3, AC4)**
9. `providers/definition.ts` (position → class/selector → `Location[]`) + `onDefinition`.
10. Cache-freshness on edit (debounce) + index invalidation; unit + LSP server tests.
11. **Electron integration harness** (`.vscode-test.mjs` + fixture workspace + first e2e); wire into CI.

**Release (0.4.0, after Slice C)**
12. Run the `verification.md` manual matrix; bump version + CHANGELOG; confirm `MARKETPLACE` PAT;
    cut `v0.4.0`.

## Dependencies & Risks
- **Dep**: US-411 (`parse`, `buildSymbolTable`) — merged. `vscode-languageserver` types (present).
- **Workspace scan cost / `files.exclude` plumbing** — get the exclude globs from the client config;
  guard very large trees; lazy + incremental. (Risk → Slice B.)
- **Definition over-matching** (dynamic dispatch returns all implementors) — by design; order
  same-file first; covered by manual M8.
- **Electron e2e flakiness/cost** — keep it a thin smoke; the LSP server test carries the bulk of
  automated end-to-end confidence; the manual matrix carries the rest.
- **Position fidelity** — `selectionRange` must sit on the name for clean go-to-def selection.

## Verification
Per slice: `check-types` + `lint` + unit (tsx) + the LSP server test green; AST/symbol behavior
unchanged (US-411 frozen). Story-level: Electron e2e + the **manual matrix** in `verification.md`
(the 0.4.0 gate), then the release cut.
