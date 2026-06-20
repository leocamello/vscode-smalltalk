# Tasks: Document/Workspace Symbols + Go-To-Definition

**ID**: US-412 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Map tasks to acceptance criteria where possible.
Delivered in three slices (each its own PR): **A** documentSymbol → **B** workspace/symbol →
**C** definition + freshness + Electron e2e. The `verification.md` manual matrix gates 0.4.0.

## Phase 1 — Spec & Setup
- [x] T001 Spec authored; `requirements-validation.md` gate passed (PASS).
- [x] T002 `plan.md` + `tasks.md` written; manual-QA matrix in `verification.md`.

## Phase 2 — Slice A: documentSymbol / outline (AC1) — start here
- [x] T010 `server/src/documents/parseCache.ts` — `getSymbols(doc)` memoized by `(uri, version)`; `invalidate(uri)` on close.
- [x] T011 `server/src/providers/documentSymbol.ts` — `toDocumentSymbols(SymbolNode[])`: SymbolKind→LSP map; detail/ranges/children through; method-local temps/params omitted from the outline.
- [x] T012 Wire `onDocumentSymbol` in `server.ts`; advertise `documentSymbolProvider`; keep incremental sync; drop cache on close.
- [x] T013 `server/test/providers.test.ts` (tsx) — mapping unit tests over brace + chunk + namespace fixtures (hierarchy, kinds, selectionRange ⊆ range, temps dropped).
- [x] T014 Extended the LSP server test (`handshake.test.mjs`): `initialize`→`didOpen`→`documentSymbol` returns the class→field/method hierarchy from the bundled server, and asserts the advertised capability.
- [x] T015 `check-types`/`lint`/`test:parser`/`test:server` green + `npm run package` smoke; open Slice A PR (links the story).

## Phase 3 — Slice B: workspace/symbol (AC2)
- [ ] T020 `server/src/providers/workspaceIndex.ts` — scan `**/*.{st,gst}` in workspace folders, parse + flatten; honor `files.exclude`; incremental on `didChange`/`didCreate`/`didDelete`.
- [ ] T021 `server/src/providers/workspaceSymbol.ts` — query filter → `WorkspaceSymbol[]`; `onWorkspaceSymbol`; advertise capability.
- [ ] T022 Unit + LSP server tests over a multi-file fixture dir; `files.exclude` respected.
- [ ] T023 Slice B PR.

## Phase 4 — Slice C: definition + freshness (AC3, AC4)
- [ ] T030 `server/src/providers/definition.ts` — position → identifier/selector → class defs / all implementors → `Location[]` (same-file first); `onDefinition`; advertise capability.
- [ ] T031 Cache freshness on edit (debounce ~250–300 ms) + index invalidation; tests.
- [ ] T032 Electron integration harness: `.vscode-test.mjs` + fixture workspace + first e2e (`executeDocumentSymbolProvider`/`executeDefinitionProvider`); wire into CI.
- [ ] T033 Slice C PR.

## Phase 5 — Verify & Release (0.4.0)
- [ ] T900 Automated: unit + LSP server + Electron e2e green; `npm run eval` (grammar) unaffected.
- [ ] T901 **Manual matrix in `verification.md` executed and passed** (real-corpus workspace, outline, search, go-to-def, live editing, robustness, no-`gst`, cross-platform, clean-install VSIX). *(release gate)*
- [ ] T902 CI green on Linux/macOS/Windows.
- [ ] T903 Bump `version` + CHANGELOG; confirm `MARKETPLACE` PAT valid; cut `v0.4.0` (CI publishes).
