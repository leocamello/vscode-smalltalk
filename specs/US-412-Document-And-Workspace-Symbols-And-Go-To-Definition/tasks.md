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
- [x] T020 `server/src/providers/workspaceIndex.ts` — scan `**/*.{st,gst}` (sync fs walk, `MAX_FILES` guard), parse + flatten classes/namespaces/methods to located entries; `defaultExclude` + `excludeFromConfig(files.exclude)`; per-document refresh via `setFile`.
- [x] T021 `server/src/providers/workspaceSymbol.ts` — substring query → `WorkspaceSymbol[]` (kind/location/containerName); `onWorkspaceSymbol` + `workspaceSymbolProvider` advertised; index built on `initialized` from `workspaceFolders` (+ `files.exclude` via configuration); kept fresh on `didChangeContent`, reverted from disk on `didClose`.
- [x] T022 Unit tests (index query, real-folder scan over `test-cases/`, mapping, exclude predicates) + LSP server test driving `workspace/symbol` against the indexed fixture folder (answers `workspace/configuration`).
- [x] T023 `check-types`/`lint`/`test:parser`/`test:server` green + package smoke; open Slice B PR.

## Phase 4 — Slice C: definition + freshness (AC3, AC4)
- [x] T030 `server/src/providers/definition.ts` — AST walk resolves the cursor to a class ref or a message selector; `findDefinitions` returns class defs / **all** selector implementors as `Location[]` (same-file first); `onDefinition` + `definitionProvider` advertised.
- [x] T031 Debounced (250 ms) workspace-index refresh on `didChangeContent`; outline stays immediate via the version-keyed cache; index reverted from disk on close.
- [x] T032 Electron e2e harness: `.vscode-test.mjs` + `client/test-e2e/` fixture workspace + `navigation.test.js` (executes documentSymbol/workspaceSymbol/definition providers in a real VS Code). **Ran locally — 3/3 passing.** `test:e2e` script added; not wired into the default CI job (downloads ~256 MB VS Code + needs a display) — runs locally + is the basis of the manual matrix.
- [x] T033 `check-types`/`lint`/`test:parser`/`test:server` + e2e green; package smoke clean; open Slice C PR.

## Phase 5 — Verify & Release (0.4.0)
- [x] T900 Automated: unit + LSP server + Electron e2e green; `npm run eval` (grammar) unaffected.
- [x] T901 **Manual matrix in `verification.md` executed and passed** — M1–M14, real kernel corpus, clean-install VSIX. Two bugs found & fixed (#39, #40). *(release gate cleared)*
- [x] T902 CI green on Linux/macOS/Windows (all slice + fix PRs).
- [~] T903 Bump `version` (0.4.0) + 0.4.0 CHANGELOG done in the release PR; `MARKETPLACE` PAT current (0.3.0 published 2026-06-19). **Cut the `v0.4.0` GitHub Release** → CI publishes. *(tag = owner's one-click step)*
