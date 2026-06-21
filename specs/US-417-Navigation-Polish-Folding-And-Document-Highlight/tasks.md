# Tasks: Navigation Polish — Folding + Document Highlight

**ID**: US-417 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Two slices: **A** foldingRange → **B** documentHighlight.

## Phase 1 — Spec & Setup
- [x] T001 Spec authored; `requirements-validation.md` gate passed (PASS).
- [x] T002 `plan.md` + `tasks.md` written.
- [x] T003 Pre-req fix: retired shields.io Marketplace badges replaced (PR #44, merged).

## Phase 2 — Slice A: foldingRange (AC1)
- [x] T010 `documents/parseCache.ts` — combined `(uri, version)` entry exposing `getAst`/`getTokens`/`getSymbols`; shared AST walker extracted to `parser/walk.ts`.
- [x] T011 `providers/foldingRange.ts` — `toFoldingRanges(ast, tokens)`: Definition/MethodDefinition/Block (multi-line) + multi-line `Comment` tokens; `endLine` = construct's last line.
- [x] T012 Wire `onFoldingRanges` in `server.ts`; advertise `foldingRangeProvider`.
- [x] T013 Unit tests (class/method/block ranges, multi-line comment, single-line skipped, exact line numbers).
- [x] T014 Real-server LSP test (`foldingRange`) + Electron e2e row (`executeFoldingRangeProvider`).
- [x] T015 `check-types`/`lint`/`test:parser`/`test:server`/`test:e2e` + package smoke green; open Slice A PR.

## Phase 3 — Slice B: documentHighlight (AC2)
- [x] T020 No AST change needed: selector token ranges are **derived in the provider** from the receiver/arg gaps + token stream (`tokenIn`), so nested sends aren't included and the parser/snapshots are untouched. `definition.ts` exposes `resolveQueryInAst` (no re-parse).
- [x] T021 `providers/documentHighlight.ts` — selector match (all sends, each selector part) + scope-aware variable (`pathToOffset` + `bindingScope`: nearest block/method binding, else file-wide); writes for declarations/assignment targets. Shared `childNodes` added to `walk.ts`.
- [x] T022 Wire `onDocumentHighlight`; advertise `documentHighlightProvider`.
- [x] T023 Unit tests (unary + keyword selector occurrences; scoped variable; no cross-scope bleed; decl + assignment writes) + real-server LSP + Electron e2e (`executeDocumentHighlights`).
- [x] T024 `check-types`/`lint`/`test:parser`/`test:server`/`test:e2e` (7/7) + package smoke green; open Slice B PR.

## Phase 4 — Verify & Release (0.4.1)
- [ ] T900 Automated green (unit + LSP server + e2e); `npm run eval` unaffected.
- [ ] T901 Manual spot-check in the Extension Host (fold class/method/block/comment; highlight a selector and a scoped variable).
- [ ] T902 CI green on Linux/macOS/Windows.
- [ ] T903 Bump version (0.4.1) + CHANGELOG; cut `v0.4.1` (CI publishes).
