# Tasks: Hover

**ID**: US-415 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Map tasks to acceptance criteria where possible.

## Phase 1 — Spec & Setup
- [x] T001 Spec reviewed; `requirements-validation.md` gate passed (incl. §3.5 AC routing).
- [x] T002 Clarify gate resolved: provenance gates prose (spec §4a); memory recorded.

## Phase 2 — Acceptance Harness (TDD e2e — write tests BEFORE code)
- [x] T005 Route each AC to its layer per `requirements-validation.md` §3.5.
- [x] T006 Write failing e2e asserts in `client/test-e2e/US-415.acceptance.test.js` — one per AC
  (AC1 selector, AC2 class chain, AC3 variable, AC4 literal, AC5 Markdown) + workspace prose.
- [x] T007 Add failing `server/test/hover.test.ts` unit asserts (content per kind) + extend the
  handshake test for `textDocument/hover`.
- [x] T008 Confirmed RED for the right reason (module-not-found / empty hover) before code.

## Phase 3 — Implementation
### Slice A — provider + facts + workspace prose
- [x] T010 `IndexEntry.superclass` (+`comment`) from `SymbolNode.detail` / `extractComments`. (AC2)
- [x] T011 `KernelIndexService`: `superclassOf` / `implementorsOf` / `hasClass` / `activeProvenance`. (AC1/AC2)
- [x] T012 `server/src/providers/hover.ts`: cursor classification + per-kind Markdown renderers
  (selector AC1, class AC2, variable AC3, literal AC4, fences AC5); chain stops at the `nil` root.
- [x] T013 Wire `server.ts`: advertise `hoverProvider: true` + `connection.onHover`.
### Slice B — installed-cartridge + workspace prose
- [x] T014 `extractComments` (`server/src/parser/comments.ts`) + capture into
  `indexKernelDirectoryToCartridge` → `documentation`, gated on `header.carriesProse` (installed=true).
- [x] T015 `classComment` + comment-bearing `implementorsOf` on the service (prose only when
  `carriesProse`); `WorkspaceIndex` attaches comments; surfaced in hover. (AC1/AC2 comment clauses)
- [x] T016 Bundled stays facts-only (existing `cartridgeLoader`/`kernelIndex` tests) + installed
  adapter carries prose (new indexer + service tests); workspace index comment test.

## Phase 4 — Verify
- [x] T020 Added `evals/datasets/hover/{cases.json,run.ts}`; wired into `npm run eval` (10/10).
- [x] T900 Acceptance tests GREEN; unit + eval pass (`test:parser` 11 hover + 6 comments, `eval` 10/10,
  `test:server` hover-over-wire, `test:e2e` 21 passing). Installed-prose path verified on the dev box.
- [x] T901 `verification.md` signed off — **manual-QA matrix CLEARED (owner, 2026-06-24)**; PO accepted.
- [ ] T902 CI green on Linux/macOS/Windows + e2e — pending commit/push.
