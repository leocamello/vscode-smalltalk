# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding
**Type**: Implementation Verification
**Story**: US-426 — Scope-aware Rename

---

## Section 1: Acceptance Criteria
- [x] All ACs in `tasks.md` are checked (Phases 1–3 complete).
- [x] Each AC has a passing test:
  - **AC1** (prepareRename accept/reject) → unit `server/test/rename.test.ts` (8-case matrix) **+** e2e
    (selector rejected) **+** handshake (`renameProvider.prepareProvider` advertised) — **green**.
  - **AC2** (temp/arg — exact scope, no bleed) → unit **+** property `server/test/rename.property.test.ts`
    (edited ranges cover only same-name occurrences) **+** e2e (temp rename in-buffer) — **green**.
  - **AC3** (ivar — workspace-wide, shadow-skip) → unit (split class + shadow) **+** output eval
    `evals/datasets/rename/` (multi-file + shadowing goldens) — **green**.
  - **AC4** (new-name validation: identifier + collision) → unit **+** eval **+** e2e (collision refused) —
    **green**.
  - **AC5** (no `gst`; no kernel edit; idempotent) → property (round-trip a→b→a; renamed source parses
    clean) **+** the pure resolver only edits supplied files — **green**.
- [x] Each user-observable AC's acceptance test was **red before** implementation (Phase 2 proof:
  `MODULE_NOT_FOUND` rename.ts for unit/eval/property; `renameProvider undefined` for handshake;
  e2e temp-rename red by construction). Recorded in `tasks.md` T007.

## Section 2: Code Quality
- [x] `npm run lint` passes; `npm run check-types` passes (strict, `noUncheckedIndexedAccess`).
- [x] `npm run test:parser` (rename unit 13 + property 9; documentHighlight regression 24 still green after the
  `parser/scope.ts` extraction), `npm run test:server` (handshake), `npm run eval` (rename 6/6),
  `npm run test:e2e` (US-426 3/3; whole suite 38/38) all pass locally.
- [x] No unjustified `any`. The provider + resolver are pure (`vscode-languageserver-types` only); the server
  injects file access.
- [x] JSDoc/header comments on the public surface (`prepareRenameAt`, `renameAt`, `enclosingClassNameAt`,
  `ivarOccurrences`, `parser/scope.ts` helpers).

## Section 3: Constitutional Compliance
- [x] **Native**: standard `textDocument/rename` + `prepareRename` (F2 / Rename Symbol + refactor preview).
- [x] **Zero Config**: no settings; safety is structural (scope resolution), works out of the box, no `gst`.
- [x] **Robustness**: front end never throws; unsafe/unresolvable cases are typed rejections surfaced via
  `ResponseError`; collisions/invalid names refused (no partial edit); kernel/cartridge never edited.
- [x] **TDD**: acceptance harness written and proven red before code (Phase 2).
- [x] **Dialect-agnostic**: resolution is over the US-411 symbol/scope model, not GST-lexeme-specific.

## Section 4: Manual Verification
- [ ] Feature works in Extension Host — run `specs/US-426-Scope-Aware-Rename/manual-qa-workspace/` matrix
  (Parts A–D): temp/arg rename scope-bounded; **workspace-wide ivar rename across `Account.st` +
  `Account-Report.st`**; the shadowing `Account-Shadow.st` left untouched; selector/class/pseudo-var
  rejected with a message; collision refused; all files still parse 0-diag; round-trip clean. *(To run
  before release, with real code and a clean VSIX.)*
- [ ] No errors in Developer Tools console during the matrix.

## Section 5: Sign-Off
- [ ] Ready for Merge — pending the Part A–D Extension-Host pass + PO acceptance + green CI on
  Linux/macOS/Windows + e2e.

---

**Automated-layer status (2026-06-30):** all green — `test:parser` (rename unit 13 + property 9; full suite
incl. documentHighlight 24), `test:server` (handshake: `renameProvider.prepareProvider`), `eval`
(rename 6/6), `test:e2e` (US-426 3/3; suite 38/38). Also fixed a pre-existing US-416 e2e test-isolation leak
(the `blockStyle` suite left `format.enable` persisted in the test host's Global settings).
**Remaining for Done:** manual-QA matrix in the Extension Host (§4), doc-rot sweep + version bump + CHANGELOG
(`tasks.md` T903), PO sign-off, CI green on all three OSes.
