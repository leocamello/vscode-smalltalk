# Tasks: Formatting

**ID**: US-416 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Map tasks to acceptance criteria where possible.

## Phase 1 — Spec & Setup
- [x] T001 Spec reviewed; `requirements-validation.md` gate passed (incl. §3.5 AC routing).
- [x] T002 ADR-0005 (formatter = whitespace-only token-stream rewriter; AC2 "reprint" reinterpretation) authored + linked from spec/plan.

## Phase 2 — Acceptance Harness (TDD e2e — write tests BEFORE code)
- [x] T005 Route each AC to its layer per `requirements-validation.md` §3.5 (done in gate; reconfirm).
- [x] T006a Write RED property test `server/test/format.property.test.ts`: idempotence `format(format(x))===format(x)` + significant-token-stream invariance, over kernel corpus + fuzzed inputs (AC3). Registered in `run.ts`.
- [x] T006b Write RED output-eval `evals/datasets/formatting/` (`cases.json` + `run.ts`): spacing, indent, cascade-align, keyword-wrap, blank-line collapse, comment/blank preservation, malformed→unchanged (AC2/AC5). Wired `eval:formatting` into `npm run eval`.
- [x] T006c Write RED handshake assertions in `server/test/handshake.test.mjs`: 3 formatting capabilities advertised + on-type trigger char `]` (AC4).
- [x] T006d Write RED e2e in `client/test-e2e/US-416.acceptance.test.js`: Format Document changes buffer when enabled (AC2) + comment/blank survive + no-op when disabled (AC4); range touches only selection (AC1); on-type dedents on `]` (AC1).
- [x] T007 Confirmed RED for the right reason: `test:parser` + `eval:formatting` → `MODULE_NOT_FOUND` formatter.ts; `test:server` → `documentFormattingProvider` undefined (handshake.test.mjs:129). e2e RED by construction (enabled suite polls for edits a missing provider never emits).

## Phase 3 — Implementation
- [x] T010 Core `server/src/format/formatter.ts`: depth model + `(prevKind,nextKind)` spacing table + blank-line collapse → green spacing/indent goldens + idempotence (AC2/AC3). Tight-bracket spacing matches GST kernel style; `Scope` token (`A.B`/`A::B`) kept tight to preserve the token stream.
- [x] T011 Cascade alignment (`cascades: align|preserve`) + keyword-message wrapping (`keywordWrap` columns) via AST-derived forced-break map (AC1/AC2).
- [x] T012 Skip-on-error: any parse diagnostic → return input unchanged (whole-file skip, v1) (AC5).
- [x] T013 Provider `server/src/providers/formatting.ts`: `formatDocument`/`formatRange`/`formatOnType`, `TextEdit[]`; honor `FormattingOptions` (insertSpaces) + `indentSize`; VS Code minimizes the whole-doc replace into small diffs.
- [x] T014 Wired capabilities in `server.ts` (`documentFormattingProvider`, `documentRangeFormattingProvider`, `documentOnTypeFormattingProvider` first `]` + more `\n`,`;`).
- [x] T015 Config in `package.json` `smalltalk.format.*` (`enable` default **false**, `indentSize` 4, `cascades` `align`, `keywordWrap` 100); pulled per request (pull model); disabled → `[]` (AC4).
- [x] T016 `blockStyle: preserve|expand` (default `preserve`) — opt-in expanded aesthetic via AST-derived structural forced-breaks (method/class/multi-statement-block bodies one-statement-per-line; single-statement arg blocks stay inline; inter-method blanks preserved). Still whitespace-only; idempotent + token-invariant over the kernel corpus in both modes. Wired through `FormatOptions`/`FormatSettings`/config; 5 new eval goldens; property test covers both modes; e2e `blockStyle expand` test. Realizes the "safe pretty-printer look" noted in ADR-0005.

## Phase 4 — Verify
- [x] T900 All acceptance tests GREEN: `test:parser` (incl. format property 26 + 122/122 kernel files idempotent & token-invariant), `test:server` (handshake), `eval` (incl. formatting 17), `test:e2e` (US-416 5/5). `check-types` + `lint` clean.
- [~] T901 Created `specs/US-416-Formatting/manual-qa-workspace/` (README matrix A–D + `Messy.st` valid/0-diag fixture + `Broken.st` malformed fixture) and filled `verification.md`. **Extension-Host matrix run still pending** (verification.md §4).
- [ ] T902 CI green on Linux/macOS/Windows + e2e job (after PR).
- [ ] T903 Doc-rot sweep staged for release: user-stories US-416 → Done, EPIC-004, ROADMAP, README features, CLAUDE.md, CHANGELOG, version bump.
