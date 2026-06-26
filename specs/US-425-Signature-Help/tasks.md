# Tasks: Signature Help

**ID**: US-425 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Map tasks to acceptance criteria where possible.

## Phase 1 — Spec & Setup
- [x] T001 Spec reviewed; `requirements-validation.md` gate passed (incl. §3.5 AC routing).

## Phase 2 — Acceptance Harness (TDD e2e — write tests BEFORE code)
- [x] T005 Route each AC to its layer (AC1 → e2e + unit + eval + handshake; AC2 → handshake/unit) per `requirements-validation.md` §3.5.
- [x] T006 Write the failing tests: `client/test-e2e/US-425.acceptance.test.js` (executeSignatureHelpProvider → active param), `server/test/signatureHelp.test.ts` (cursor analysis + matching + null cases), handshake capability ad + request, `evals/datasets/signature-help/`.
- [x] T007 Confirm the new asserts are RED for the right reason (unit `MODULE_NOT_FOUND` on the missing provider) — red phase proven before implementation.

## Phase 3 — Implementation
- [x] T010 Implement `server/src/providers/signatureHelp.ts` → unit (16) + eval (10) green (AC1, AC2).
- [x] T011 Wire `server/src/server.ts` (advertise `signatureHelpProvider` + `onSignatureHelp`) → handshake + e2e (29) green.

## Phase 4 — Verify
- [x] T900 Acceptance tests GREEN; `test:parser` / `test:server` / `test:e2e` / `npm run eval` all pass.
- [x] T901 `verification.md` §4 gate PASSED — `manual-qa-workspace/` run hands-on in the Extension Host (owner, 2026-06-26). Fixture parses with 0 diagnostics (verified).
- [ ] T902 CI green on Linux/macOS/Windows + e2e (after push/PR).
