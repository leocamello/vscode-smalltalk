# Tasks: {{TITLE}}

**ID**: {{US_ID}} | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Map tasks to acceptance criteria where possible.

## Phase 1 — Spec & Setup
- [ ] T001 Spec reviewed; `requirements-validation.md` gate passed (incl. §3.5 AC routing).

## Phase 2 — Acceptance Harness (TDD e2e — write tests BEFORE code)
- [ ] T005 Route each AC to its layer (user-observable → e2e; contract/data → unit or `evals/`; protocol → handshake) per `requirements-validation.md` §3.5.
- [ ] T006 Write the failing acceptance test(s): user-observable ACs → `client/test-e2e/{{US_ID}}.acceptance.test.js` (else route to unit/eval and delete the stub).
- [ ] T007 Confirm the new asserts are RED for the right reason (`npm run test:e2e` / unit) — red phase proven before any implementation.

## Phase 3 — Implementation
- [ ] T010 Implement to green.

## Phase 4 — Verify
- [ ] T900 Acceptance tests now GREEN; Output eval / unit tests pass (`npm run eval`, `npm run test:e2e`).
- [ ] T901 `verification.md` gate passed.
- [ ] T902 CI green on Linux/macOS/Windows + e2e.
