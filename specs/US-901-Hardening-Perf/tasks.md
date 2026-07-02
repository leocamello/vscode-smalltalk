# Tasks: Hardening & Perf

**ID**: US-901 | **Spec**: ./spec.md | **Plan**: ./plan.md | **Issue**: #112

Mark each task `[x]` as it lands. Map tasks to acceptance criteria where possible.

## Phase 1 — Spec & Setup
- [x] T001 Spec reviewed; `requirements-validation.md` gate passed (incl. §3.5 AC routing).

## Phase 2 — Acceptance Harness (TDD — write tests BEFORE code)
- [x] T005 Route each AC to its layer (§3.5) — perf → bench; robustness → fuzz unit; telemetry → guard; bug-bash → per-fix.
- [x] T006 Remove the e2e stub `client/test-e2e/US-901.acceptance.test.js` (no user-observable surface, §3.5).
- [x] T007 RED: `server/test/robustness.test.ts` fuzz over `tokenize`/`parse` (never-throw + size ceiling) — AC4/AC5. **Found the stack-overflow defect (RED).**
- [x] T008 RED: no-telemetry guard test (denylist absent from `server/src`+`client/src`) — AC3.
- [x] T009 RED: `scripts/gen-corpus.ts` + `scripts/bench.ts`; captured **baseline** index/completion/rename numbers (measure-first) — AC1/AC2/AC5.

## Phase 3 — Implementation
- [x] T010 Fixed the never-throw defect: `MAX_EXPRESSION_DEPTH` guard in `parser.ts` (GREEN AC4/AC5).
- [x] T011 Budgets met with 20–23× headroom; `allWorkspaceFiles` **not** optimized (measured non-bottleneck) — AC1/AC2.
- [x] T012 Wired `npm run bench` + `gen-corpus`; documented no-telemetry (README/CHANGELOG) — AC3.
- [x] T013 Bug-bash provider matrix (`providerRobustness.test.ts`, 735 checks) → triage: 1 found / 1 fixed / 0 deferred — AC6.
- [x] T014 Defect pinned red-first by `robustness.test.ts`, then fixed — AC6.

## Phase 4 — Verify
- [x] T900 Bench budgets PASS; fuzz + guards green; `test:parser`/`test:server`/`test:client`/`eval` green.
- [x] T901 Recorded bench numbers + bug-bash triage in `verification.md`; gate passed.
- [ ] T902 CI green on Linux/macOS/Windows + e2e (on push/PR).
- [ ] T903 Doc-rot sweep (CLAUDE/README/ROADMAP/epics/user-stories + dates) inline with release.
- [ ] T904 Manual-QA matrix run in the Extension Host (owner) before release.
