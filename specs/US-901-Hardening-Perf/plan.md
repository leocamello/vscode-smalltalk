# Implementation Plan: Hardening & Perf

**ID**: US-901 | **Date**: 2026-07-02 | **Spec**: ./spec.md | **Branch**: `feature/US-901-hardening-perf` | **Issue**: #112

## Summary
Add a local perf benchmark (`npm run bench`) over a generated synthetic corpus that asserts the index
(<5 s / 1k files) and completion (p95 <100 ms) budgets; harden the parser/front-end against fuzzed &
pathological input (never-throw + bounded); add a static no-telemetry guard; and run a triaged
provider bug-bash. Optimize the class-rename `allWorkspaceFiles` scan **only if measured** to matter.

## Approach
Drive the existing **pure** server functions directly from `tsx` scripts (no Electron/LSP), mirroring
the eval harness pattern:
- `scripts/gen-corpus.ts` — seeded parametric generator (`--files N --out DIR`) writing `.st` files
  with superclass chains, ivars, keyword/unary/binary sends, and cross-file class refs.
- `scripts/bench.ts` — generate (or point at) a corpus, then time: (a) cold index via `walkStFiles` →
  `WorkspaceIndex.setFile` + `workspaceXref.setFile` (mirrors `scanWorkspace`); (b) `completionsAt`
  over a request sample (p50/p95/max); (c) `allWorkspaceFiles` + `classOccurrences` rename scan.
  Prints `metric … measured (budget) PASS/FAIL`; exits non-zero on any FAIL.
- `server/test/robustness.test.ts` — fuzz `tokenize`/`parse` (unterminated/unbalanced/huge/random),
  assert no throw + a size ceiling; wire into `run.ts` (`npm run test:parser`).
- No-telemetry guard — a test asserting the denylist (`http`, `https`, `fetch`, `net`,
  `TelemetryReporter`, `analytics`) is absent from `server/src` + `client/src` (and/or built `dist`).
- Perf suspect: if `allWorkspaceFiles` disk re-reads dominate, add `WorkspaceIndex.getText(uri)` /
  `entriesWithText()` and read index-held text (open-doc-wins preserved); guarded by `classRename` tests.
- Bug-bash: run the provider×edge matrix, triage, fix-now items get a red-first regression test.

## Steps
1. Route ACs (done in requirements-validation §3.5); delete the e2e stub.
2. RED: write `robustness.test.ts` (fuzz) + the no-telemetry guard; author `bench.ts`/`gen-corpus.ts`
   and capture the **baseline** numbers (this is the "measure first" step for the scan).
3. GREEN: fix any never-throw/bounded defect the fuzz test finds; make budgets pass (optimize the scan
   only if the baseline shows it's needed).
4. Bug-bash matrix → triage → red-first regression test + fix per accepted defect.
5. Docs: `package.json` `bench` script; README/CHANGELOG no-telemetry stance; record numbers in
   `verification.md`; doc-rot sweep at release.

## Dependencies & Risks
- Reuses US-412 index (`workspaceIndex.ts`), US-413 completion (`completionsAt`), US-411 parser
  (`tokenize`/`parse`), US-428 rename scan (`allWorkspaceFiles`/`classOccurrences`).
- Risk: bench timing variance → local/release signal only, not a CI gate. Risk: scan optimization must
  preserve open-doc-wins + skip-unreadable semantics (classRename tests guard it).

## Verification
- **Acceptance harness (TDD e2e):** US-901 has **no user-observable surface** (§3.5) — ACs route to the
  bench harness (perf), a fuzz unit test (robustness), and a static guard (telemetry); the scaffolded
  `client/test-e2e/US-901.acceptance.test.js` stub is removed. Bug-bash fixes that touch user-observable
  behavior get their own red-first test at fix time. Keep `test:parser`/`test:server`/`test:e2e`/`eval`
  green; record bench numbers in `verification.md`.
