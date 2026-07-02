# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins
**Type**: Requirements Quality Gate
**Story**: US-901 — Hardening & Perf | **Issue**: #112

---

## Section 1: Constitution Gates (Mandatory)
- [x] **Native Look & Feel**: No new UI; existing standard VS Code/LSP surfaces only.
- [x] **Zero Config**: Bench is a dev/release tool, not a user setting; no new runtime config. No-telemetry stance is the zero-config default.
- [x] **Protocol First**: Work is behind the existing LSP providers; the bench drives the same pure functions the server uses.
- [x] **Robustness**: This story *is* a robustness pass — AC4/AC5 make the never-throw + bounded invariants explicit and tested.
- [x] **Dialect Agnostic**: Perf/robustness apply to the Console; no dialect-specific assumptions (corpus is GST-flavored but the harness is dialect-neutral).

## Section 2: Specification Completeness
- [x] Goals and Non-Goals explicitly listed? (spec §2/§3 — incl. no-CI-gate, no worker offload)
- [x] User stories in standard format? (spec §4)
- [x] Acceptance scenarios defined? (AC1–AC6, each with a measurable/assertable condition)
- [x] Edge cases identified? (fuzz/adversarial input AC4; pathological large/nested file AC5)
- [x] Dependencies listed? (builds on US-412 index, US-413 completion, US-411 parser, US-428 class-rename scan)

## Section 3: Technical Design
- [x] API/Command contracts defined? (`npm run bench`, `scripts/bench.ts`, `scripts/gen-corpus.ts`; bench exit code = budget pass/fail)
- [x] Data structures defined? (reuses `WorkspaceIndex`, `workspaceXref`, `LexResult`/`ParseResult`; optional `getText`/`entriesWithText` accessor if the scan is optimized)
- [x] Error handling strategy defined? (never-throw invariant is the subject; bench is best-effort/local)
- [x] Testing strategy defined? (bench harness for perf; fuzz unit test for robustness; static guard test for telemetry; per-fix regression tests for bug-bash)

## Section 3.5: Acceptance Harness (TDD e2e plan) — AC ROUTING
US-901 adds **no new user-observable feature surface** — it is perf + robustness + audit + a bug-bash.
Per the routing rule, ACs go to unit / bench / guard layers, **not** e2e. The scaffolded
`client/test-e2e/US-901.acceptance.test.js` stub is therefore **removed** (T006).

| AC | Nature | Routed to | Red-first? |
|----|--------|-----------|-----------|
| AC1 Index budget | Perf measurement | `scripts/bench.ts` (`npm run bench`), budget-gated exit | Bench asserts budget; RED = over-budget before any opt |
| AC2 Completion budget | Perf measurement | `scripts/bench.ts` p95 | as AC1 |
| AC3 No telemetry | Data/build invariant | Static guard test (`server/test/`) over sources/`dist` | RED if denylist token present |
| AC4 Never-throw | Robustness invariant | Fuzz unit test `server/test/robustness.test.ts` over `tokenize`/`parse` | RED on any throwing input found |
| AC5 Stress limits | Robustness/perf | Fuzz unit test (ceiling assert) + bench rename/large-file timing | RED if unbounded/over-ceiling |
| AC6 Bug-bash | Process → fixes | Each fix-now defect → its own **red-first** regression test (unit/eval/e2e as fits) | per-defect |

- [x] Each AC routed (table above).
- [x] User-observable ACs pinned red-first: **none are user-observable** for the story itself; bug-bash fixes (AC6) that touch user-observable behavior get their own red-first test at fix time.
- [x] No user-observable surface stated → the e2e stub is removed (recorded in tasks T006).

## Section 4: Validation Result
- [x] **PASS** — Ready for implementation. Design calls (perf harness form, synthetic corpus, scope) locked in Clarify; ACs measurable and routed.
