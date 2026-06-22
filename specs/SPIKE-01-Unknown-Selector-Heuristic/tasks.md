# Tasks (Spike): Unknown-Selector Heuristic

**ID**: SPIKE-01 | **Spec**: ./spec.md | **Plan**: ./plan.md

Time-box: 2–3 days. Output is a go/no-go memo, not shipped diagnostics.

## Phase 1 — Setup
- [ ] T001 Confirm US-430 loader exposes `methodTableOf(classId)` (own ∪ inherited ∪ traits).

## Phase 2 — Implementation (flagged off)
- [ ] T010 `server/src/diagnostics/unknownSelectorGate.ts`: closed-world receiver resolution + resolved method table. (AC1)
- [ ] T011 §4 truth table (single emit row; severity Hint). (AC1)
- [ ] T012 §5 escape hatches: DNU-in-chain, perform: family, proxy/forwarding, incomplete table, reflective allowlist, explicit opt-out. (AC2)

## Phase 3 — Measure & decide
- [ ] T020 `scripts/spike-unknown-selector.ts`: walk `learning-smalltalk/` + GST kernel; tally emits.
- [ ] T021 Triage every emit (true catch vs false positive); compute precision + closed-world coverage. (AC3)
- [ ] T022 Go/no-go memo in `verification.md` (adopt → file feature story; or shelve with evidence). (AC4)
