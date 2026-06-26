# Tasks (Spike): Unknown-Selector Heuristic

**ID**: SPIKE-01 | **Spec**: ./spec.md | **Plan**: ./plan.md

Time-box: 2–3 days. Output is a go/no-go memo, not shipped diagnostics.

## Phase 1 — Setup
- [x] T001 Confirmed US-430 loader exposes `methodTableOf(classId, side)` (own ∪ inherited ∪ traits).

## Phase 2 — Implementation (flagged off)
- [x] T010 `server/src/diagnostics/unknownSelectorGate.ts` + `cartridgeClassWorld.ts`: closed-world receiver resolution + resolved method table (incl. metaclass protocol + the `self` subclass closure). (AC1)
- [x] T011 §4 truth table (single emit row; severity Hint). (AC1)
- [x] T012 §5 escape hatches: DNU-in-chain, perform: family, proxy/forwarding, incomplete table, reflective allowlist, explicit opt-out — all unit-tested (`server/test/unknownSelectorGate.test.ts`, 16 cases). (AC2)

## Phase 3 — Measure & decide
- [x] T020 `scripts/spike-unknown-selector.ts`: walked GST kernel (122 files / 21,711 sends) + learning-smalltalk; tally + report saved to `corpus-report.txt`.
- [x] T021 Triaged every emit. Naive 58 FPs → **12** after the `self` subclass-union fix; of the 12, ~4 genuine catches (incl. the `primtiveFailed` typo), ~7-8 cartridge-completeness FPs. Coverage ~27%. (AC3)
- [x] T022 Go/no-go memo in `verification.md`: **SHELVE** (zero-FP bar unmet + low coverage); reconsider after cartridge hardening + scope restriction. No feature story filed. (AC4)
