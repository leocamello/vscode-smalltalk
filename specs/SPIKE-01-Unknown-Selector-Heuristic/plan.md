# Implementation Plan (Spike): Unknown-Selector Heuristic

**ID**: SPIKE-01 | **Date**: 2026-06-22 | **Spec**: ./spec.md | **Branch**: `spike/SPIKE-01-unknown-selector` (tbd)

## Summary
Time-boxed (2–3 days). Implement the §4 gate + §5 escape hatches behind a flag, measure precision +
closed-world coverage on a real corpus, and recommend adopt or shelve. No published diagnostics.

## Approach
Reuse the US-411 AST/symbols (receiver resolution, scope, chain walk) and the US-430 cartridge loader
(`methodTableOf` = own ∪ inherited ∪ traits). The gate is a pure predicate; drive it from a small harness
that walks every message-send node in a corpus and tallies emits.

New (throwaway-until-adopted): `server/src/diagnostics/unknownSelectorGate.ts` (the predicate) +
`scripts/spike-unknown-selector.ts` (corpus harness + report).

## Steps
1. Implement `shouldEmitMissingSelectorWarning` per §4 (closed-world receiver resolution; resolved method
   table via the cartridge loader). (AC1)
2. Implement the six escape hatches §5. (AC2)
3. Harness: walk `learning-smalltalk/` + `../smalltalk-3.2.5/kernel`; tally emits; triage every emit as
   true catch vs false positive; compute closed-world coverage. (AC3)
4. Write the go/no-go memo into `verification.md`. (AC4)

## Dependencies & Risks
- **Depends on US-430** (cartridge loader / `methodTableOf`) + US-411. Corpus present locally (dev box).
- **Risks** (spec §8): false positives (the whole point — zero-FP bar), low coverage (shelve signal),
  un-indexed packages (closed-world gate mitigates).

## Verification
- The spike's *output* is the verification: the precision/coverage report + memo (`verification.md`).
- If **adopt**: file the follow-up feature story (published linter: default `Hint`, opt-out, allowlist).
- If **shelve**: record the evidence; the gate code is parked, not shipped.
