# Tasks: Dialect Cartridge + Console loader + convergence

**ID**: US-430 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Map tasks to acceptance criteria where possible.

## Phase 1 — Spec & Setup
- [x] T001 Schema designed + reviewed (`server/src/types/knowledge-base.ts`). (AC1)
- [x] T002 Reflective exporter written + validated against local gst 3.2.5. (AC3/AC6)
- [x] T003 Cartridge #01 generated + committed (249 classes / 4746 signatures, 0 prose). (AC3)
- [ ] T004 `requirements-validation.md` gate passed.

## Phase 2 — Implementation
### Slice B — loader + projection
- [ ] T010 `server/src/kernel/cartridgeLoader.ts`: typed inline import of the cartridge JSON.
- [ ] T011 `loadCartridge`: resolved `methodTableOf(classId)` (own ∪ inherited ∪ trait-composed) + implementor/sender views; memoize by `contentHash`.
- [ ] T012 `cartridgeToKernelIndex(cartridge): KernelIndexData` projection (~30 lines). (AC7)
- [ ] T013 Repoint `KernelIndexService` bundled source at the projected cartridge; `completion.ts` unchanged.
- [ ] T014 Tests: schema round-trip (no functions/cycles) (AC2); no-prose licensing gate (AC4); projection-equivalence vs old index over a fixture.

### Slice C — installed adapter + resolution
- [ ] T020 `indexKernelDirectory` emits `DialectCartridge` shape (classes tier; crossReference optional); still no runtime `gst`.
- [ ] T021 `KernelIndexService.configure()`: Tier-1 installed (preferred) else Tier-2 floor (ADR-0003).
- [ ] T022 Provenance/status labels: floor (frozen reference) vs installed.
- [ ] T023 Tests: resolution/discovery over a temp fixture kernel dir.

### Slice D — retire + finalize
- [ ] T030 Delete `server/data/kernel-index.json` + `scripts/gen-kernel-index.ts`; update `bundledIndex.ts`/npm scripts.
- [ ] T031 `contentHash` stamping in the gen/build step (AC5); deterministic-output guard.
- [ ] T032 Update CLAUDE.md generation notes + doc-rot pass.

## Phase 3 — Verify
- [ ] T900 `evals/datasets/completion/` green across the swap (behaviour-preserving gate).
- [ ] T901 `verification.md` gate passed (completion unchanged; installed vs floor; with/without gst).
- [ ] T902 CI green on Linux/macOS/Windows.
- [ ] T903 Record the "rich floor vs kernel-subset" decision before Slice C ships.
