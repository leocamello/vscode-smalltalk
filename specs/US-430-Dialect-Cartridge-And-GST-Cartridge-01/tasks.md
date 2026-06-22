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
- [x] T010 `server/src/kernel/cartridgeLoader.ts`: typed inline import of the cartridge JSON (`bundledCartridge`, esbuild-inlined).
- [x] T011 `loadCartridge`: resolved `methodTableOf(classId, side)` (own ∪ inherited ∪ trait-composed, own>trait, subclass>super) + implementor/sender views (from `crossReference`, implementors derived when absent); memoized by `contentHash` (identity-guarded for the `pending` placeholder).
- [x] T012 `cartridgeToKernelIndex(cartridge): KernelIndexData` projection (keys by simple name, maps superclass ClassId→name, `gnu-smalltalk`→`gst`). (AC7)
- [x] T013 Repoint `KernelIndexService` bundled source at the projected cartridge; `completion.ts` unchanged. Old `kernel-index.json` tree-shaken out of the bundle.
- [x] T014 Tests (`server/test/cartridgeLoader.test.ts`): schema round-trip (no functions/cycles) (AC2); no-prose licensing gate (AC4); projection over a fixture + real cartridge; loader inheritance/trait resolution.

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
- [x] T900 `evals/datasets/completion/` green across the swap (8/8, now sourced from the 249-class cartridge) — behaviour-preserving gate.
- [ ] T901 `verification.md` gate passed (completion unchanged; installed vs floor; with/without gst).
- [ ] T902 CI green on Linux/macOS/Windows.
- [x] T903 Decision recorded (before Slice C): **ship the rich floor as-is** (full 249-class base image), per ADR-0003 (sized rich) — completion eval stayed green across the swap, so no kernel-subset filtering. Revisit only if completion noise is reported.
