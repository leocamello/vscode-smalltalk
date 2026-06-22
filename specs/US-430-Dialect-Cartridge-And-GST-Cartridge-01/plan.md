# Implementation Plan: Dialect Cartridge + Console loader + convergence

**ID**: US-430 | **Date**: 2026-06-22 | **Spec**: ./spec.md | **Branch**: `feature/US-430-console-cartridges`
**Architecture**: [ADR-0002](../../docs/decisions/0002-kernel-symbol-sourcing.md), [ADR-0003](../../docs/decisions/0003-cartridge-resolution.md)

## Summary
Schema + reflective exporter + Cartridge #01 are done and committed. This plan covers the remaining
work: a runtime **cartridge loader**, a thin **projection** so the existing completion service consumes
the cartridge unchanged, evolving the **installed adapter** to cartridge shape (ADR-0003 Tier-1), and
**retiring** the US-413 `kernel-index.json`. Behaviour-preserving for completion; unblocks US-422/423/415.

## Approach
**Reuse the US-413 plumbing.** `bundledIndex.ts` already inlines `kernel-index.json`; mirror it for the
cartridge. `KernelIndexService` already resolves prefer-installed-else-bundled and exposes
`classes()`/`selectors()` to `completion.ts`; keep that contract and feed it a **projection** of the
cartridge. The cartridge is a superset of `KernelIndexData`, so the projection is mechanical.

New/changed modules:
- `server/src/kernel/cartridgeLoader.ts` (new) — inline JSON + build resolved views + `cartridgeToKernelIndex`.
- `server/src/kernel/kernelIndexService.ts` — source from cartridges (bundled floor + installed adapter).
- `server/src/kernel/indexer.ts` — emit `DialectCartridge` shape (or wrap) for the installed dir.
- Remove `server/data/kernel-index.json`, `scripts/gen-kernel-index.ts`, old `bundledIndex.ts` import.

## Steps (sliced into PRs)
1. **Slice A — done.** Schema (`knowledge-base.ts`), exporter (`export-gst-cartridge.st`), committed
   `gst-3.2.5-cartridge.json` (validated: 249 classes / 4746 signatures, 0 prose, integrity clean).
2. **Slice B — loader + projection (AC2, AC4, AC7-partial).**
   - `cartridgeLoader.ts`: typed inline import; `loadCartridge` building `methodTableOf` (chain + traits)
     and the implementor/sender views; `cartridgeToKernelIndex` projection.
   - Repoint `KernelIndexService` bundled source at the projected cartridge; completion untouched.
   - Tests: round-trip/no-prose/projection-equivalence; completion eval stays green.
3. **Slice C — installed adapter + resolution (AC7).**
   - `indexKernelDirectory` emits cartridge shape (classes tier; crossReference optional); no runtime `gst`.
   - `KernelIndexService.configure()` resolves Tier-1 installed (preferred) else Tier-2 floor; update
     provenance/status labels to floor-vs-installed (ADR-0003).
   - Tests: resolution/discovery over a temp fixture kernel dir.
4. **Slice D — retire + finalize (AC5).**
   - Delete `kernel-index.json` + `gen-kernel-index.ts`; update npm scripts + CLAUDE.md gen notes.
   - `contentHash` stamping in the build/gen step; deterministic-output guard.
   - Doc-rot pass; eval + three test layers green.

## Dependencies & Risks
- **Depends on** (merged): US-411 parser/symbols, US-413 kernel service/completion + `parseCache`.
- **Build-time only**: `gst` to (re)generate the floor cartridge (dev box has it; CI commits the artifact).
- **Risks** (spec §7): completion-set drift (base-image broader than kernel-dir) — gated by the eval +
  manual check + an open "filter floor to kernel view?" decision; esbuild size; hash stamping.

## Verification
- **Automated**: `test:parser` (round-trip, no-prose, projection equivalence, loader resolution),
  `test:server` (completion regression), `evals/datasets/completion/` green (the convergence gate).
- **Manual**: `verification.md` — completion unchanged after swap, installed vs floor, with/without `gst`.
- **Open decision to record before Slice C**: ship the rich floor as-is (base image) or filter to a
  "kernel" subset (ADR-0003 sized it *rich*; revisit only if completion noise is reported).
