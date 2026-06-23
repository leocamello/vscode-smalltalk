# Specification: Dialect Cartridge Schema + GST Cartridge #01 + Console Loader

**ID**: US-430
**Feature**: The Console & Cartridges data foundation — pure-JSON cartridge schema, GST reflective exporter, runtime loader, and kernel-index convergence
**Status**: In Progress (schema + exporter + Cartridge #01 done; loader + convergence remain)
**Owner**: Leonardo Nascimento
**Created**: 2026-06-22
**Architecture**: [ADR-0002 — Kernel symbol sourcing](../../docs/decisions/0002-kernel-symbol-sourcing.md), [ADR-0003 — Cartridge resolution](../../docs/decisions/0003-cartridge-resolution.md)

## 1. Overview
The data foundation for EPIC-005 ("Console & Cartridges"): a dialect-neutral **Console** that loads
frozen, per-dialect **Cartridges** of resolved facts. GNU Smalltalk 3.2.5 is **Cartridge #01**.

The schema (`server/src/types/knowledge-base.ts`) and the reflective GST exporter
(`scripts/export-gst-cartridge.st` → `server/data/cartridges/gst-3.2.5-cartridge.json`, 249 classes /
4746 signatures) are **built and validated**. What remains is wiring the cartridge into the running
server (a loader) and **converging** it with the US-413 kernel index so there is one canonical fact
model, per [ADR-0003](../../docs/decisions/0003-cartridge-resolution.md).

## 2. Goals
- A pure-JSON `DialectCartridge` schema: resolved facts only (no ASTs), explicit class-side/instance-side
  split, superclass/trait resolution separate from an open `taxonomy` bag. **(done)**
- A reflective GST exporter as the template for future Pharo/Squeak image-export adapters; build-time
  `gst` only, runtime stays zero-dependency. **(done)**
- A **runtime cartridge loader** that inlines the committed JSON (esbuild) and exposes an in-memory index.
- **Convergence**: make `DialectCartridge` the canonical model; a thin projection feeds the existing
  completion service; the static `indexKernelDirectory` becomes the **preferred, cached Tier-1** adapter
  and the committed cartridge the **rich frozen Tier-2 floor** (ADR-0003). Retire `kernel-index.json`.
- Tests: schema round-trip (no functions/cycles), licensing no-prose gate, cartridge load smoke,
  projection/resolution; keep the completion eval green.

## 3. Non-Goals
- **No new feature providers** — semantic tokens (US-422), references/senders (US-423), hover (US-415)
  are separate stories that *consume* this foundation.
- **No full ADR-0003 generate-and-cache Tier-1** — the on-user-machine reflective-runtime mode + cache
  keying/invalidation is **US-603** (EPIC-006). US-430 lands the *model convergence* + the static
  installed adapter + the frozen floor.
- No image-based (Pharo/Squeak) adapter yet — US-601.

## 4. User Stories & Acceptance Criteria
**US-430**: As a maintainer, I want a pure-JSON Dialect Cartridge schema and a GST exporter that compiles
GNU Smalltalk 3.2.5 into Cartridge #01, so that the Console can serve image-grade kernel knowledge
offline and new dialects become a new cartridge rather than a rewrite.

- **AC1**: `knowledge-base.ts` defines `DialectCartridge` — pure JSON, facts only, class/instance split,
  superclass/trait resolution separate from `taxonomy`. **(done)**
- **AC2**: A test proves a cartridge round-trips through `JSON.parse` with no functions/cycles and
  validates structurally against the schema. **(done — cartridgeLoader.test.ts)**
- **AC3**: `export-gst-cartridge.st` runs headlessly, emits the cartridge (classes + `crossReference`). **(done)**
- **AC4**: Facts-only (`carriesProse: false`); a test asserts no prose fields. **(done)**
- **AC5**: Deterministic output (sorted keys); `contentHash` stamped by the TS build step. **(done —
  `stamp-cartridge.ts` + `cartridgeHash.ts`; determinism guard test)**
- **AC6**: Exporter is the documented template for image-export adapters. **(done)**
- **AC7 (convergence)**: a runtime loader exposes the cartridge; the completion service reads it through a
  projection; `indexKernelDirectoryToCartridge` emits cartridge shape as the Tier-1 installed adapter; the
  bundled cartridge is the Tier-2 floor; `kernel-index.json` + `gen-kernel-index.ts` are retired. **(done)**

## 5. Technical Design
**The cartridge IS the per-user generator output.** The same schema + exporter serve build-time (frozen
floor) and, later, user-time generation (US-603). US-430 makes the runtime *consume* a cartridge.

### 5.1 Loader (`server/src/kernel/cartridgeLoader.ts`, new)
- Inline the committed JSON so esbuild bundles it: `import data from '../../data/cartridges/gst-3.2.5-cartridge.json'`
  typed as `DialectCartridge` (mirror `bundledIndex.ts`, which today inlines `kernel-index.json`).
- Expose `loadCartridge(data): LoadedCartridge` that builds the in-memory views used by features:
  the class map (already keyed by `ClassId`), and **resolved lookups** — `methodTableOf(classId)`
  (own ∪ inherited via superclass chain ∪ trait-composed), a `selector → ImplementorRef[]` and
  `selector → SendSite[]` view (the cartridge already ships these precomputed in `crossReference`).
- Pure data, `vscode`-free; memoize derived views by `header.contentHash`.

### 5.2 Projection → keep completion working (`cartridgeLoader.ts` → `KernelIndexService`)
- `DialectCartridge` is a **superset** of the US-413 `KernelIndexData`. Add a ~30-line projection
  `cartridgeToKernelIndex(cartridge): KernelIndexData` (drop `crossReference`/traits/taxonomy/protocol;
  map `instanceMethods`/`classMethods` → `instanceSelectors`/`classSelectors`).
- Point `KernelIndexService`'s bundled source at `cartridgeToKernelIndex(bundledCartridge)`. Completion
  (`completion.ts`) is unchanged — it still consumes `classes()`/`selectors()`.

### 5.3 Installed adapter → cartridge shape (ADR-0003 Tier-1)
- Evolve `indexKernelDirectory` (or add a thin wrapper) to emit a `DialectCartridge` (at least the
  `classes` tier; `crossReference` optional) from the user's installed kernel **source** dir — still
  **no runtime `gst`**, just our parser. The resolution chain then compares like-for-like cartridges.
- `KernelIndexService.configure()` already does prefer-installed-else-bundled; repoint it at cartridges.

### 5.4 Retire the old index
- Delete `server/data/kernel-index.json`, `scripts/gen-kernel-index.ts`, and `bundledIndex.ts`'s
  old import; the cartridge generator (`export-gst-cartridge.st`, CI-runnable) replaces it.
- Update `npm` scripts + CLAUDE.md generation notes. `model.ts` (`KernelIndexData`) may remain as the
  projection target during transition, or be folded in.

### 5.5 Slices (each a reviewable PR; three test layers green)
- **A (done)** — schema + exporter + committed Cartridge #01.
- **B** — loader + projection + AC2/AC4 tests; completion now reads the cartridge (behaviour-preserving).
- **C** — installed adapter emits cartridge shape; resolution wiring (Tier-1 preferred, Tier-2 floor);
  status/provenance updated to floor-vs-installed.
- **D** — retire `kernel-index.json`/`gen-kernel-index.ts`; docs + `contentHash` stamping (AC5); eval green.

### 5.6 Testing
- **Unit (`test:parser`)**: schema round-trip + structural validation (AC2); **no-prose** licensing gate
  (AC4); projection equivalence (cartridge→KernelIndexData yields the same completion candidates the old
  index did, over a fixture); loader resolves inheritance/trait tables correctly.
- **Server (`test:server`)**: completion still answers after the swap (regression).
- **Eval**: the existing `evals/datasets/completion/` must stay green across the swap (the real gate that
  convergence is behaviour-preserving).

## 6. Manual Verification
Lightweight (foundation, not a user-facing feature): in the Extension Development Host, confirm
completion is unchanged after the convergence (kernel selectors/classes still offered, provenance/status
still correct), with and without a discoverable GST install. Full matrix in `verification.md`.

## 7. Risks & Limitations
- **Behaviour drift during convergence** — the cartridge is the *base image* (249 classes, incl.
  `SystemExceptions`/`NetClients`), broader than the old `kernel/`-dir index (~250). Completion will offer
  a slightly broader set. Mitigation: the completion eval + manual check; decide if the floor should be
  filtered to a "kernel" view (open question, see notes).
- **Send-scan under-count** in `crossReference` (special-selector bytecodes missed) — documented v1
  limitation; refine via bytecode disassembly later. Doesn't affect completion/classes.
- **Esbuild JSON size** (1.2 MB inlined) — acceptable (kernel-index.json was comparable); revisit lazy
  loading only if startup budget (Constitution: activation < 500 ms) is threatened.
- **`contentHash` stamping** needs a build step (currently `"pending"`); keep the cartridge deterministic
  so the hash is stable.
