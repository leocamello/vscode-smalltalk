# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins
**Type**: Requirements Quality Gate
**Story**: US-430 — Dialect Cartridge schema + GST Cartridge #01 + Console loader + convergence
**Architecture**: [ADR-0002](../../docs/decisions/0002-kernel-symbol-sourcing.md), [ADR-0003](../../docs/decisions/0003-cartridge-resolution.md)

---

## Section 1: Constitution Gates (Mandatory)
- [x] **Native Look & Feel**: No new UI. The convergence is behaviour-preserving for the standard
  completion widget and snippets; the only surface change is the existing status-bar **label** wording
  (floor vs installed) — still a standard `StatusBarItem`.
- [x] **Zero Config**: Default `kernelLibrary = auto` still gives useful completion with no settings and no
  `gst` — now served by the rich frozen reference floor (Cartridge #01) when nothing is installed.
- [x] **Protocol First / Zero-Runtime**: The cartridge is frozen, pure JSON inlined by esbuild; the runtime
  loads it with **no `gst` and no VM**. Build-time `gst` is allowed only to *generate* the floor
  (ADR-0001). The installed adapter is a static `.st` parse — also no runtime `gst`.
- [x] **Robustness**: Built on the never-throws front end; the loader/projection are pure and total; the
  service degrades installed → floor → empty; the cartridge is an always-safe fallback.
- [x] **Dialect Agnostic**: The `DialectCartridge` schema is dialect-neutral (class/instance split,
  superclass/traits resolution separate from an open `taxonomy` bag) and fed by source adapters
  (reflective image-export = template; static `.st` parse = GST installed). New dialects are an additive
  cartridge, not a rewrite (Principle IV, ADR-0003).

## Section 2: Specification Completeness
- [x] Goals and Non-Goals explicitly listed (§2/§3; **excludes** new feature providers (US-422/423/415),
  the full generate-and-cache Tier-1 with cache keying/invalidation (US-603), and image-based adapters
  (US-601)).
- [x] User story in standard format (§4).
- [x] Acceptance criteria defined (§4 AC1–AC7), each mapped to evidence: schema, round-trip, headless
  exporter, facts-only, determinism + `contentHash`, exporter-as-template, and the convergence (loader +
  projection + installed adapter + floor + retirement).
- [x] Edge cases identified: round-trip with no functions/cycles, no-prose licensing gate, completion-set
  **drift** (base image broader than the old kernel-dir index — eval-gated), esbuild JSON size, deterministic
  output so `contentHash` is stable, installed-vs-floor resolution, with/without a discoverable `gst`.
- [x] Dependencies listed (plan): US-411 parser/symbols, US-413 kernel service/completion + `parseCache`
  (merged); build-time `gst` to (re)generate the floor (dev box has it; CI commits the artifact).

## Section 3: Technical Design
- [x] API/Command contracts defined: `cartridgeLoader.ts` (`loadCartridge` → `methodTableOf`/implementor/
  sender views; `cartridgeToKernelIndex` projection); `indexKernelDirectoryToCartridge` (installed adapter,
  cartridge shape); `KernelIndexService` resolution (Tier-1 installed / Tier-2 floor); `stamp-cartridge`
  build step (`npm run stamp:cartridge`).
- [x] Data structures defined: the `DialectCartridge` schema (`knowledge-base.ts`), the committed
  `gst-3.2.5-cartridge.json` shape, the `KernelIndexData` projection target (retained during convergence),
  the `contentHash` over fact tables (`cartridgeHash.ts`).
- [x] Error handling strategy defined: pure/total loader + projection; never-throws installed adapter and
  service; empty/partial degradation; the frozen floor is the always-safe baseline.
- [x] Testing strategy (Unit vs Integration) defined: unit (round-trip / no-functions-cycles, **no-prose**
  licensing gate, projection-equivalence over a fixture + the real cartridge, loader inheritance/trait
  resolution, installed-adapter cartridge shape, **`contentHash` determinism guard**) + server (completion
  regression) + e2e (fixture workspace) + the **completion eval** as the behaviour-preserving convergence
  gate + a **manual verification matrix** (`verification.md`).

## Section 4: Validation Result
- [x] PASS - Ready for implementation *(validated retroactively against the as-built Slices A–D)*

**Emphasis**: three hard, story-specific gates beyond green CI — (1) the **licensing** constraint stays
encoded as a test (the cartridge carries *facts only*, `carriesProse: false`, no comment prose); (2) the
**completion output eval** must stay green across the cartridge swap (the real proof the convergence is
behaviour-preserving); and (3) per PO direction the **manual verification matrix** in `verification.md`
(completion unchanged after the swap, installed-vs-floor status/provenance labels, with/without `gst`,
clean-install VSIX) is the release-readiness gate.
