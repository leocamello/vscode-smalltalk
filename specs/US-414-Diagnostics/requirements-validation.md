# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins  
**Type**: Requirements Quality Gate  
**Story**: US-414 — Diagnostics (parser live; `gst` opt-in)

---

## Section 1: Constitution Gates (Mandatory)
- [x] **Native Look & Feel**: Uses standard LSP push diagnostics (`textDocument/publishDiagnostics`),
  standard `CodeAction` quick fixes, and a standard command — all rendered by VS Code natively.
- [x] **Zero Config**: Parser tier is always-on, no setup, no `gst`. The `gst` tier is opt-in
  (`smalltalk.diagnostics.useGst` default `false`) and auto-resolves the executable (setting → PATH).
- [x] **Protocol First**: Pure LSP — `publishDiagnostics`, `codeAction`. No bespoke channels except the
  thin `validateWithGst` command bridge (client→server), which still drives standard diagnostics.
- [x] **Robustness**: Front end never throws (empty list on failure); `gst` tier is bounded by timeout,
  killed on supersede, and silently inert when `gst` is absent; no zombies (AC3).
- [x] **Dialect Agnostic**: Parser tier already feeds from the layered front end; the `gst` runner is a
  GST-specific *optional* adapter, isolated in `server/src/gst/` — other dialects add their own later.

## Section 2: Specification Completeness
- [x] Goals and Non-Goals explicitly listed? (spec §2/§3)
- [x] User stories in standard format? (spec §4)
- [x] Acceptance scenarios defined? (AC1–AC4, each with trigger/observable)
- [x] Edge cases identified? (spec §6: gst absent, tier overlap, parser noise, stale gst on edit)
- [x] Dependencies listed? US-411 (parser diagnostics), US-301 (gst resolution lineage), ADR-0001.

## Section 3: Technical Design
- [x] API/Command contracts defined? `smalltalk.diagnostics.useGst` setting; `smalltalk.validateWithGst`
  command; `publishDiagnostics`/`codeAction` shapes (spec §5.1–§5.5).
- [x] Data structures defined? `LexDiagnostic → Diagnostic` mapping; `parseGstStderr(text,uri) →
  Diagnostic[]`; per-uri in-flight child + timer maps.
- [x] Error handling strategy defined? (Robustness gate above; spec §6).
- [x] Testing strategy (Unit vs Integration) defined? (§3.5 below).

## Section 3.5: Acceptance Harness (TDD e2e plan) — AC routing
- [x] Each AC routed to a verification layer:
  - **AC1** (parser squiggles on change) → **e2e** `client/test-e2e/US-414.acceptance.test.js`
    (open malformed `.st`, assert diagnostics with code `smalltalk(parse)`); **+ unit** for the
    `LexDiagnostic → Diagnostic` mapping (ranges/severity); **+ eval** golden dataset
    `evals/datasets/diagnostics/` (malformed sources → expected diagnostic codes/ranges/severity).
  - **AC2** (gst stderr → diagnostics, opt-in) → **unit** `parseGstStderr` against verified gst-3.2.5
    fixture strings (CI-safe, no gst); the opt-in wiring/command is exercised by the server test where
    feasible. (e2e for the live gst run is **local-only**, gated behind gst presence, recorded in
    `verification.md` manual-QA — CI must not require gst.)
  - **AC3** (no zombies / non-blocking) → **unit/integration** an explicit "rapid edits spawn no
    surviving children" test on `gstRunner` with an injected fake spawner (deterministic, no gst).
  - **AC4** (insert missing `]`/`)`) → **e2e** assert the quick fix appears and edits the buffer; **+
    unit** for the pure code-action mapping.
- [x] User-observable ACs (AC1, AC4) pinned by acceptance tests written **before** implementation
  (red → green) in `client/test-e2e/US-414.acceptance.test.js`.
- [x] AC2/AC3 have no fully CI-reproducible user-observable surface (gst is optional/absent in CI);
  routed to unit + injected-spawner integration + local manual-QA. The e2e stub is **kept** (AC1/AC4).

## Section 4: Validation Result
- [x] **PASS — Ready for implementation.** (Slice A first: AC1.)
