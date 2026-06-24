# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins  
**Type**: Requirements Quality Gate  
**Story**: US-414 — Diagnostics (parser live; `gst` opt-in)

---

> **Scope note:** these gates were validated for the original two-tier design; the `gst` tier was
> deferred to EPIC-007 (spec §7). The shipped scope is the parser tier (AC1) + quick fixes (AC4).

## Section 1: Constitution Gates (Mandatory)
- [x] **Native Look & Feel**: Uses standard LSP push diagnostics (`textDocument/publishDiagnostics`)
  and standard `CodeAction` quick fixes — all rendered by VS Code natively.
- [x] **Zero Config**: Parser tier is always-on, no setup, **no `gst`**.
- [x] **Protocol First**: Pure LSP — `publishDiagnostics`, `codeAction`. No bespoke channels.
- [x] **Robustness**: Front end never throws (empty list on failure); diagnostics never block typing.
- [x] **Dialect Agnostic**: Parser tier feeds from the layered front end; no GST-specific runtime
  dependency ships (the deferred gst runner would be an isolated EPIC-007 adapter).

## Section 2: Specification Completeness
- [x] Goals and Non-Goals explicitly listed? (spec §2/§3)
- [x] User stories in standard format? (spec §4)
- [x] Acceptance scenarios defined? (AC1–AC4, each with trigger/observable)
- [x] Edge cases identified? (spec §6: gst absent, tier overlap, parser noise, stale gst on edit)
- [x] Dependencies listed? US-411 (parser diagnostics), US-301 (gst resolution lineage), ADR-0001.

## Section 3: Technical Design
- [x] API/Command contracts defined? `publishDiagnostics`/`codeAction` shapes (spec §5.1–§5.3).
- [x] Data structures defined? `LexDiagnostic → Diagnostic` mapping; the per-uri diagnostics debounce
  timer map; the closer/string `toCodeActions` grouping.
- [x] Error handling strategy defined? (Robustness gate above; spec §6).
- [x] Testing strategy (Unit vs Integration) defined? (§3.5 below).
- _gst-tier contracts (setting/command/runner) **deferred to EPIC-007** — see spec §7._

## Section 3.5: Acceptance Harness (TDD e2e plan) — AC routing
- [x] Each AC routed to a verification layer:
  - **AC1** (parser squiggles on change) → **e2e** `client/test-e2e/US-414.acceptance.test.js`
    (open malformed `.st`, assert diagnostics with code `smalltalk(parse)`); **+ unit** for the
    `LexDiagnostic → Diagnostic` mapping (ranges/severity); **+ eval** golden dataset
    `evals/datasets/diagnostics/` (malformed sources → expected diagnostic codes/ranges/severity).
  - **AC4** (insert a missing closer `]`/`)`/`}`/`>` or close a string) → **e2e** assert the quick fix
    appears, edits the buffer, and the squiggle clears; **+ unit** for the pure code-action mapping
    (each closer + string, "applying re-parses clean", non-fixable parse error offers nothing, position
    regressions for `}`/`>`).
  - **AC2/AC3** (opt-in gst tier) → **deferred to EPIC-007** (spec §7); no longer in 0.6.0 scope.
- [x] User-observable ACs (AC1, AC4) pinned by acceptance tests written **before** implementation
  (red → green) in `client/test-e2e/US-414.acceptance.test.js`.

## Section 4: Validation Result
- [x] **PASS.** Shipped scope: AC1 (parser tier) + AC4 (quick fixes). AC2/AC3 deferred to EPIC-007.
