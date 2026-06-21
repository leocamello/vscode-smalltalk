# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins
**Type**: Requirements Quality Gate
**Story**: US-417 — Navigation polish (folding + document highlight)

---

## Section 1: Constitution Gates (Mandatory)
- [x] **Native Look & Feel**: standard `foldingRange`/`documentHighlight` LSP surfaces (gutter folds, occurrence highlights); no bespoke UI.
- [x] **Zero Config**: no settings; reuses the existing parse cache; works on first open.
- [x] **Protocol First**: two pure LSP providers over the bundled server.
- [x] **Robustness**: built on the never-throws front end; empty result on unresolved cursor; no throw on malformed input.
- [x] **Dialect Agnostic**: consumes the dialect-neutral AST/token stream (US-411).

## Section 2: Specification Completeness
- [x] Goals/Non-Goals explicit (§2/§3; no rename/cross-file/type resolution).
- [x] User story + ACs in standard form (§4).
- [x] Acceptance scenarios (Given/When/Then) defined (§4).
- [x] Edge cases identified: multi-line-only folds, `endLine` semantics, keyword-selector spans, variable scoping/shadowing, no-binding → file-wide.
- [x] Dependencies listed: US-411 (AST/tokens) + US-412 (`parseCache`, provider plumbing) — both shipped.

## Section 3: Technical Design
- [x] API/Command contracts: `foldingRangeProvider`, `documentHighlightProvider`; `toFoldingRanges`, `documentHighlightsAt` (§5).
- [x] Data structures: reuse AST nodes + token stream; one small AST touch (selector range) for highlight precision.
- [x] Error handling: never-throws front end; empty results, not failures.
- [x] Testing strategy: unit (tsx) + real-server LSP + Electron e2e + manual spot-check.

## Section 4: Validation Result
- [x] PASS - Ready for implementation

**Scope note**: two slices — **A** `foldingRange` (truly near-free, ship-first) and **B**
`documentHighlight` (scope-aware; carries the small selector-range AST touch). If B's scoping
balloons, A ships alone and B becomes its own follow-up (§6).
