# Implementation Plan: Selector-surface coverage audit

**ID**: US-427 | **Date**: 2026-06-27 | **Spec**: ./spec.md | **Branch**: `feature/US-427-selector-surface-coverage-audit`

## Summary
Doc + additive-snippets story. Inventory the three selector surfaces (spec §5.1), pin their
division of labour in **ADR-0004**, and add 14 missing block-bearing idioms to `snippets.json` —
each a real GST 3.2.5 selector — guarded by a new Node test wired into `npm run eval`. No provider
code changes; completion/signature-help behaviour is untouched.

## Approach
- **Inventory (AC1)** lives in `spec.md §5.1` (table + benchmark). No code.
- **ADR-0004 (AC2)** — `docs/decisions/0004-selector-surface-division.md`, mirroring the
  ADR-0001..0003 format (Status/Date/Deciders/Context/Decision/Consequences). Linked from CLAUDE.md
  code-map and the spec.
- **Snippets (AC3)** — append the 14 entries from spec §5.3 to `snippets/snippets.json`, reusing the
  existing `prefix`/`body`/`description`/`scope` shape.
- **Guard (AC3)** — `src/test/snippets-verification.js`, a plain-Node test mirroring
  `src/test/grammar-verification.js` (snapshot via `--update`). Build a selector Set from
  `server/data/cartridges/gst-3.2.5-cartridge.json` (`classes[*].instanceMethods` +
  `classMethods` → `selector`); assert unique prefixes, cross-check every `:`-bearing snippet key,
  and snapshot the sorted `prefix → key` list. Wire a `test:snippets` npm script and add it to the
  `eval` chain next to `test:grammar`.

## Steps
1. Write the guard test first (RED): missing prefix snapshot + the pre-add prefix set; run it,
   confirm it fails for the right reason.
2. Add the 14 snippets to `snippets.json` (cross-check + uniqueness now pass).
3. Generate the prefix snapshot with `--update`; commit it. Guard fully green.
4. Write ADR-0004; link it from CLAUDE.md + spec.
5. Run `test:parser`, `test:server`, `npm run eval` — all green.
6. Fill `verification.md` (incl. the manual-QA tab-trigger matrix); doc-rot sweep.

## Dependencies & Risks
- Cartridge #01 (`server/data/cartridges/gst-3.2.5-cartridge.json`) is the cross-check oracle —
  already committed; the guard reads it directly.
- Risk: a snippet templating a non-GST selector (`valuesDo:`, `withIndexDo:`) — already caught in
  spec §5.1 and structurally prevented by the cross-check going forward.

## Verification
- **Acceptance harness (TDD):** AC3's guard is the RED→GREEN harness (unit, run by `npm run eval`).
  AC1/AC2 are reviewed documentation artifacts; AC4 is regression (all layers stay green). No
  user-observable LSP surface → the e2e stub is removed (recorded in `requirements-validation.md`
  §3.5).
- Manual QA: tab-trigger each new prefix in the Extension Host; confirm tab-stop order and that no
  existing prefix changed behaviour (`verification.md`).
