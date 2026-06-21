# Implementation Plan: Navigation Polish — Folding + Document Highlight

**ID**: US-417 | **Date**: 2026-06-20 | **Spec**: ./spec.md | **Branch**: `feature/US-417-navigation-polish-folding-and-document-highlight`

## Summary
Two more LSP providers over the US-411 front end, reusing `parseCache`: **A** `foldingRange`
(class/method/block/comment folds — truly near-free) and **B** `documentHighlight` (scope-aware
occurrence highlighting). Delivered as two reviewable slices. The retired-Marketplace-badge README
fix shipped separately (PR #44) per the request.

## Approach
- **`documents/parseCache.ts`** — add `getAst(doc)` (and `getTokens(doc)` if needed) alongside
  `getSymbols`, memoized by the same `(uri, version)` key, so both providers read the cached parse.
- **Slice A — `providers/foldingRange.ts`**: `toFoldingRanges(ast, tokens)`:
  - Generic AST walk (reuse the `visit` pattern from `definition.ts`); for `Definition`,
    `MethodDefinition`, `Block` with `startPos.line < endPos.line`, push `{ startLine, endLine }`.
  - Multi-line `Comment` tokens → `{ startLine, endLine, kind: 'comment' }`.
  - LSP `endLine` = the line of the node's last line that still contains content (so the closing
    `]`/`!` line stays visible per convention; pick `endPos.line` or `endPos.line - 1` — pin with
    tests).
- **Slice B — `providers/documentHighlight.ts`**: `documentHighlightsAt(ast, offset)`:
  - Classify the cursor with the `definition.ts` resolver (selector vs variable).
  - **Selector** → all `Message` nodes with that selector; highlight the selector range.
  - **Variable** → walk up to the nearest `Block`/`MethodDefinition`/`Program` whose params/temps
    bind the name; collect same-name `Variable` nodes within that subtree; no binding → file-wide.
  - Small AST touch: add the selector token range to `MessageNode` (`selectorStart/End`/positions)
    in the parser so the selector highlight is precise, not the whole send. One focused parser edit
    + snapshot refresh; behavior otherwise unchanged.
- **`server.ts`** — advertise `foldingRangeProvider` + `documentHighlightProvider`; register the
  handlers reading the cache.

## Steps (slices)
**Slice A — foldingRange (AC1)** ← start here
1. `parseCache.getAst` (+ token access).
2. `providers/foldingRange.ts` + unit tests (per construct, multi-line comment, single-line skipped, exact line numbers).
3. Wire `onFoldingRanges`; advertise capability.
4. Real-server LSP test (`foldingRange` request) + Electron e2e row.
5. Green → PR.

**Slice B — documentHighlight (AC2)**
6. Add selector range to `MessageNode` (parser) + refresh AST snapshots.
7. `providers/documentHighlight.ts` (selector + scoped variable) + unit tests (selector match; scoped variable; no cross-scope bleed).
8. Wire `onDocumentHighlight`; advertise capability; real-server + e2e.
9. Green → PR.

**Release (0.4.x)**
10. Bump version (0.4.1) + CHANGELOG; manual spot-check; cut `v0.4.1`.

## Dependencies & Risks
- **Dep**: US-411 (AST/tokens), US-412 (`parseCache`, provider plumbing) — shipped.
- **`endLine` off-by-one** — pin with exact-line unit tests.
- **Selector range** — the one parser/AST touch; keep it additive (don't disturb existing ranges);
  refresh AST snapshots and confirm no diff beyond the new field.
- **Variable scoping** — v1 = nearest binding scope; document shadowing limits. If it balloons,
  ship Slice A and split B (per spec §6).

## Verification
Per slice: `check-types` + `lint` + unit (tsx) + real-server LSP green; AST snapshots stable
(except the additive selector-range field in Slice B). Story-level: Electron e2e rows + a manual
spot-check in the Extension Host (fold a class/method/block/comment; highlight a selector and a
scoped variable), then the 0.4.1 release.
