# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding
**Type**: Implementation Verification
**Story**: US-417 — Navigation polish (folding + document highlight) · target v0.4.1

---

## Section 1: Acceptance Criteria
- [x] All ACs in `tasks.md` are checked (AC1 folding, AC2 document highlight).
- [x] Each AC has passing tests — unit (`providers.test.ts`) + real-server LSP (`handshake.test.mjs`) + Electron e2e (`client/test-e2e/`, 7/7).

## Section 2: Code Quality
- [x] `npm run lint` passes.
- [x] `npm run test:parser` + `npm run test:server` + `npm run test:e2e` pass; `npm run check-types` clean.
- [x] No unjustified `any`; TSDoc on the new provider surfaces.

## Section 3: Constitutional Compliance
- [x] **Native**: standard `foldingRange`/`documentHighlight` LSP surfaces; no bespoke UI.
- [x] **Zero Config**: reuses the parse cache; no settings; no `gst`.
- [x] **Robustness**: built on the never-throws front end; empty result on an unresolved cursor.
- [x] **TDD**: tests authored with the change at all three layers; no parser/AST change (snapshots untouched).

## Section 4: Manual Verification — Extension Host spot-check (0.4.1 gate)
Launch with **F5**, then open **[`spot-check.st`](./spot-check.st)** (in this folder) — a single
self-contained file with the exact S1/S2/S3 examples and inline instructions. It parses with **0
diagnostics**, so any odd behaviour is the feature under test, not a parse error.
Tip: to see *only* this extension's output in the console, launch with installed extensions disabled:
`code --extensionDevelopmentPath="$PWD" --disable-extensions <folder>`.

| ID | Check | Expected | Result |
|----|-------|----------|--------|
| S1 | **Fold** a class, a method, a block, and a multi-line comment via the gutter chevrons. | Each collapses on its own range. | ☐ |
| S2 | Cursor on a **message selector** (e.g. `printNl`, `at:`). | Every send of that selector is highlighted in the file. | ☐ |
| S3 | Cursor on a **local variable / block param**. | Only its same-scope uses highlight — a same-named local in another method does not. | ☐ |
| S4 | Developer Tools console. | No errors/exceptions. | ☐ |

## Section 5: Sign-Off
- [x] Automated gates green (Sections 1–3); slices #45 + #46 merged.
- [ ] Manual spot-check (Section 4) passed in the Extension Host.
- [ ] Ready to cut **v0.4.1** (version + CHANGELOG done in the release PR → tag the Release → CI publishes).
