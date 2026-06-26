# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding  
**Type**: Implementation Verification  
**US**: US-425 — Signature Help

---

## Section 1: Acceptance Criteria
- [x] All ACs in `tasks.md` are checked (AC1 active-param union + null cases; AC2 no-`gst`).
- [x] Each AC has a passing test:
  - **AC1** → `server/test/signatureHelp.test.ts` (16 unit tests: cursor analysis, prefix union,
    active param, dedup, null cases) + `evals/datasets/signature-help/` (10 golden cases) +
    `server/test/handshake.test.mjs` (capability ad + real-server request) +
    `client/test-e2e/US-425.acceptance.test.js` (executeSignatureHelpProvider → active param; unary → none).
  - **AC2** (no `gst`) → all of the above run over the bundled cartridge with no `gst` on the path.
- [x] Each user-observable AC's acceptance test was **red before** implementation (the unit suite failed
  with `MODULE_NOT_FOUND` for the missing provider; e2e/handshake added in the same red phase).

## Section 2: Code Quality
- [x] `npm run lint` passes.
- [x] `npm run check-types` passes; `test:parser` / `test:server` / `test:e2e` / `npm run eval` green.
- [x] No `any` types (the provider is pure `vscode-languageserver-types`; candidates are typed).
- [x] JSDoc provided for the public API (`signatureHelpAt`, `SignatureCandidate`, the module header).

## Section 3: Constitutional Compliance
- [x] **Native**: standard `textDocument/signatureHelp` drives VS Code's built-in popup (N-of-M cycling
  + active-parameter bolding); no bespoke UI.
- [x] **Zero Config**: no setting; kernel signatures resolve via the Console (installed-first / floor).
- [x] **Robustness**: never throws; returns `null` on any miss (no keyword context / empty index /
  malformed input); token+lookup path, no I/O.
- [x] **TDD**: tests written first (red → green), four layers.

## Section 4: Manual Verification
- [x] Feature works in the Extension Host — `manual-qa-workspace/` run against the
  `manual-qa-workspace/README.md` matrix (Parts A–D). **PASS** (owner, 2026-06-26): keyword sends pop
  signature help with the active parameter tracked; workspace ∪ kernel provenance shown; unary/binary/
  head/unknown cursors stay quiet. The completion-vs-signature-help double-popup was reviewed and
  accepted as conventional VS Code behaviour (left as-is, per the product discussion).
- [x] No errors in the Developer Tools console during the matrix.

> Automated layers are all green (unit 16, eval 10, handshake, e2e 29 incl. 2 new). The hands-on
> Extension-Host pass (per the `manual-qa-before-release` memory) is complete and passed. Fixture parses
> with 0 diagnostics (verified). A follow-up **selector-surface coverage audit** (snippets ∪ completion ∪
> signature-help division of labour) was raised during QA and filed to the backlog.

## Section 5: Sign-Off
- [x] Ready for Merge — automated + manual QA green; PO accepts (2026-06-26).
