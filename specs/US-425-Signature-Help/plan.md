# Implementation Plan: Signature Help

**ID**: US-425 | **Date**: 2026-06-26 | **Spec**: ./spec.md | **Branch**: `feature/US-425-signature-help`

## Summary
Add `textDocument/signatureHelp` for keyword sends: a pure provider that, from a backward token-stream
scan, reconstructs the keyword selector typed so far and the active parameter, matches it (as a prefix
union) against the workspace ∪ cartridge selector set, and returns `SignatureInformation[]` with the
keyword part highlighted. Works offline, no `gst`. Reuses the completion provider's index + provenance
rank and the lexer token stream — no new index.

## Approach
- **New provider** `server/src/providers/signatureHelp.ts` (pure, `vscode-languageserver-types` only) with
  `signatureHelpAt(offset, text, tokens, signatures) → SignatureHelp | null`. Mirrors `completion.ts`:
  token-based cursor analysis (robust at an incomplete cursor), provenance rank + `bestByName` dedup, and
  the keyword-snippet split (`selector.match(/[^:]+:/g)`) for the `keywords` derivation.
- **Wire** in `server/src/server.ts`: advertise `signatureHelpProvider` (trigger `:`/` `, retrigger `:`);
  in `onSignatureHelp`, build `SignatureCandidate[]` from `index.all()` keyword method symbols
  (`Provenance.Workspace`) ∪ `kernelService.selectors()` keyword selectors (kernel provenance).
- **Reuse**: `getTokens(doc)` (parseCache), `Provenance` + the rank/label helpers' shape from
  `completion.ts`, the `KernelSelectorEntry` from `kernelIndexService`.

## Steps
1. **Acceptance Harness (RED):** fill `client/test-e2e/US-425.acceptance.test.js`
   (`executeSignatureHelpProvider` → active parameter), add `server/test/signatureHelp.test.ts` (cursor
   analysis, prefix matching, null cases), extend `server/test/handshake.test.mjs` (capability ad + a
   `textDocument/signatureHelp` request), and add `evals/datasets/signature-help/` (cases + run.ts). Watch
   them fail for the right reason.
2. **Implement** `signatureHelp.ts` → GREEN on unit/eval.
3. **Wire** `server.ts` (capability + handler) → GREEN on handshake + e2e.
4. **Verify:** `test:parser`, `test:server`, `test:e2e`, `npm run eval` all green; build the
   `manual-qa-workspace/` and run it hands-on in the Extension Host.

## Dependencies & Risks
- Deps: US-413 index + `kernelService`, US-430 cartridge (signature source), US-411 lexer. All landed.
- Risk: short-prefix union breadth (`at:` → many) — mitigated by rank + VS Code cycling, never filtering.
- Risk: cursor-analysis edge cases (nesting, cascade, assignment) — pinned by unit tests before code.

## Verification
- **Acceptance harness (TDD e2e):** AC1 pinned by `client/test-e2e/US-425.acceptance.test.js` *before*
  implementation (red → green); cursor-analysis/matching invariants routed to
  `server/test/signatureHelp.test.ts` + `evals/datasets/signature-help/`; LSP shape to the handshake test.
  Routing recorded in `requirements-validation.md` §3.5.
- Three layers green per change: `test:parser`, `test:server`, `test:e2e`; `npm run eval`.
- Manual QA: `specs/US-425-Signature-Help/manual-qa-workspace/` + `verification.md` matrix in the
  Extension Development Host.
