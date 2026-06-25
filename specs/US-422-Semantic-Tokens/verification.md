# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding  
**Type**: Implementation Verification  
**US**: US-422 — Semantic Tokens (Cartridge-Aware) · **Updated**: 2026-06-25

---

## Section 1: Acceptance Criteria
- [X] All ACs in `tasks.md` are checked (T010–T014, T900).
- [X] Each AC has a passing test:
  - **AC1** (roles) — `semanticTokens.test.ts` ivar/temp/param/block-arg rows; eval ivar/param/temp.
  - **AC2** (class iff known; cartridge ⇒ defaultLibrary) — unit `AC2` rows; eval kernel-vs-workspace-vs-unknown; `test:server` decodes a `class` token for `OrderedCollection` off the bundled cartridge; e2e AC2.
  - **AC3** (keyword parts + pseudo-vars distinct) — unit `AC3` rows; eval; e2e `AC1/AC3`.
  - **AC4** (no-cartridge capitalization fallback) — unit `AC4 with no cartridge …`.
  - **AC5** (works with no `gst`; output eval) — `evals/datasets/semantic-tokens/` (9/9, run by `npm run eval`, no `gst`).

## Section 2: Code Quality
- [X] `npm run lint` passes (eslint clean).
- [X] `npm test` passes — `test:parser` (incl. 14 new), `test:server`, `test:e2e` (23 passing), full `eval`.
- [X] No `any` types (provider is fully typed; legend/types exported as `const` tuples).
- [X] JSDoc provided for the public API (`collectSemanticTokens`, `semanticTokensFull/Range`, `encodeSemanticTokens`, `SemanticTokenContext`, legend).

## Section 3: Constitutional Compliance
- [X] **Native**: standard `textDocument/semanticTokens` (full + range) + the standard token legend.
- [X] **Zero Config**: no setting required; degrades to capitalization with no cartridge (AC4); no `gst`.
- [X] **Robustness**: never throws — empty token set on a missing doc; reuses `parseCache`.
- [X] **TDD**: unit + e2e + handshake + eval written BEFORE the provider (RED confirmed, then GREEN).

## Section 4: Manual Verification
- [X] Feature works in Extension Host — `manual-qa-workspace/` token-inspector matrix (Parts A–D)
  **accepted by the PO (2026-06-25)**; the fixture was additionally verified offline to parse with
  0 diagnostics and to classify every documented role exactly.
- [X] No errors in Developer Tools console.

## Section 5: Sign-Off
- [X] Ready for Merge — **PO-accepted 2026-06-25**; merged via PR #90 (closes #65), CI green on
  Linux/macOS/Windows + e2e, and shipped in **v0.8.0**.
