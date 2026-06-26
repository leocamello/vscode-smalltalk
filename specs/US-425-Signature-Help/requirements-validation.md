# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins  
**Type**: Requirements Quality Gate  
**US**: US-425 — Signature Help · **Validated**: 2026-06-26

---

## Section 1: Constitution Gates (Mandatory)
- [X] **Native Look & Feel**: Standard `textDocument/signatureHelp` drives VS Code's built-in signature
  popup (with "1 of N" cycling + active-parameter bolding); no bespoke UI (spec §5.2).
- [X] **Zero Config**: No setting required. Kernel signatures resolve via the US-430 Console
  (installed-first / frozen floor); empty index ⇒ null, graceful. Works with no `gst` (AC2).
- [X] **Protocol First**: Pure LSP — advertises `signatureHelpProvider`; no out-of-band channel (spec §5.2).
- [X] **Robustness**: Front end never throws — the provider returns `null` on any miss (no keyword context,
  empty index, malformed input); the query path is parser/token + map lookups, no I/O (spec §5.1, AC2).
- [X] **Dialect Agnostic**: Signatures come from the workspace ∪ the active **cartridge**; provenance is
  tagged. A future dialect cartridge lights up signature help with no provider change (spec §5.1).

## Section 2: Specification Completeness
- [X] Goals and Non-Goals explicitly listed (spec §2/§3 — keyword-only, no type resolution, no arg names,
  no new index).
- [X] User story in standard format (spec §4).
- [X] Acceptance criteria defined and testable (AC1 union/active-param/null-cases, AC2 no-`gst`).
- [X] Edge cases identified: nested-arg skip (`at: (a foo: b) put:`), cascade/assignment/paren/`|`
  boundaries, prefix breadth, unary/binary/head cursors ⇒ null, empty index ⇒ null (spec §5.1, §5.4).
- [X] Dependencies listed (spec §5.3): US-413 index + `kernelService`, US-430 cartridge, US-411 lexer.

## Section 3: Technical Design
- [X] API/Command contracts defined: `signatureHelpProvider` advertised in `server.ts`;
  `signatureHelpAt(offset, text, tokens, signatures) → SignatureHelp | null` (spec §5.1, §5.2).
- [X] Data structures defined: `SignatureCandidate { selector, keywords, provenance }`; the backward
  token scan's keyword accumulation + bracket-depth tracking; `SignatureInformation` packaging with
  keyword-part parameter regions + `activeParameter`/`activeSignature` (spec §5.1).
- [X] Error-handling strategy defined: never-throw; null on miss; empty-index graceful (spec §5.1, AC2).
- [X] Testing strategy defined: unit (`test:parser` — cursor analysis + matching + null cases), server
  (`test:server` — capability ad + real-server query), e2e (`test:e2e`), output eval
  `evals/datasets/signature-help/` (spec §5.4).

## Section 3.5: Acceptance Harness (TDD e2e plan)
- [X] Each AC is **routed**:
  - **AC1** (user-observable signature + active parameter) → **e2e**
    (`client/test-e2e/US-425.acceptance.test.js`, `executeSignatureHelpProvider`) + **unit**
    (`server/test/signatureHelp.test.ts`, cursor analysis/matching/null cases) + **eval**
    (`evals/datasets/signature-help/`).
  - **AC1** LSP shape (capability ad + protocol response) → **handshake** (`server/test/handshake.test.mjs`).
  - **AC2** (no `gst`) → handshake + unit run with the bundled floor (no `gst` on the path by construction).
- [X] User-observable ACs pinned by acceptance tests **written before implementation** (red → green):
  `client/test-e2e/US-425.acceptance.test.js`.
- [X] The story HAS a user-observable surface (the signature popup) — the e2e stub is filled, not removed.

## Section 4: Validation Result
- [X] **PASS — Ready for implementation.** The spec is complete, testable, and constitutionally clean. The
  central design choice — keyword-prefix matching presented as an honest union with provenance, and the
  active parameter tracked from a robust backward token scan — is pinned to AC1 assertions across unit /
  handshake / e2e / eval. The no-`gst` guarantee (AC2) is structural (bundled floor + pure token/lookup
  path). Proceed to the Acceptance Harness (write the failing tests first).
