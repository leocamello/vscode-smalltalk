# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding
**Type**: Implementation Verification
**Story**: US-416 — Formatting

---

## Section 1: Acceptance Criteria
- [x] All ACs in `tasks.md` are checked (Phases 1–3 complete).
- [x] Each AC has a passing test:
  - **AC1** (range + on-type) → e2e `client/test-e2e/US-416.acceptance.test.js` (range touches only the
    selection; on-type dedents on `]`) — **green**.
  - **AC2** (document whitespace-normalize, preserve comments/blanks) → e2e (Format Document changes the
    buffer; comment + blank survive) **+** output eval `evals/datasets/formatting/` (17 goldens) — **green**.
  - **AC3** (idempotence + token-stream invariance) → unit property test `server/test/format.property.test.ts`
    over hand samples **+ all 122 kernel files** (0 idempotence failures, 0 token-stream failures) — **green**.
  - **AC4** (off by default; enable gates edits) → e2e (no-op when disabled) **+** handshake (3 caps
    advertised) — **green**.
  - **AC5** (no `gst`; malformed → no edits) → unit (3 malformed samples returned unchanged) **+** eval
    (malformed goldens) — **green**.
- [x] Each user-observable AC's acceptance test was **red before** implementation (Phase 2 proof:
  `MODULE_NOT_FOUND` formatter.ts for unit/eval; `documentFormattingProvider undefined` for handshake;
  e2e enabled-suite red by construction). Recorded in `tasks.md` T007.

## Section 2: Code Quality
- [x] `npm run lint` passes.
- [x] `npm run check-types` passes (strict, `noUncheckedIndexedAccess`).
- [x] `npm run test:parser` (incl. 26 format property checks), `npm run test:server` (handshake),
  `npm run eval` (incl. 17 formatting goldens), and `npm run test:e2e` (US-416 suite) all pass locally.
- [x] No unjustified `any`. The formatter core is pure (no `vscode` import), runs under `tsx`.
- [x] JSDoc/header comments on the public surface (`formatSource`, `lineIndentDepth`, the three providers,
  `FormatOptions`/`FormatSettings`).

## Section 3: Constitutional Compliance
- [x] **Native**: standard `documentFormatting` / `rangeFormatting` / `onTypeFormatting` LSP providers; hooks
  into Format Document/Selection + `editor.formatOnSave`/`formatOnType`. No bespoke UI.
- [x] **Zero Config**: sensible defaults (indent 4, cascades align, keywordWrap 100; tabs/spaces from the
  editor). The one deliberate non-default — `enable: false` — is the safety posture mandated by AC4.
- [x] **Robustness**: front end never throws; any parse diagnostic → input returned unchanged (no
  corruption); disabled → `[]`. Idempotence + token invariance are property-tested CI gates.
- [x] **TDD**: acceptance harness written and proven red before code (Phase 2).
- [x] **Dialect-agnostic**: the rewriter consumes only the layered lexer/AST contract (`TokenKind`/`NodeKind`);
  the one dialect-sensitive rule (keep `Scope` tight) is structural, not GST-lexeme-specific. ADR-0005.

## Section 4: Manual Verification
- [x] Feature works in Extension Host — `specs/US-416-Formatting/manual-qa-workspace/` matrix
  (Parts A–E) passed by the owner (2026-06-29): off-by-default no-op; document format matches the
  documented golden; idempotent second pass; range/on-type; comments + blank lines +
  `SystemExceptions.NotYetImplemented` (tight `Scope`) preserved; `Broken.st` left byte-for-byte intact;
  `blockStyle: expand` reflows bodies one-statement-per-line. "Everything still working."
- [x] No errors in Developer Tools console during the matrix.

## Section 5: Sign-Off
- [x] **Ready for Merge** — Extension-Host Parts A–E passed + PO accepted + CI green on
  Linux/macOS/Windows + e2e. **Merged via PR #106 (squash, `15a9564`); closed #28; shipped as v0.10.0.**

---

**Automated-layer status (2026-06-28):** all green — `test:parser` (incl. format property **36**),
`test:server`, `eval` (incl. formatting **22**), `test:e2e` (US-416: **6/6**, incl. `blockStyle expand`).
Kernel corpus: 122/122 idempotent + token-invariant **in both `preserve` and `expand` modes**.
The opt-in `blockStyle: expand` (default `preserve`) delivers the expanded/one-statement-per-line aesthetic
via structural forced-breaks — still whitespace-only (ADR-0005).
**Done (2026-06-29):** automated layers green, doc-rot sweep + v0.10.0 bump + CHANGELOG (T903), manual-QA
Parts A–E passed, PO sign-off, PR #106 merged (closes #28). Remaining outside this story: cut the
`v0.10.0` GitHub Release to publish to the Marketplace.
