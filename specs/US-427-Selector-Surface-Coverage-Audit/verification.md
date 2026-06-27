# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding
**Type**: Implementation Verification
**Story**: US-427 — Selector-surface coverage audit · **Date**: 2026-06-27

---

## Section 1: Acceptance Criteria
- [x] All tasks in `tasks.md` checked (Phase 3 closed below).
- [x] **AC1 (inventory)** — `spec.md §5.1` (three-surface table + benchmark; the cartridge cross-check
  corrected `valuesDo:`→dropped, `withIndexDo:`→`doWithIndex:`).
- [x] **AC2 (division-of-labour doc)** — [ADR-0004](../../docs/decisions/0004-selector-surface-division.md);
  linked from CLAUDE.md code-map + spec.
- [x] **AC3 (fill the gaps + guard)** — 14 block snippets added; `src/test/snippets-verification.js`
  (unique prefixes + cartridge cross-check + prefix snapshot) GREEN, wired into `npm run eval`.
- [x] **AC4 (no behaviour change)** — completion/signature-help providers untouched; their evals + the
  LSP handshake unchanged and green.
- [x] **TDD:** the guard was **red before** the snippets (missing snapshot / pre-add prefix set),
  driven green by the additions. AC1/AC2 are reviewed doc artifacts (no executable test); the e2e stub
  was removed per `requirements-validation.md` §3.5 (no user-observable LSP surface).

## Section 2: Code Quality
- [x] `npm run test:parser` — crossReference + all parser suites pass.
- [x] `npm run test:server` — LSP handshake clean (signatureHelp/completion advertised, unchanged).
- [x] `npm run eval` — grammar + **snippets (35 snippets, 2459 cartridge selectors)** + completion +
  diagnostics + hover + semantic-tokens + references + signature-help all pass.
- [x] No TypeScript changed (guard is plain Node JS; only `snippets.json` + `package.json` scripts +
  docs touched) → no new `any`, no type churn.

## Section 3: Constitutional Compliance
- [x] **Native**: standard `contributes.snippets` + a Markdown ADR.
- [x] **Zero Config**: snippets always-on; works with no `gst`.
- [x] **Robustness**: guard fails loudly on collision/unknown-selector/snapshot-drift; runtime surfaces
  untouched (front end still never throws).
- [x] **TDD**: guard written red-first.

## Section 4: Manual Verification (Extension Host — run before release)
Ready-to-open workspace + the full tab-trigger matrix: [`manual-qa-workspace/`](manual-qa-workspace/README.md)
(`Playground.st` leaves a receiver in place for each new message-fragment snippet).

- [x] **Part A** — all 14 new prefixes expand with correct tab-stop order.
- [x] **Part B** — existing prefixes (`ift`, `ifn`, `ifnn`, `do`, `to`) unchanged; no new prefix
  shadows an old one (the `ifn`/`ifnn` vs `ifninn`/`ifnnin` danger zone).
- [x] **Part C** — snippet ∪ completion co-exist after a receiver (division of labour is intentional).
- [x] **Part D** — clean-VSIX parity; no Developer Tools console errors.

> Manual QA executed in the Extension Host on 2026-06-27 (Leonardo Nascimento) — all parts pass.

## Section 5: Sign-Off
- [x] Automated layers GREEN.
- [x] Manual-QA matrix executed (2026-06-27) — pass.
- [x] PO accepts — shipping in **v0.9.2**.
