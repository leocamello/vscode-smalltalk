# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding  
**Type**: Implementation Verification  
**Story**: US-415 — Hover

---

## Section 1: Acceptance Criteria
- [X] All ACs in `tasks.md` are checked.
- [X] Each AC has a passing test:
  - AC1 selector signature + implementors (+ comment by provenance) → `hover.test.ts`, hover e2e, handshake, `eval:hover`.
  - AC2 superclass chain + class comment → `hover.test.ts` (incl. `nil`-root stop), hover e2e, `eval:hover`.
  - AC3 variable kind + declaration site → `hover.test.ts`, hover e2e, `eval:hover`.
  - AC4 numeric literal radix/scaled-decimal decode → `hover.test.ts`, hover e2e, `eval:hover`.
  - AC5 Markdown with code fences → asserted across all the above.
  - Provenance prose gate → `comments.test.ts`, `kernelService.test.ts` (installed carries / bundled facts-only), `providers.test.ts` (workspace), workspace-prose e2e.
- [X] Each user-observable AC's acceptance test was **red before** implementation (module-not-found,
  then empty hover) and driven green.

## Section 2: Code Quality
- [X] `npm run lint` passes.
- [X] `npm run test:parser` (lexer/parser/symbols/kernel/cartridge/kernelService/providers/diagnostics/
  codeAction + **comments 6** + **hover 11**), `npm run test:server` (handshake incl. hover over the
  wire), `npm run test:e2e` (**21 passing**), `npm run eval` (completion + diagnostics + **hover 10/10**).
- [X] No new `any` types; resolvers are typed (`HoverContext`).
- [X] JSDoc on the new public surfaces (`hoverAt`/`HoverContext`, `extractComments`, service queries).

## Section 3: Constitutional Compliance
- [X] **Native**: standard `textDocument/hover` + Markdown `MarkupContent`; VS Code's built-in hover UI.
- [X] **Zero Config**: no new settings; reuses the resolved kernel tier + workspace index.
- [X] **Robustness**: provider returns `null` (never throws); chain walk uses a visited set + depth cap
  + `nil`-root stop; comment extraction is best-effort.
- [X] **TDD**: acceptance + unit tests written first (red → green).

## Section 4: Manual Verification
- [X] Installed-kernel prose path verified on the dev box (`auto` → installed): `Object`/`OrderedCollection`
  class comments and `printString` method comment surface in hover; bundled reference stays facts-only.
- [X] **Full manual-QA matrix — CLEARED (owner, 2026-06-24).** All rows passed in the Extension Host via
  **[`manual-qa-workspace/`](./manual-qa-workspace/README.md)**: Part A (content per symbol kind, AC1–AC5),
  Part B (prose gated by provenance — `auto`/`bundled`/`off`), Part C (robustness + live edit), Part D
  (clean VSIX). The pass caught + fixed two issues: a radix-float **grammar coloring** bug (`16rFF.`
  swallowed the period) and a false robustness-fixture claim (`16r.` is not malformed) — both fixed and
  re-verified.
- [X] No errors in Developer Tools console during the manual-QA pass.

## Section 5: Sign-Off
- [X] **PO accepted** (owner, 2026-06-24) — manual-QA matrix cleared.
- [ ] **Ready for Merge** — pending only commit/push + CI green on Linux/macOS/Windows. Code complete,
  green locally across all four gates, and manually verified.
