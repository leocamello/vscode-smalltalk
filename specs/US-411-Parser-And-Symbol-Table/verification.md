# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding
**Type**: Implementation Verification
**Story**: US-411 — Error-tolerant lexer + parser + symbol table (verified after slice 4)

---

## Section 1: Acceptance Criteria
- [x] All ACs in `tasks.md` are checked across the four slices.
  - **AC1 (lexer)** — slice 1 (#31): full GST token inventory, never-throws recovery.
  - **AC2 (expression parser)** — slice 2 (#32): unary>binary>keyword, cascades, blocks, assignment.
  - **AC3 (containers)** — slice 3a (#33) brace + slice 3b (#34) chunk; method patterns + primitives.
  - **AC4 (recovery)** — this slice: never throws; recovers on `.`/`!`/`]`/method starts; error nodes + diagnostics. Proven by the **kernel smoke test** (122 files, 0 crashes).
  - **AC5 (symbol table)** — this slice: classes, methods (selector + arity + side), instance/class/temporary variables, namespaces; merged per class.
- [x] Each AC has a passing test (lexer/parser/container/chunk/symbols unit + snapshot suites; kernel gate).

## Section 2: Code Quality
- [x] `npm run lint` passes (eslint, `server/src` strict).
- [x] `npm run test:parser` passes — lexer 26 + parser 19 + container 10 + chunk 7 + symbols 7 + kernel 1; `test:client` 6; `test:server` ok. `npm run check-types` clean (strict, `noUncheckedIndexedAccess`).
- [x] No `any` types — the AST/symbol model is fully typed discriminated unions.
- [x] JSDoc/TSDoc on the public surface (`tokenize`, `parse`, `buildSymbolTable`, node/symbol types).

## Section 3: Constitutional Compliance
- [x] **Native**: produces LSP-shaped data (offsets + `{line,character}` ranges) for US-412+ providers.
- [x] **Zero Config**: pure in-process front end; no settings, no `gst`.
- [x] **Robustness**: never throws — the kernel smoke test parses all 122 GNU Smalltalk 3.2.5 kernel files with **0 crashes**, **0 diagnostics** (all 122 fully clean), **206 classes** extracted.
- [x] **TDD**: tests authored with each slice; snapshots committed as golden output evals.

## Section 4: Manual Verification
- [ ] Feature works in Extension Host — **N/A**: the front end is internal (no provider wired yet); it is exercised by the unit/snapshot/kernel suites. LSP providers (US-412+) add the user-facing surface.
- [x] No errors in the test runner; deterministic snapshots.

## Section 5: Sign-Off
- [x] Ready for Merge (pending PO acceptance). With the kernel smoke test green, the **gate for US-412+ is satisfied**.

**Edge cases handled (beyond the ACs):** the GST dotted-namespace `A.B` path form (lexed as a
scope separator exactly like GST's `lex.c` `scan_ident`) and the implicit-receiver
`definition: [ name: … ]` block of messages — both previously the source of the kernel diagnostics,
now fully parsed (kernel corpus: 0 diagnostics).
