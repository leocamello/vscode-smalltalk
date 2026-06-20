# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins
**Type**: Requirements Quality Gate
**Story**: US-411 — Error-tolerant lexer + parser + symbol table

---

## Section 1: Constitution Gates (Mandatory)
- [x] **Native Look & Feel**: Front end produces LSP-shaped data (positions/ranges) consumed by standard providers in US-412+; no bespoke UI here.
- [x] **Zero Config**: Pure in-process parser; no settings, no `gst`, nothing for the user to configure.
- [x] **Protocol First**: Output is designed for LSP (token/AST/symbol ranges as `{line,character}`); LSP `Diagnostic`/`Range` mapping deferred to the provider boundary, by design.
- [x] **Robustness**: Error handling is the headline requirement — never throws, recovers via synchronization points, always returns an AST + diagnostics (AC4); lexer-level recovery specified for slice 1 (AC1.7, §6).
- [x] **Dialect Agnostic**: Dialect-neutral ANSI core with **pluggable** GST container formats (AC3, §5); other dialects plug in without forking the core.

## Section 2: Specification Completeness
- [x] Goals and Non-Goals explicitly listed (§2/§3; Tonel, semantic analysis, LSP wiring, `gst`, incremental reparse all excluded).
- [x] User stories in standard format (§4).
- [x] Acceptance scenarios (Given/When/Then) defined (§4, story-level and slice-1 level).
- [x] Edge cases identified: `123e.` vs `123e2`, `16rF.Fd`, `16rA.Bs2`, `''` escape, all symbol forms, `$<65>`, `::`/`:=`/`:`, `#(`/`#[`/`#{`/`{`, unterminated string/comment, stray bytes, CRLF + non-ASCII positions (§4.1, §6).
- [x] Dependencies listed: blueprints `01-*`/`02-*`, 15 `test-cases/*.st`, kernel corpus `../smalltalk-3.2.5/kernel/`; no new runtime deps (pure TS, `tsx` for tests).

## Section 3: Technical Design
- [x] API/Command contracts defined: `tokenize(source) → { tokens, diagnostics }` (slice 1); `parse`/symbol-table contracts sketched for later slices (§5).
- [x] Data structures defined: `TokenKind`, `Token` (offsets + LSP positions), `LexDiagnostic` (slice 1); AST/symbol-table shapes outlined for slices 2–4 (§5).
- [x] Error handling strategy defined: lexer emits `Error` tokens + diagnostics and always advances (no infinite loop, no throw); parser synchronizes on `.`/`!`/`]`/method-pattern starts (§5/§6).
- [x] Testing strategy defined: `tsx` unit + golden token snapshots over fixtures `01–05,15` via new `test:parser` script; mutation/garbage-input recovery tests; kernel smoke test gating US-412+ (slice 4).

## Section 4: Validation Result
- [x] PASS - Ready for implementation

**Scope note**: This story is one spec, four implementation slices/PRs (lexer → expression parser
→ GST containers → symbol table). Slice 1 (lexer, AC1) is cleared to implement; AC2–AC5 remain
Draft-validated and will be re-confirmed as each slice is planned in `plan.md`/`tasks.md`.
