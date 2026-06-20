# Tasks: Parser and symbol table

**ID**: US-411 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Map tasks to acceptance criteria where possible.
Story delivered as four slices/PRs: **S1 lexer (AC1)** → S2 expression parser (AC2) →
S3 GST containers (AC3) → S4 symbol table + recovery (AC4, AC5).

## Phase 1 — Spec & Setup
- [x] T001 Spec authored; `requirements-validation.md` gate passed (PASS, slice 1 cleared).
- [x] T002 `plan.md` (slice-1 lexer) + `tasks.md` written.

## Phase 2 — Implementation (Slice 1: Lexer, AC1)
- [x] T010 `server/src/parser/token.ts` — `TokenKind`, `Position`, `Token`, `LexDiagnostic` (no imports). *(AC1.2)*
- [x] T011 `server/src/parser/lexer.ts` — cursor + `tokenize()` skeleton: whitespace skip, position tracking (`\n`/`\r`/`\r\n`), trailing `EOF`, never-throws main loop. *(AC1.1, AC1.2)*
- [x] T012 Comments (`"…"`, `""` escape) as `Comment` trivia; unterminated → token-to-EOF + diagnostic. *(AC1.7)*
- [x] T013 Strings (`'…'`, `''` escape); unterminated → token-to-EOF + diagnostic. *(AC1.4, AC1.7)*
- [x] T014 Char literals: `$x`, `$$`, `$<65>`, `$<16r42>`, bare `$<`. *(AC1.5)*
- [x] T015 Numbers: decimal/radix integers, `_` separators, floats (`.`frac, `e`/`d`/`q` + sign), scaled (`s`scale); `123e.` and radix-float edge cases. *(AC1.3)*
- [x] T016 Identifiers + keyword detection (colon vs `:=` vs `::`); pseudo-vars stay `Identifier`. *(AC1.1)*
- [x] T017 Symbols: identifier/keyword (`#at:put:`), quoted (`#'…'`), binary (`#+`, `#<=`). *(AC1.4)*
- [x] T018 Operators/structural: binary-selector runs, `|` Pipe, `:=`/`_` Assign, `::` Scope, `:` Colon, `( ) [ ] { } . ^ ;`, `#(`/`#[`/`#{` combined, bare `#`. *(AC1.6)*
- [x] T019 Shebang (`#!` at file start) and chunk `!` Bang. *(AC1.1, AC1.6)*
- [x] T020 Error recovery: stray char → `Error` token + diagnostic, always advances (no throw/loop). *(AC1.1, AC1.7)*

## Phase 3 — Tests (Slice 1)
- [x] T030 `server/test/lexer.test.ts` — per-category unit assertions + edge cases (`123e.`, `16rF.Fd`, `1.2s2`, `''`, all symbol forms, `$<65>`, `::`/`:=`/`:`, `#(`/`#[`/`#{`/`{`). *(AC1.3–AC1.6)*
- [x] T031 Recovery/robustness tests: unterminated string + comment, stray byte, random garbage → no throw, `EOF` present, diagnostic emitted. *(AC1.1, AC1.7)*
- [x] T032 Position test under CRLF + non-ASCII. *(AC1.2)*
- [x] T033 Snapshot tests over fixtures `01–05,15` with `--update`; commit goldens under `server/test/__snapshots__/`. *(AC1)*
- [x] T034 Add `"test:parser"` script; wire into CI alongside `test:client`/`test:server`.

## Phase 4 — Verify (Slice 1)
- [x] T040 `npm run check-types`, `npm run lint`, `npm run test:parser` all green.
- [ ] T041 Open slice-1 PR (links #23); lexer fixtures snapshot cleanly, no `Error` on well-formed input.

## Later slices (tracked here, planned per-slice when started)
- [ ] S2 Expression parser (AC2): `ast.ts`, `parser.ts` (unary>binary>keyword, cascades, blocks, statements).
- [ ] S3 GST containers (AC3): `Container` interface, brace + chunk plug-ins, method patterns + primitives.
- [ ] S4 Symbol table + recovery (AC4, AC5): `symbolTable.ts`, synchronization, kernel smoke test.

## Story-level Done (after S4)
- [ ] T900 Output eval / tests pass (`npm run eval` + AST/symbol snapshots + kernel smoke test).
- [ ] T901 `verification.md` gate passed.
- [ ] T902 CI green on Linux/macOS/Windows.
