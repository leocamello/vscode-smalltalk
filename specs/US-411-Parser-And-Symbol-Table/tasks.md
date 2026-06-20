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

## Phase 5 — Slice 2: Expression parser (AC2) — branch `feature/US-411-expression-parser`
- [x] T050 `server/src/parser/ast.ts` — `NodeKind` + node interfaces with offsets + LSP positions (`Program`, `Block`, `Return`, `Assignment`, `Message`, `Cascade`, `CascadeReceiver`, `Variable`, `Literal`, `LiteralArray`, `ByteArray`, `DynamicArray`, `ErrorNode`).
- [x] T051 `server/src/parser/parser.ts` — `parse()`: tokenize, drop `Comment` trivia, `Program` = `| temps |` (incl. interspersed REPL redeclarations) + statement sequence; never-throws scaffold with synchronization on `.`/`]`/`!`/EOF.
- [x] T052 Primaries: variables, scalar literals, **negative numeric literals** (folded `-` from the lexer), `#(…)` literal array, `#[…]` byte array, `{…}` dynamic array, `(…)` parenthesized. *(AC2)*
- [x] T053 Message precedence: unary run → binary run (incl. lone `|`) → single keyword message. *(AC2)*
- [x] T054 Assignment (right-assoc, `:=`/`_`) and `^` return statements. *(AC2)*
- [x] T055 Cascades: receiver = first send's receiver; segments are full message **chains** rooted at a `CascadeReceiver` marker. *(AC2)*
- [x] T056 Blocks: params `(':' id)*`, arg-terminator `|`, `| temps |` with `looksLikeTemporaries` lookahead (handles `||` separator and empty `| |`), body sequence. *(AC2)*
- [x] T057 `server/test/parser.test.ts` — unit assertions per construct + recovery tests (malformed statement → `ErrorNode` + diagnostic, no throw) + AST snapshots over fixtures `06,07,08`; `run.ts` runs lexer+parser suites under `test:parser`.
- [x] T058 `check-types`, `lint`, `test:parser` green; open slice-2 PR (links #23).

## Phase 6 — Slice 3a: GST brace container (AC3) — branch `feature/US-411-gst-containers`
- [x] T060 `ast.ts` — `Definition`, `MethodDefinition`, `Pragma`, `InstanceVariables` nodes; `DynamicArray.temporaries`.
- [x] T061 Method definitions: `Class >> sel`, `Class class >> sel` (full) + `sel [ … ]` (short) with unary/binary/keyword patterns; `looksLikeMethodDef`/`looksLikeShortMethodDef`. *(AC3)*
- [x] T062 Scoped definitions: `<expr> [ body ]` via trailing-`[`; `classifyDefiner` → subclass/namespace/extend/classScope; class/namespace name extraction. *(AC3)*
- [x] T063 Class body: instance-var decls, nested defs, statements; `.` optional between items. *(AC3)*
- [x] T064 Pragmas/attributes `<…>` (incl. `<primitive: …>`) in class/method bodies; `A::B` scoped names; `{ | t | … }` array-constructor temps. *(AC3)*
- [x] T065 `server/test/container.test.ts` + shared `astDump.ts` — unit tests + no-throw sweep over `10–14` + AST snapshots `11,12,13`.
- [x] T066 `check-types`, `lint`, `test:parser` green; open slice-3a PR (links #23).

## Phase 7 — Slice 3b: GST chunk container + GST primaries (AC3, remainder) — branch `feature/US-411-gst-chunk-primaries`
- [x] T070 Chunk method format `Class [class] methodsFor: '…'! method ! … ! !` → `Definition` (`methodsFor`) with `MethodDefinition` bodies; section ends on the empty `! !` chunk or the first doit chunk (`chunkLooksLikeMethod` heuristic, validated on fixture 05's loose style). *(AC3)*
- [x] T071 GST primaries: `#{…}` binding constants (`BindingConstant`), `##(…)` compile-time constants (`CompileTimeConstant`); fixture 14 now parses with **zero diagnostics**. *(AC3)*
- [x] T072 `server/test/chunk.test.ts` — unit tests + no-throw sweep over `05/10/14` + AST snapshot `14`.
- [x] T073 `check-types`, `lint`, `test:parser` green; open slice-3b PR (links #23).
- [ ] S3b-NOTE (documented limitation, not blocking AC3): implicit-receiver `definition: [ name: … ]` blocks and dotted-namespace `A.B` paths (fixture 11 tail) remain unsupported — not named by any AC; revisit if a feature needs them.

## Later slices (tracked here, planned per-slice when started)
- [ ] S4 Symbol table + recovery (AC4, AC5): `symbolTable.ts`, synchronization, kernel smoke test.

## Story-level Done (after S4)
- [ ] T900 Output eval / tests pass (`npm run eval` + AST/symbol snapshots + kernel smoke test).
- [ ] T901 `verification.md` gate passed.
- [ ] T902 CI green on Linux/macOS/Windows.
