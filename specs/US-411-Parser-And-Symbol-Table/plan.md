# Implementation Plan: Parser and symbol table

**ID**: US-411 | **Date**: 2026-06-20 | **Spec**: ./spec.md | **Branch**: `feature/US-411-parser-and-symbol-table`

## Summary
Deliver the compiler front end as four slices (lexer → expression parser → GST containers →
symbol table). **This plan covers slice 1: the lexer (AC1).** A pure, never-throws `tokenize()`
in `server/src/parser/` turns a Smalltalk source string into a token stream (with byte offsets +
LSP positions) plus a diagnostics list, fully unit-tested with `tsx`. No `vscode` imports, no
`gst`, no parser/AST work.

## Approach
Hand-written single-pass scanner, mirroring the `char_table` dispatch from
`docs/research/gst-syntax/01-*`. Files:

- **`server/src/parser/token.ts`** — `TokenKind` enum, `Position {line,character}` (0-based,
  UTF-16, LSP-shaped), `Token {kind,text,start,end,startPos,endPos}`, and an LSP-free
  `LexDiagnostic {message,severity,start,end,startPos,endPos}`. No external imports.
- **`server/src/parser/lexer.ts`** — `tokenize(source): { tokens, diagnostics }`. A cursor tracks
  `pos`/`line`/`character`; `advance()` updates line/character (handles `\n`, `\r`, `\r\n`). Main
  loop skips whitespace, snapshots the start position, dispatches on the current char, and emits a
  token. **Every branch advances ≥1 char**, so the scanner cannot loop or hang; nothing throws.
- **`server/test/lexer.test.ts`** — `node:assert`/`tsx`, same idiom as `client/test/gstLocator.test.ts`:
  focused unit assertions + golden token snapshots over fixtures `01–05,15`.
- **`server/test/__snapshots__/NN_*.tokens.txt`** — committed golden token streams.
- **`package.json`** — add `"test:parser": "tsx server/test/lexer.test.ts"`; the test file accepts
  `--update` to regenerate snapshots (mirrors the grammar harness convention).

**Key tokenization decisions (recorded so snapshots are intentional):**
- **Leading sign is not part of a number.** `-456` → `BinarySelector('-')`, `Integer('456')`;
  the parser folds unary minus into negative literals in slice 2. Exponent signs (`1.0e-2`,
  `123e-1`) *are* consumed inside the float. (Matches GST `scan_number`, which reads digits;
  `-` is a binary-op char.)
- **`|` is always its own `Pipe` token**, never merged into a multi-char binary selector (GST
  returns the `|` char). Binary selectors scan a maximal run of `% & * + , - / < = > ? @ \ ~`.
- **`#(`, `#[`, `#{` are dedicated combined tokens** (`HashParen`/`HashBracket`/`HashBrace`) and
  `{` is `LBrace`. This intentionally departs from GST emitting `#`+`(` as separate tokens — a
  single token lets the slice-2 parser branch without lookahead. Bare `#` → `Hash`.
- **Keyword vs scope vs assignment at a colon:** after an identifier, a following `:` makes a
  `Keyword` **unless** the next char is `=` (→ `Identifier` then `Assign :=`) or `:` (→
  `Identifier` then `Scope ::`), so `Smalltalk::Foo` and `x:=1` lex correctly.
- **`_` at token start is `Assign`** (identifiers never start with `_`); `_` inside an identifier
  or after `$` is an ordinary character.
- **Strings and comments treat the doubled delimiter (`''`, `""`) as an escape**; an unterminated
  string/comment yields a token spanning to EOF **plus** a diagnostic (no throw).
- **Char literals**: `$x` (any single char incl. `$`, space, brackets) and the code-point form
  `$<65>` / `$<16r42>`; a bare `$<` not followed by a digit is the character `<`.
- **Shebang**: only when the file begins with `#!` — the first line becomes one `Shebang` token.

## Steps
1. `token.ts` — token model + diagnostic shape (no imports).
2. `lexer.ts` — cursor + `tokenize()`: whitespace/positions, comments, strings, chars, numbers
   (decimal + radix + float + scaled, underscores, exponents), identifiers/keywords, symbols,
   binary operators, assignment/scope/colon, structural tokens, shebang, chunk `!`, error
   recovery, trailing `EOF`.
3. `server/test/lexer.test.ts` — unit assertions for each category + the edge cases
   (`123e.`, `16rF.Fd`, `1.2s2`, `''` escape, all symbol forms, `$<65>`, `::`/`:=`/`:`,
   `#(`/`#[`/`#{`/`{`), recovery tests (unterminated string/comment, stray byte, garbage input →
   no throw, `EOF` present, diagnostic emitted), and snapshot checks over `01–05,15`.
4. Generate snapshots (`--update`), eyeball them against the fixtures.
5. Add `test:parser` script; run `check-types`, `lint`, `test:parser` green.

## Dependencies & Risks
- **Inputs**: blueprint `01-*`; fixtures `test-cases/01–05,15.st`. No new runtime deps.
- **Number-literal subtlety** (radix floats `16rF.Fd`, scaled `16rA.Bs2`, `123e.`): lex
  conservatively, consume the longest valid prefix, never past a valid literal. Token *kind* and
  *boundaries* are the goal — value evaluation is out of scope for the lexer.
- **Position correctness** under CRLF and non-ASCII: covered by a dedicated unit test.
- **Infinite-loop risk**: every branch advances; a garbage-input test asserts termination + `EOF`.
- **Snapshot churn** tied to `TokenKind` shape: snapshots serialize `kind + text + start offset`
  only, with `--update` for intentional changes.

## Verification
- `npm run test:parser` — unit + recovery + snapshot tests pass.
- `npm run check-types` and `npm run lint` clean (strict TS, eslint).
- Slice exit: lexer-relevant fixtures snapshot cleanly with no `Error` token on well-formed input.
  (Full-story `npm run eval`, kernel smoke test, and `verification.md` are completed at the end of
  the story, after the symbol-table slice.)

---

# Slice 2 — Expression parser (AC2)

**Status:** in progress (slice 1 merged in PR #31). Branch `feature/US-411-expression-parser`.

## Summary
A recursive-descent parser over the slice-1 token stream that produces an **AST with positions**
for the ANSI expression/statement core: literals & primaries, message sends with
**unary > binary > keyword** precedence, assignment chains, cascades, and blocks (params +
temporaries + body). Like the lexer it **never throws** — on a malformed statement it records a
diagnostic, emits an `Error` node, and synchronizes to the next `.`/`]`/`!`/EOF, always returning
an AST.

## Scope refinement
- **In:** `parse(source) → { ast, diagnostics }`; `Program` = optional `| temps |` + statement
  sequence; statements (`^expr` / `expr`); assignment (right-assoc, `:=` and `_`); cascades;
  message precedence; blocks; primaries — variables, scalar literals, literal arrays `#(…)`, byte
  arrays `#[…]`, dynamic arrays `{…}`, parenthesized expressions.
- **Out (moved to slice 3):** **method patterns + `<primitive: …>`** — these only appear at a
  container boundary (chunk `!sel…!` / brace `Foo >> sel [ … ]`), which slice 3 introduces. Slice 2
  delivers the statement-sequence/expression core that method bodies reuse. (AC2 names method
  patterns; the spec stays the source of truth and this is a documented decomposition, re-confirmed
  in slice 3's plan.)

## Approach — files
- **`server/src/parser/ast.ts`** — `NodeKind` + node interfaces, all carrying `start/end` offsets
  and `startPos/endPos` (reusing `Position` from `token.ts`): `Program`, `Block`, `Return`,
  `Assignment`, `Message` (receiver, selector, arguments, `messageType`), `Cascade`, `Variable`,
  `Literal` (scalar), `LiteralArray`, `ByteArray`, `DynamicArray`, and `ErrorNode`.
- **`server/src/parser/parser.ts`** — `parse(source): { ast, diagnostics }`. Tokenizes via the
  lexer, drops `Comment` trivia, then a `Parser` walks the token array. Precedence climb:
  `parseExpression` (assignment) → `parseCascade` → `parseKeywordMessage` → `parseBinaryMessage`
  → `parseUnaryMessage` → `parsePrimary`.
- **`server/test/parser.test.ts`** + `__snapshots__/*.ast.txt` — unit assertions + AST snapshots
  over fixtures `06,07,08` (message sends, assignments/cascades, blocks).

## Key parsing decisions (recorded so snapshots are intentional)
- **Precedence**: unary binds tightest (a run of trailing `Identifier` sends), then binary
  (left-assoc run of `BinarySelector` — **and a lone `Pipe`** acting as the `|` binary selector,
  e.g. `true | false`), then keyword (one keyword message whose args are binary-level expressions).
- **Assignment** is right-associative and only when `Identifier` is immediately followed by
  `Assign`; `var1 := var2 := 100` nests. Covers both `:=` and `_`.
- **Cascades**: `recv msg ; msg ; …` — the cascade receiver is the *receiver of the first message
  send*; subsequent `;`-messages target it. A `Cascade` node holds `receiver` + `messages[]`.
- **Block header** (the subtle part): `params = (':' Identifier)*`; if params present, consume one
  `|` arg-terminator; then the body is a statement sequence that itself parses an optional
  `| temps |`. Temporaries are only committed when a **closing `|`** is confirmed by lookahead
  (`looksLikeTemporaries`), so `[ :a :b || a , b ]` (doubled-pipe separator, no temps) and
  `[ :x | | t | … ]` (separator then real temps) both parse — and `[ | | … ]` is empty temps.
- **Trivia**: `Comment` tokens are filtered before parsing; positions still come from real tokens.
- **Recovery**: `parsePrimary` on an unexpected token emits an `ErrorNode` + diagnostic and the
  statement loop synchronizes on `.`/`]`/`!`/EOF — never throws.

## Verification
- `npm run test:parser` covers lexer **and** parser suites (unit + AST snapshots for `06,07,08`).
- `check-types` + `lint` clean. Slice exit: fixtures `06,07,08` produce no `Error` node /
  diagnostic; recovery tests prove no-throw on malformed input.

---

# Slice 3 — GST containers (AC3)

**Status:** slice 3a (brace) in progress on `feature/US-411-gst-containers`. Slice 2 merged (#32).

## Scope split (kept reviewable)
AC3 names two container formats; landing both at once is unreviewably large, so:
- **Slice 3a (this PR): the GST brace format** — `Object subclass: #Foo [ … ]` class bodies,
  `Foo >> sel [ … ]` / `Foo class >> sel [ … ]` and short-form `sel [ … ]` method definitions,
  `obj extend [ … ]`, `Namespace current: #X [ … ]`, `Foo class [ … ]` class-side scopes, instance
  -variable declarations, `<…>` attributes/pragmas (incl. `<primitive: …>`), method patterns
  (unary/binary/keyword), and `Namespace::Class` scoped names. Snapshots over fixtures `11,12,13`.
- **Slice 3b (next): the GST chunk method format** (`!Class methodsFor: '…'! … ! !`) and the GST
  primaries `#{…}` binding / `##(…)` compile-time constants (fixture 14), plus the esoteric
  "nested receiver" definition blocks (`Namespace definition: [ name: … ]`, implicit-receiver
  keyword messages) and dotted-namespace paths (`A.B`) seen in fixture 11's tail.

## Approach — files & integration
The container layer is **layered over the slice-2 core** (the expression parser is untouched):
- **`ast.ts`** — new nodes `Definition` (with `definitionKind`: subclass | namespace | extend |
  classScope | scoped), `MethodDefinition` (target?, classSide, selector, messageType, params,
  pragmas, temps, statements), `Pragma`, `InstanceVariables`; `DynamicArray` gains `temporaries`.
- **`parser.ts`** — container recognition sits in `parseStatement` + a new `parseClassBody`:
  - **Method def** `Class [class] >> pattern [ body ]` detected by `looksLikeMethodDef` lookahead
    (so binary/keyword patterns like `>> + arg` don't break the expression parser).
  - **Scoped def** `<expr> [ body ]`: parse the expression; a **trailing `[`** (left after a
    complete keyword/unary message, vs a block that is a keyword arg) introduces a container body.
    `classifyDefiner` derives the kind/name from the definer's shape.
  - **Class-body items**: instance-var `| … |`, `<…>` pragmas, full/short method defs, nested
    defs, and statements — `.` optional between items.
  - **Short-form method** `pattern [ body ]` inside a body via `looksLikeShortMethodDef` (a leading
    `Keyword`, a `BinarySelector + Identifier`, or `Identifier` directly before `[`).
  - **Scoped names** `A::B` handled in `parsePrimary`.

## Key decisions
- Method defs are **not** `.`-separated from following items (GST grammar); the sequence loop
  skips the `.`-requirement for `Definition`/`MethodDefinition` nodes.
- Pragmas `<…>` are recognized only in body-leading / class-body position (a bare `<`), so `a < b`
  stays a binary message; pragma selector is built from its `Keyword` parts, args via `parsePrimary`
  (so the closing `>` is never consumed).
- The container logic is isolated behind clear seams (`parseMethodDefinition`, `parseScopedDefinition`,
  `parseClassBody`); the **chunk format plugs in alongside** in slice 3b without touching the core.

## Verification
- `server/test/container.test.ts` — unit tests for class/method/extend/namespace/class-scope defs,
  all method patterns, pragmas, and a no-throw sweep over fixtures `10–14`; AST snapshots over
  `11,12,13`. Shared `astDump.ts` serializer.
- Slice exit: fixtures `12,13` snapshot with **zero diagnostics**; fixture `11`'s brace core is
  clean — its only diagnostics are in the slice-3b tail (implicit-receiver `definition:` blocks).
