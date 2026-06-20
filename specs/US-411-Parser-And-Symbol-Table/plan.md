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
