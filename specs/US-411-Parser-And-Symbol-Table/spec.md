# Specification: Parser and symbol table

**ID**: US-411
**Feature**: Error-tolerant Smalltalk lexer + parser + symbol table
**Status**: Implemented (shipped behind v0.4.0; internal milestone M3)
**Owner**: Leonardo Nascimento
**Created**: 2026-06-20

## 1. Overview
Build the **compiler front-end** that every language-intelligence feature (US-412..416) depends
on: a hand-written, **error-tolerant lexer + recursive-descent parser** that produces an **AST
with source positions**, plus a **per-document symbol table**. It runs inside the bundled
TypeScript language server (per [ADR-0001](../../docs/decisions/0001-typescript-bundled-lsp-server.md))
with **no `gst` dependency**, and is built as a *pure, side-effect-free* module so it can be unit
tested in Node via `tsx`. Because editor buffers are constantly in a half-typed state, the front
end **must never throw**: on malformed input it recovers, emits error nodes plus a diagnostics
list, and always returns a usable AST/symbol table. The parser is **layered** — a dialect-neutral
ANSI core with **pluggable GST container formats** (brace classes and chunk file-outs) — so other
dialects can plug in later without forking the core.

This is an **XL** story delivered as **four reviewable slices / PRs**, each with its own tests:

1. **Lexer** (this slice — AC1): tokenizer + token model + diagnostics.
2. **Expression parser** (AC2): ANSI primaries, message precedence, cascades, blocks, statements.
3. **GST containers** (AC3): brace (`subclass: Foo [ … ]`, `Foo class >> sel [ … ]`) and chunk
   (`!Foo methodsFor: '…'! … ! !`) formats over the core, with method patterns + primitives.
4. **Symbol table + recovery hardening** (AC4, AC5): scopes/symbols, synchronization points, and
   the kernel-corpus smoke test.

The story is not Done until all four land, the snapshot/mutation tests pass, and **every
`../smalltalk-3.2.5/kernel/*.st` parses with zero crashes and a bounded error count**.

## 2. Goals
- A `Token` model carrying kind, text, and **both** byte offsets and LSP line/character positions,
  so downstream features get ranges for free.
- A lexer covering the **full GST token inventory** in `docs/research/gst-syntax/01-*`.
- A recursive-descent parser covering ANSI expressions with correct **unary > binary > keyword**
  precedence, cascades, blocks, method patterns, and primitives, per `…/02-*`.
- **Pluggable container layer**: the GST brace and chunk formats are recognized over the shared
  ANSI core, keeping the core dialect-neutral.
- A parser that **never throws** — it synchronizes on `.`, `!`, `]`, and method-pattern starts,
  emitting error nodes + diagnostics, and always returns an AST.
- A **symbol table** recording classes, methods (selector + arity), instance/class/temporary
  variables, and their scopes.
- Pure modules unit-testable with `tsx`; snapshot tests reuse the 15 `test-cases/*.st` categories;
  a kernel-corpus smoke test proves robustness.

## 3. Non-Goals
- **No LSP feature wiring** (document symbols, definition, completion, diagnostics surfaced to the
  editor, hover, formatting) — those are US-412..416. This story delivers the data, not providers.
- **No Tonel** or other package formats; scope is ANSI + the two GST container formats.
- **No semantic analysis** beyond the symbol table (no type inference, no cross-file resolution,
  no method-lookup/inheritance resolution).
- **No `gst` invocation**; the front end is pure TypeScript with zero runtime dependencies.
- **No incremental/streaming reparse** — full-document parse per change is acceptable for now.
- **No change to the TextMate grammar** or its snapshot harness.

## 4. User Stories & Acceptance Criteria
**US-411**: As a developer, I want a hand-written, error-tolerant lexer + recursive-descent parser
producing an AST with positions and a per-document symbol table, so that all downstream LSP
features have accurate semantic data even while code is being edited.

- **AC1 (Lexer — slice 1)**: The lexer covers the GST token inventory — radix/scaled/float
  numbers, `$c` and `$<n>` chars, strings (with `''` escape), symbols (`#sym`, `#'…'`, `#+`,
  `#kw:wd:`), the multi-token literal starts `#(`, `#[`, `{`, `#{`, scoped names (`::`),
  assignment (`:=` and `_`), shebang (`#!`), and the chunk bang (`!`) — per
  `docs/research/gst-syntax/01-*`. It is pure, never throws, and emits a diagnostic (not an
  exception) for stray characters and unterminated strings/comments.
- **AC2 (Expression parser — slice 2)**: The parser covers ANSI expressions with
  **unary > binary > keyword** precedence, cascades, blocks (with block args and temporaries),
  method patterns, and primitives, per `docs/research/gst-syntax/02-*`.
- **AC3 (Containers — slice 3)**: Container formats are pluggable: GST **brace**
  (`Object subclass: Foo [ … ]`, `Foo class >> sel [ … ]`) and GST **chunk**
  (`!Foo methodsFor: '…'! … ! !`), layered over the dialect-neutral core.
- **AC4 (Recovery — slice 4)**: The parser **never throws**; it recovers by synchronizing on `.`,
  `!`, `]`, and method-pattern starts, emitting error nodes + a diagnostics list, and **always**
  returns an AST.
- **AC5 (Symbol table — slice 4)**: A symbol table records classes, methods (selector + arity),
  instance/class/temporary variables, and scopes.

**Acceptance scenarios**
- *Given* any of the 15 `test-cases/*.st` fixtures, *when* the lexer tokenizes it, *then* the
  token stream matches the committed snapshot for that category and contains no `Error` token for
  the well-formed fixtures.
- *Given* a fixture with an unterminated string or a stray character, *when* the lexer runs, *then*
  it returns a complete token stream ending in `EOF` plus a diagnostic at the offending position —
  and **does not throw**.
- *Given* every `../smalltalk-3.2.5/kernel/*.st`, *when* the full front end runs, *then* it returns
  for all files with zero crashes and a total error count within the agreed bound (slice 4).

### Slice 1 (this PR) — lexer ACs in detail
- **AC1.1**: `tokenize(source)` returns `{ tokens, diagnostics }`; `tokens` always ends with an
  `EOF` token; the function never throws for any input (including binary/garbage).
- **AC1.2**: Every token carries `kind`, `text`, `start`/`end` byte offsets, and `start`/`end` LSP
  `{ line, character }` (0-based, UTF-16) positions.
- **AC1.3**: Number lexing handles `123`, `-456`, `1_000_000`, radix (`2r1011`, `16rFF`,
  `36rSMALLTALK`, `16rCAFE_BABE`), floats (`1.0`, `1.23d`, `12.34E5`, `1.0q2`, `16rF.Fd`),
  scaled decimals (`1s`, `1.2s2`, `16rA.Bs2`), and the tricky `123e.` (no exponent digits) vs
  `123e2`.
- **AC1.4**: String lexing treats `''` as an escaped quote; symbol lexing handles identifier,
  keyword (`#at:put:`), quoted (`#'a b'`), and binary (`#+`, `#<=`) symbols.
- **AC1.5**: `$c`, `$$`, `$ ` (space), and `$<65>` char literals are single tokens.
- **AC1.6**: `#(`, `#[`, `#{`, and `{` produce the documented multi-token / dedicated starts so the
  parser can branch; `::`, `:=`, `_`, `#!`, and `!` are distinguished from `:`, `#`, and binary
  ops.
- **AC1.7**: Comments (`"…"`) are emitted as `Comment` trivia tokens; inter-token whitespace is
  skipped. Unterminated string/comment → a token spanning to EOF **plus** a diagnostic.

## 5. Technical Design
**Placement.** New pure modules under `server/src/parser/` (dialect-neutral core; container
plug-ins arrive in slice 3). No imports from `vscode`/`vscode-languageserver` in the core so it
stays unit-testable in plain Node; LSP `Range`/`Diagnostic` mapping lives at the provider boundary
in later stories. The parser is wired into `server.ts` only when US-412+ add providers.

**Slice 1 — lexer files**
- `server/src/parser/token.ts`
  - `enum TokenKind` — identifiers/keywords; `BinaryOp`; `Assign` (`:=`/`_`); `Scope` (`::`);
    number kinds (`Integer`, `Float`, `ScaledDecimal`) or a single `Number` kind with sub-flags
    (decided in implementation, recorded here); `String`, `Symbol`, `Char`; the literal-start /
    delimiter tokens (`LParen`/`RParen`, `LBracket`/`RBracket`, `LBrace`/`RBrace`, `HashParen`,
    `HashBracket`, `HashBrace`); `Hash`; `Period`, `Caret`, `Semicolon`, `Pipe`, `Bang`,
    `Shebang`; `Comment`; `Error`; `EOF`.
  - `interface Position { line: number; character: number }` (0-based, matches LSP).
  - `interface Token { kind: TokenKind; text: string; start: number; end: number; startPos: Position; endPos: Position }`.
  - `interface LexDiagnosticSeverity`/`interface LexDiagnostic { message: string; start: number; end: number; startPos: Position; endPos: Position }` — a minimal, LSP-free shape mapped to `vscode-languageserver` `Diagnostic` at the provider boundary later.
- `server/src/parser/lexer.ts`
  - `export function tokenize(source: string): { tokens: Token[]; diagnostics: LexDiagnostic[] }`.
  - A single-pass scanner over a cursor (offset + line/character tracked together; `\n`, `\r`, and
    `\r\n` all advance the line). Character dispatch mirrors `char_table` from `01-*`: letter →
    ident/keyword; digit (or `-`+digit in literal position is **not** the lexer's job — `-` is a
    `BinaryOp`, the parser folds sign) → `scanNumber`; `$` → char; `'` → string; `"` → comment;
    `#` → symbol / `#(` / `#[` / `#{` / `#!` / bare `Hash`; `:` → `::` / `:=`-tail / `Keyword`
    colon / bare colon; `_` → `Assign`; binary-op chars → `scanBinaryOp`; structural chars →
    their dedicated tokens; `!` → `Bang` (or `Shebang` only at file start with `#!`).
  - **Error tolerance**: stray/unknown char → emit an `Error` token (length ≥ 1, always advances to
    guarantee progress) + a diagnostic, then continue. Unterminated string/comment → token to EOF +
    diagnostic. The scanner is structurally incapable of an infinite loop (every branch advances).
- `server/test/lexer.test.ts` — same `tsx`/`node:assert` idiom as `client/test/gstLocator.test.ts`:
  - Focused unit assertions per category (numbers incl. `123e.`/`16rF.Fd`/`1.2s2`; strings with
    `''`; all four symbol forms; `$<65>`; `::` vs `:=` vs `:`; `#(`/`#[`/`#{`/`{`).
  - Recovery tests: unterminated string, unterminated comment, stray char → no throw, `EOF`
    present, diagnostic emitted at the right offset.
  - **Snapshot tests** over the lexer-relevant fixtures `01,02,03,04,05,15` (committed golden token
    streams; a `--update` flag regenerates them, mirroring the grammar harness convention).

**Test runner.** Add `"test:parser": "tsx server/test/lexer.test.ts"` to `package.json` scripts
(keeping `test:client` for the client). CI runs `test:client`, `test:server`, and `test:parser`.

**Later slices (design intent, not built in slice 1)**
- Slice 2: `ast.ts` (nodes with `start`/`end` ranges + an `Error` node kind), `parser.ts`
  (Pratt-style precedence climb for unary/binary/keyword; cascades; blocks; statements).
- Slice 3: a `Container` interface; `gstBraceContainer.ts` and `gstChunkContainer.ts` plug-ins that
  segment a document into class/method units and hand bodies to the core parser; method-pattern +
  `<primitive: …>` parsing.
- Slice 4: `symbolTable.ts` (classes, methods w/ selector+arity, ivars/cvars/temps, nested scopes
  with ranges); recovery hardening + the kernel smoke test (`test:kernel`), which gates US-412+.

## 6. Risks & Limitations
- **Number-literal ambiguity** (`123e` identifier-ish forms, radix floats like `16rF.Fd`, scaled
  `16rA.Bs2`) is the lexer's subtlest area. Mitigation: explicit fixtures from `01-*`, and lex
  conservatively (back off to the longest valid prefix; never consume past a valid literal).
- **Position correctness** under `\r\n` and multi-byte input. Mitigation: track line/character in
  the cursor; LSP characters are UTF-16 code units — assert on a fixture containing CRLF and a
  non-ASCII char.
- **Infinite-loop risk** in an error-tolerant scanner. Mitigation: every branch (including the
  error branch) advances the cursor by ≥1; a test feeds random/garbage bytes and asserts
  termination + `EOF`.
- **Snapshot churn**: token snapshots are sensitive to the `TokenKind` shape. Mitigation: keep
  snapshots minimal (kind + text + start offset), and provide `--update` for intentional changes.
- **Scope discipline**: this slice ships **only** the lexer; resist pulling parser/AST work
  forward. Parser, containers, and symbol table are separate PRs against this same spec.
