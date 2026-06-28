# Specification: Formatting

**ID**: US-416
**Feature**: `textDocument/formatting` + `rangeFormatting` + `onTypeFormatting` — conservative, idempotent GNU Smalltalk code formatting over the US-411 lexer/AST, **offline, no `gst`**
**Status**: Draft
**Owner**: Leonardo Nascimento
**Created**: 2026-06-27
**Epic**: EPIC-004 (Language Intelligence — TypeScript LSP) → ~1.0
**Architecture**: builds on US-411 (lexer/parser/AST); new ADR-0005 (formatter = whitespace-only token-stream rewriter, not an AST pretty-printer)

## 1. Overview
Normalize the *layout* of GNU Smalltalk source — indentation, inter-token spacing, blank-line runs,
cascade alignment, and long keyword-message wrapping — **without ever changing the code itself**. The
formatter is a **whitespace-only token-stream rewriter**: it keeps every significant token and every
comment verbatim (byte-for-byte) and rewrites only the whitespace *between* tokens, using the US-411 AST
to inform structural depth (block nesting, cascade segments, keyword-message arguments). This design makes
the two hard guarantees — **idempotence** (`format(format(x)) === format(x)`) and **token-stream
invariance** (the lexer emits the identical significant-token sequence before and after) — structural
rather than aspirational, which is the whole point: for a formatter, *data loss = trust loss*. It ships
**off by default** (`smalltalk.format.enable`) and works with no `gst`.

## 2. Goals
- `textDocument/formatting` (whole document) — whitespace-normalize the file.
- `textDocument/rangeFormatting` — the same engine clamped to the selected range (snapped to whole lines /
  statement boundaries) so only the selection's lines are re-emitted.
- `textDocument/onTypeFormatting` — live, local layout assists: indent the new line after `[`, dedent the
  line on `]`, and keep cascade segments aligned, triggered on `]`, newline, and `;`.
- **Conservative + canonical layout**: indentation by block depth; one space around binary selectors and
  after a keyword colon; no space before `.`/`;`; collapse 2+ blank lines to 1; **align cascade segments**;
  **wrap long keyword messages** one-keyword-per-line past a width threshold.
- **Optional expanded layout** (`blockStyle: expand`): reflow method/class/multi-statement-block bodies
  onto their own indented lines, one statement per line (single-statement argument blocks stay inline) —
  achieved with structural forced-breaks, **still whitespace-only** (no AST re-synthesis). Default
  `preserve` keeps the author's block layout.
- **Idempotence** and **token-stream invariance** as property-tested invariants (AC3).
- Default-off behind `smalltalk.format.*` config; richer knobs (`indentSize`, `cascades`, `keywordWrap`).
- Works with **no `gst`**; an output-eval dataset `evals/datasets/formatting/`.

## 3. Non-Goals
- **No AST pretty-printing / source re-synthesis.** We never rebuild text from nodes; we only rewrite
  whitespace between preserved tokens (ADR-0005) — *including* the opt-in `blockStyle: expand` aesthetic,
  which is structural forced-breaks over the existing tokens, not re-synthesis. This is what guarantees AC3.
- **No semantic rewrites**: no reordering statements, no renaming, no removing/adding parentheses, no
  touching string/comment/symbol-literal *contents*, no normalizing number radixes — purely layout.
- **No formatting of unparseable input**: if the file (or, for range formatting, the selected span) does
  not parse cleanly — **any** parse diagnostic — it is left **byte-for-byte untouched** (no edits). v1 is
  whole-input skip, not partial/per-method recovery; never reformat around an `Error` node. (Partial
  per-region formatting is a possible future enhancement — see §7.)
- Not a linter or style-warning surface (that would be US-414's lane); the formatter is silent.
- Does not replace `editor.formatOnSave`/`formatOnType` — it *registers a provider* those features call.

## 4. User Stories & Acceptance Criteria
**US-416**: As a **Smalltalk developer**, I want **conservative, idempotent code formatting**, so that **I
can normalize style without risking my code.**

- **AC1**: `rangeFormatting` + `onTypeFormatting` ship: on-type indents the new line after `[`, dedents the
  line on `]`, and keeps cascade segments aligned; rangeFormatting reformats exactly the selected lines.
- **AC2**: `documentFormatting` whitespace-normalizes the whole file from the token stream (informed by the
  AST), **preserving every comment and intentional blank line**, normalizing only indentation, inter-token
  spacing, blank-line runs, cascade alignment, and keyword-message wrapping.
- **AC3**: Property tests hold over the corpus + generated inputs: `format(format(x)) === format(x)`
  (idempotence) **and** the significant-token stream (kinds + text, comments included, whitespace excluded)
  is **identical** before and after formatting (no token added, dropped, split, or merged).
- **AC4**: Formatting is **off by default** (`smalltalk.format.enable: false`) for at least one release;
  enabling it is required for any edits to be produced.
- **AC5**: Works with **no `gst`**; a malformed file produces **no edits** for the unparseable region (never
  corrupts code); output-eval dataset `evals/datasets/formatting/` passes in CI on Linux/macOS/Windows.

### Acceptance scenarios (Given/When/Then)
- **Given** a file with `foo:=Account new.` **When** I format the document **Then** it becomes
  `foo := Account new.` and re-formatting it again yields no further changes.
- **Given** a cascade `foo bar; baz; quux.` **When** I format **Then** segments align one-per-line under the
  receiver (cascades=align), and the lexer token stream is unchanged.
- **Given** a keyword send longer than `keywordWrap` columns **When** I format **Then** each keyword part
  starts a new, indented line; a short one is left on one line.
- **Given** a file with a syntax error anywhere **When** I format the document **Then** no edits are
  produced and the file is left byte-for-byte untouched (whole-file skip in v1).
- **Given** `smalltalk.format.enable` is `false` **When** I invoke Format Document **Then** zero edits are
  returned.
- **Given** I type `]` to close a block **When** onTypeFormatting fires **Then** that line is dedented to its
  block's level (no other line changes).

### Edge cases
- Empty file / whitespace-only file → no edits.
- File ending without a trailing newline → preserve the author's final-newline choice (don't add/remove).
- CRLF vs LF → preserve the document's prevailing EOL; never mix.
- Mixed tabs/spaces in the original indent → normalized per `indentSize` + `editor.insertSpaces`.
- Comments mid-expression / trailing on a line → kept attached to their line, never moved across tokens.
- GST chunk (`!`) / container (`Foo extend [ … ]`) and class/method bodies → indented by their nesting.
- Pragmas (`<primitive: 80>`), literal arrays, byte arrays, dynamic arrays → spacing normalized, contents
  preserved.

### Dependencies
- US-411 lexer (`server/src/parser/lexer.ts`, `token.ts`) — significant-token stream incl. comments.
- US-411 parser/AST (`parser.ts`, `ast.ts`) + `parseCache` for structural depth.
- VS Code formatting APIs (the client already uses `vscode-languageclient`; server advertises the providers).

## 5. Technical Design
### Engine — whitespace-only token-stream rewriter (ADR-0005)
- New module `server/src/format/` with the pure core `formatter.ts` (no `vscode` import; runs under `tsx`):
  - Input: source text + the US-411 token stream (incl. comment tokens) + the AST (for structural depth).
  - Output: a new string built by walking tokens in order and emitting **each token's exact text** with
    **computed whitespace** between consecutive tokens. Because token texts are copied verbatim and the
    sequence is never reordered, the significant-token stream is invariant by construction (AC3).
  - **Indentation**: a depth counter driven by the AST/bracket structure (block `[`, container `[`,
    method body, paren/brace groups). Each logical line begins with `indentUnit × depth`.
  - **Inter-token spacing rules** (the "conservative + canonical" set): one space around binary selectors,
    one space after a keyword `:`/before its argument, none before `.`/`;`/`)`/`]`/`}`, none after
    `(`/`[`/`{`, one space between a unary receiver and selector, etc. A small spacing table keyed by
    `(prevKind, nextKind)`.
  - **Blank-line runs**: collapse 2+ consecutive blank lines to 1; preserve a single blank line as an
    intentional separator.
  - **Cascade alignment** (`cascades: align`): each `;`-segment starts a new line indented one unit under
    the cascade receiver. `cascades: preserve` leaves author line breaks (spacing still normalized).
  - **Keyword-message wrapping** (`keywordWrap: <columns>`): if a keyword send's single-line rendering
    would exceed the threshold, emit each keyword part on its own continuation line, indented one unit;
    otherwise keep it inline.
- **Idempotence (AC3)**: guaranteed by deriving all whitespace from a normal form (depth + spacing table +
  wrap decision computed from the *normalized* width). A property test asserts the fixed point.

### Providers + wiring
- `server/src/providers/formatting.ts`: `formatDocument`, `formatRange`, `onType` — each returns
  `TextEdit[]`. Prefer **minimal diffs**: compute the formatted string, then emit per-line/replace-range
  edits (or a single full-document replace for whole-doc; range = replace the clamped line span). Reuse
  `parseCache` for tokens+AST.
- `server.ts`: advertise `documentFormattingProvider`, `documentRangeFormattingProvider`, and
  `documentOnTypeFormattingProvider: { firstTriggerCharacter: ']', moreTriggerCharacter: ['\n', ';'] }`;
  gate every handler on `smalltalk.format.enable` (pulled in `didChangeConfiguration`, per the config
  gotcha — read in handler, not only at init). When disabled → return `[]`.
- **Config** (`package.json` `contributes.configuration`, `smalltalk.format.*`):
  - `enable`: boolean, default **false** (AC4).
  - `indentSize`: number, default `4` (tabs vs spaces follow `editor.insertSpaces`/`editor.tabSize`,
    passed in `FormattingOptions`).
  - `cascades`: `"align" | "preserve"`, default `"align"`.
  - `keywordWrap`: number (column threshold), default `100` (`0` disables wrapping).
  - `blockStyle`: `"preserve" | "expand"`, default `"preserve"`. `expand` adds AST-derived structural
    forced-breaks (break after a body `[`, one statement per line, `]` on its own line; single-statement
    argument blocks stay inline) — the "expanded" aesthetic **without** re-synthesizing source, so the AC3
    invariants still hold by construction (verified idempotent + token-invariant over the kernel corpus in
    both modes).
- **Robustness**: front end never throws; on any parse gap the provider formats only clean spans and
  returns no edits for the rest (Non-Goal §3). Malformed file → `[]`.

### Testing strategy
- **Property tests (unit, `server/test/`)**: idempotence + token-stream invariance over (a) the
  122-file kernel corpus and (b) hand-picked + lightly fuzzed inputs. This is the AC3 anchor and the
  primary safety net — routed to unit (internal invariant).
- **Output eval (`evals/datasets/formatting/`)**: input `.st` → golden formatted `.st`, run by
  `npm run eval` on all three OSes (canonical-layout examples: cascade align, keyword wrap, blank-line
  collapse, spacing, nested blocks).
- **Handshake (`server/test/handshake.test.mjs`)**: assert the three formatting capabilities are
  advertised and that `formatting`/`rangeFormatting` return edits when enabled, `[]` when disabled.
- **e2e (`client/test-e2e/US-416.acceptance.test.js`)**: user-observable ACs — Format Document changes the
  buffer when enabled and is a no-op when disabled (AC4); range formatting touches only the selection;
  on-type dedents on `]`.

## 6. Manual Verification
Extension Development Host (`specs/US-416-Formatting/manual-qa-workspace/`): with `format.enable` on, run
Format Document / Format Selection / type to trigger on-type on a messy fixture; confirm comments + blank
lines survive, cascades align, a long keyword send wraps, a file with a deliberate syntax error is left
intact, and a second format is a no-op. With `enable` off, confirm Format Document does nothing. Matrix in
`verification.md`.

## 7. Risks & Limitations
- **Data loss is the cardinal risk** → mitigated structurally: whitespace-only rewrite + token-invariance
  property test as a CI gate; no AST re-synthesis (ADR-0005).
- **Comment placement**: a comment between two tokens stays between those two tokens; we do not try to
  "re-flow" comment prose. Acceptable and safe; documented.
- **Partial parses**: the error-tolerant parser may produce `Error` nodes / diagnostics; v1 **skips the
  whole input** when any diagnostic is present rather than guess at region boundaries — formatting is
  best-effort, never destructive. Per-method partial formatting (format the clean methods, leave the broken
  one) is a deliberate future enhancement, not shipped here.
- **onType scope**: kept deliberately local (the just-typed line + its block level) to stay fast and
  unsurprising; not a full reflow.
- **Opinionation**: cascade-align + keyword-wrap are canonical-but-opinionated; `cascades`/`keywordWrap`
  give an escape hatch, and the whole feature is off by default for a release (AC4) to gather feedback.
- Range formatting snaps to whole lines / statement starts to avoid mid-expression token spacing surprises.
