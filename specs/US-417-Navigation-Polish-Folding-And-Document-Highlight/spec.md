# Specification: Navigation Polish — Semantic Folding + Document Highlight

**ID**: US-417
**Feature**: `foldingRange` + `documentHighlight` LSP providers
**Status**: Draft
**Owner**: Leonardo Nascimento
**Created**: 2026-06-20

## 1. Overview
A small **0.4.x** polish follow-up to US-412: two more LSP providers over the existing US-411 front
end. **Semantic folding** (`textDocument/foldingRange`) lets the editor collapse exactly on class
bodies, method bodies, blocks, and multi-line comments — more precise than VS Code's default
indentation folding. **Document highlight** (`textDocument/documentHighlight`) highlights the other
occurrences of the symbol under the cursor within the current file. Both reuse the cached AST/token
stream (`parseCache`), need no `gst`, and are zero-config.

## 2. Goals
- `textDocument/foldingRange`: ranges for class/namespace bodies, method bodies, blocks, and
  multi-line comments, derived from the AST + token stream.
- `textDocument/documentHighlight`: occurrences of the symbol under the cursor in the file —
  **scope-aware**: a message selector → its sends; a variable → its same-name references within the
  nearest binding scope (block/method/program), or file-wide for a class/global reference.
- Reuse `parseCache`; no new analysis beyond walking the existing AST/tokens.
- Fully functional with no `gst`; no settings.

## 3. Non-Goals
- No rename/refactor (highlight is read-only occurrence marking, not edit).
- No cross-file analysis (both are single-document providers).
- No type-based resolution of *which* implementor a send targets (selector match only) — that stays
  with go-to-definition's "all candidates" model.
- No folding of import/region markers beyond what the language-configuration already provides.

## 4. User Stories & Acceptance Criteria
**US-417**: As a Smalltalk developer, I want semantic code folding and highlighting of a symbol's
other occurrences, so that navigating and reading a file feels as polished as the outline and
go-to-definition already do.

- **AC1** (`foldingRange`): returns fold ranges for class/namespace bodies, method bodies, blocks,
  and multi-line comments (only where they span ≥ 2 lines), from the US-411 AST + token stream — in
  addition to VS Code's default indentation/marker folding. Never throws on malformed input.
- **AC2** (`documentHighlight`): given a cursor position, returns the occurrences of that symbol in
  the file — **scope-aware**, not a naive same-text match:
  - On a **message selector** → all sends with the same selector (and arity shape).
  - On a **variable** → its same-name references bounded by the nearest enclosing scope that binds
    it (block params/temps, method params/temps); a reference with no local binding (class/global)
    matches file-wide.
  - The symbol under the cursor is included; ordering is stable.

**Acceptance scenarios**
- *Given* a brace-format class, *when* I collapse the class in the gutter, *then* the whole
  `[ … ]` body folds; method bodies and inner blocks fold independently; a multi-line `"…"` comment
  folds.
- *Given* the cursor on a method send `printNl`, *when* highlight runs, *then* every `printNl` send
  in the file is highlighted.
- *Given* the cursor on a block parameter `:x`, *when* highlight runs, *then* only the uses of that
  `x` inside its block are highlighted — not an unrelated `x` in another method.

## 5. Technical Design
Two pure providers under `server/src/providers/`, mapped at the server boundary; both read
`getSymbols`/the AST via a small addition to `parseCache` (expose the parsed `ast`, or add
`getAst(doc)` alongside `getSymbols`).

- **`providers/foldingRange.ts`** — `toFoldingRanges(ast, comments): FoldingRange[]`:
  - Walk the AST; for `Definition`, `MethodDefinition`, and `Block` nodes whose `startPos.line <
    endPos.line`, emit `{ startLine, endLine }` (end clamped to the last content line, per LSP).
  - From the token stream, emit `kind: 'comment'` ranges for multi-line `Comment` tokens.
  - Generic AST walk (reuse the `visit` helper pattern from `definition.ts`).
- **`providers/documentHighlight.ts`** — `documentHighlightsAt(ast, offset): DocumentHighlight[]`:
  - Reuse the `definition.ts` cursor resolver to classify the target (selector vs variable name).
  - **Selector** → collect every `Message` node with that selector; highlight the selector span.
    (Needs a selector range; either store it on `Message` in slice 2, or recompute from tokens.)
  - **Variable** → find the nearest enclosing scope (`Block`/`MethodDefinition`/`Program`) that
    declares the name (params/temporaries); collect same-name `Variable` nodes within it; if no
    binding is found, collect file-wide. Mark each `DocumentHighlightKind.Text` (Write on the
    assignment target/declaration if cheap).
- **`server.ts`** — advertise `foldingRangeProvider` + `documentHighlightProvider`; register
  `onFoldingRanges` / `onDocumentHighlight` reading the cache.
- **Note**: the `Message` node currently stores `selector` as a string but not the selector token's
  range; slice 2 adds a `selectorStart/selectorEnd` (or recomputes from the token stream) so the
  highlight lands on the selector, not the whole send. This is the one small parser/AST touch.

**Testing**: unit (tsx) for `toFoldingRanges` (per construct + multi-line comment) and
`documentHighlightsAt` (selector match; scoped variable; no cross-scope bleed) over fixture strings;
extend the real-server LSP test (`handshake.test.mjs`) with a `foldingRange` + `documentHighlight`
request; an Electron e2e row each. Manual spot-check in the Extension Host.

## 6. Risks & Limitations
- **Selector range fidelity** — keyword sends span multiple tokens (`at: … put: …`); the highlight
  should cover the keyword parts. v1 may highlight the whole send range if per-part ranges are not
  stored; refine if needed. (Risk → slice 2.)
- **Scope resolution depth** — full ANSI scoping (shadowing, nested blocks) is more than "near-free".
  v1 uses the nearest binding scope; document any shadowing edge cases as known limits.
- **Folding `endLine` semantics** — LSP folds hide lines *after* `startLine` through `endLine`;
  off-by-one on the closing `]`/`!` looks wrong. Covered by unit tests on exact line numbers.
- **Scope creep** — this is polish; if `documentHighlight` scoping balloons, ship `foldingRange`
  (slice 1) alone and split highlight to its own follow-up.
