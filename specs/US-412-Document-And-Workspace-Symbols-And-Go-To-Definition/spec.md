# Specification: Document/Workspace Symbols + Go-To-Definition

**ID**: US-412
**Feature**: Navigation — outline, workspace symbol search, go-to-definition
**Status**: Draft
**Owner**: Leonardo Nascimento
**Created**: 2026-06-20

## 1. Overview
The first **user-visible language-intelligence** feature, and the headline of **0.4.0**. It wires the
US-411 front end (lexer → parser → `buildSymbolTable`) into three LSP providers — **document
symbols** (outline/breadcrumbs), **workspace symbol search** (`Ctrl/Cmd+T`), and **go-to-definition**
(`F12`) — so a developer can navigate a GNU Smalltalk codebase without an external tool and
**without `gst`**. No new program analysis is invented: the symbol tree, with LSP-shaped ranges, is
already produced by US-411; this story maps it to LSP types, indexes it across the workspace, and
keeps it fresh with a debounced parse cache.

Because this is the **first feature users actually see and click**, the verification bar is
**manual QA in a real editor, on real code**, in addition to the automated unit/integration tests —
see §6, which is the gate for shipping 0.4.0.

## 2. Goals
- `textDocument/documentSymbol`: a class → (instance/class methods, variables) hierarchy for both
  brace- and chunk-format files, mapped from `buildSymbolTable`.
- `workspace/symbol`: find classes and selectors across the workspace (`**/*.{st,gst}`, respecting
  `files.exclude`).
- `textDocument/definition`: from a class reference → its definition(s); from a message send → **all**
  implementor candidates (Smalltalk is dynamically typed — return every match, ordered stably).
- A **debounced, version-keyed parse cache** so results update as files change without reparsing on
  every keystroke; a workspace index built lazily and updated incrementally.
- Fully functional with **no `gst`**; zero configuration.
- A **manual verification plan** (§6) executed before 0.4.0 ships.

## 3. Non-Goals
- No completion (US-413), diagnostics surfaced to the editor (US-414), hover (US-415), or formatting
  (US-416).
- No type inference or true sender/implementor *resolution* — definition returns candidate
  implementors by selector, not a single resolved target.
- No cross-file *semantic* linking beyond name/selector matching (no inheritance walk).
- No changes to the parser/symbol table beyond additive helpers (US-411 behavior is frozen).
- `foldingRange`/`documentHighlight` are out of scope unless trivially free (tracked as a stretch).

## 4. User Stories & Acceptance Criteria
**US-412**: As a Smalltalk developer, I want an outline, workspace symbol search, and
go-to-definition, so that I can navigate a codebase quickly without an external tool.

- **AC1**: `textDocument/documentSymbol` returns a hierarchy (class → methods/variables) for both
  chunk- and brace-format files, with correct kinds and ranges.
- **AC2**: `workspace/symbol` finds classes and selectors across the workspace (scans
  `**/*.{st,gst}`, respects `files.exclude`), matching by substring/fuzzy query.
- **AC3**: `textDocument/definition` jumps from a class reference to its definition(s) and from a
  message send to implementor candidates — returning **all** matches as a `Location[]`.
- **AC4**: Results update as files change, backed by a debounced, version-keyed parse cache (no
  reparse storm while typing).

**Acceptance scenarios**
- *Given* a brace-format class file, *when* I open the Outline, *then* I see the class with its
  instance and class-side methods (selector + arity) and instance/class variables nested under it.
- *Given* a chunk-format file (`!Class methodsFor: '…'! … ! !`), *when* I open the Outline, *then*
  the methods appear under their class.
- *Given* the cursor on a class name, *when* I invoke Go to Definition, *then* the editor jumps to
  the `subclass:` definition; if several files define/extend it, I get a candidate list.
- *Given* the cursor on a unary/keyword message send, *when* I invoke Go to Definition, *then* I get
  every method whose selector matches, across the workspace.
- *Given* I edit a file, *when* I re-query within ~300 ms, *then* results reflect the edit; rapid
  typing does not pin the CPU.
- *Given* no `gst` on `PATH`, *when* I use any of the above, *then* they work unchanged.

## 5. Technical Design
**Reuse, don't rebuild.** All three providers consume `buildSymbolTable(parse(text).ast)` from
US-411. New code is the LSP boundary + indexing.

- **`server/src/documents/parseCache.ts`** — `getSymbols(doc)`: parse + `buildSymbolTable`, memoized
  by `(uri, version)`; invalidated on change. A short **debounce** (~250–300 ms) coalesces edits.
- **`server/src/providers/documentSymbol.ts`** — `SymbolNode → DocumentSymbol`: map our `SymbolKind`
  to LSP `SymbolKind` (Class→Class, Method→Method, InstanceVariable/ClassVariable→Field,
  Namespace→Namespace, Temporary→Variable), passing `range`/`selectionRange` straight through (already
  `{line,character}`), preserving the child hierarchy.
- **`server/src/providers/workspaceSymbol.ts`** — a lazily built **workspace index**: enumerate
  `**/*.{st,gst}` under the workspace folders (Node `fs`/glob), parse each, flatten the symbol tree to
  `WorkspaceSymbol` (class + selector entries with their `Location`), respect `files.exclude`. Updated
  incrementally on `didChange`/`didCreate`/`didDelete`.
- **`server/src/providers/definition.ts`** — at the request position, find the token/identifier:
  a **class reference** → index entries of kind Class with that name; a **selector** (unary identifier
  or keyword message) → all Method entries with that selector. Return `Location[]` (all candidates).
- **`server/src/server.ts`** — advertise `documentSymbolProvider`, `workspaceSymbolProvider`,
  `definitionProvider`; register `onDocumentSymbol`/`onWorkspaceSymbol`/`onDefinition`; keep
  incremental `textDocumentSync`.
- **Client** (`client/`) — already starts the server for `language: smalltalk`; no new UI. The Outline
  view, breadcrumbs, `Ctrl/Cmd+T`, and `F12` are standard VS Code surfaces that light up once the
  server advertises the capabilities.

**Testing strategy.** (a) **Unit**: symbol→LSP mapping and definition/selector resolution over fixture
strings + the `test-cases/*.st` files (extends the `test:parser`-style suites). (b) **Integration**:
`@vscode/test-cli`/`@vscode/test-electron` against a small fixture workspace, asserting real
`documentSymbol`/`workspace/symbol`/`definition` results end-to-end. (c) **Manual**: §6.

## 6. Manual Verification Plan (gate for 0.4.0)
Automated tests prove the providers return the right data; **manual QA proves the feature feels right
in the editor, on real code.** This runs in the **Extension Development Host** (F5) and must pass
before 0.4.0 is tagged. The full matrix lives in `verification.md`; the dimensions:

1. **Real-corpus workspace** — open `../smalltalk-3.2.5/kernel/` (122 real files) as the test
   workspace. Outline, `Ctrl/Cmd+T`, and `F12` are exercised against genuine GNU Smalltalk, not just
   fixtures. The single strongest manual test.
2. **Outline / breadcrumbs** — brace file *and* chunk file: classes, instance vs class-side methods,
   variables; selectors show arity; nesting is correct; clicking an entry navigates.
3. **Workspace symbol search** — query a class name and a selector; results span multiple files;
   selecting one opens the right location; `files.exclude` honored.
4. **Go-to-definition** — class reference jumps to the definition; a message send offers all
   implementors (peek list when >1); `Ctrl/Cmd+Click` and `F12` both work.
5. **Live editing** — type a new method/class; within ~300 ms the outline/search reflect it; rapid
   typing doesn't spike CPU or flash errors.
6. **Robustness in the editor** — a half-typed/malformed file still yields a usable (partial) outline
   and never throws; a large file responds promptly.
7. **No-`gst` & zero-config** — with `gst` absent and no settings, everything above works.
8. **Cross-platform sanity** — exercise on the dev OS; CI covers Linux/macOS/Windows builds, but the
   reviewer confirms no platform-specific path/URI glitches in navigation.
9. **Clean-install smoke** — `npm run package` → install the VSIX in a clean VS Code → repeat the
   headline checks (outline + go-to-def) to confirm the shipped artifact behaves like the dev build.

Each row is recorded with a short note (and screenshot where useful) in `verification.md`. The
`verify`/`run` skills may be used to drive the Extension Host.

## 7. Risks & Limitations
- **Workspace scan cost** on large trees — mitigate with lazy/incremental indexing, `files.exclude`,
  and a file-count/size guard; parsing is fast (kernel: 122 files instantly in tests).
- **Definition over-matching** — by design (dynamic dispatch) we return all selector implementors;
  the risk is noise. Mitigate with stable ordering (same-file first) and the peek UI; document the
  semantics so it reads as a feature, not a surprise.
- **Cache/index staleness** — version-keyed cache + `didChange`/`didCreate`/`didDelete` handlers;
  covered by the live-editing manual row.
- **Manual-QA drift** — the matrix lives in `verification.md` and is gated by the PR template; the
  real-corpus workspace keeps it honest.
- **Scope creep** — completion/diagnostics/hover stay out (US-413+).
