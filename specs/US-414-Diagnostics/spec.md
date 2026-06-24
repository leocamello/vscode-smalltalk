# Specification: Diagnostics

**ID**: US-414
**Feature**: Diagnostics (parser live; `gst` opt-in)
**Status**: Implemented (Slices A–C) — in release for v0.6.0
**Owner**: Leonardo Nascimento
**Created**: 2026-06-23
**Epic**: EPIC-004 (Language Intelligence — TypeScript LSP) · **Milestone**: 0.6.0

## 1. Overview
Surface code problems in the editor without leaving it. Two independent tiers, per
[ADR-0001](../../docs/decisions/0001-typescript-bundled-lsp-server.md): an **always-on parser tier**
that publishes syntax diagnostics from the existing error-tolerant front end (US-411) as the user
types, and an **opt-in `gst` tier** (default off) that runs the real GNU Smalltalk compiler on save
and surfaces its parse errors. The parser tier is the headline value and works with **no `gst`
installed**; the `gst` tier is a second, more authoritative opinion when a runtime is present and
degrades cleanly to nothing when it is not.

## 2. Goals
- Publish parser syntax diagnostics on open/change, debounced (250 ms), respecting the parser-emitted
  severity (Error/Warning). Never block typing. **Badge:** VS Code renders a diagnostic as
  `source(code)`, so to display **`smalltalk(parse)`** we set `source: 'smalltalk'`, `code: 'parse'`.
- Add an opt-in setting `smalltalk.diagnostics.useGst` (default **false**) that runs `gst` on save
  (and via an on-demand **Smalltalk: Validate with gst** command), parses its stderr to ranges,
  source `gst`, code `smalltalk(gst)`.
- Guarantee no zombie `gst` processes: bounded timeout + kill-on-edit/kill-on-resave; in-flight runs
  are superseded.
- Offer trivial quick-fix code actions for the cheap, unambiguous cases: insert a missing `]` or `)`.
- Keep all three test layers + the eval harness green; add a new `evals/datasets/diagnostics/` dataset.

## 3. Non-Goals
- **Semantic analysis from `gst`.** GNU Smalltalk is dynamic — undeclared variables print `nil`, not
  a compile error. The `gst` tier yields essentially *parse/syntax* diagnostics, a second opinion on
  the parser tier, not type/DNU checking. (Cross-reference/unknown-selector intelligence is US-423/SPIKE-01.)
- **Column-precise `gst` ranges.** gst 3.2.5 stderr is `<file>:<LINE>: <message>` — line only, no
  column, no severity (see §6). `gst` diagnostics are whole-line ranges.
- gst-on-*change* (live) execution. The `gst` tier is save/command-triggered only.
- Diagnostics for non-`.st`/`.gst` documents; multi-file/project compile.
- Code actions beyond missing `]`/`)` (e.g. insert missing `.`, rename fixes) — deferred.

## 4. User Stories & Acceptance Criteria
**US-414**: As a **Smalltalk developer**, I want **error squiggles as I type, plus optional real
compile errors from `gst`**, so that **I catch mistakes without leaving the editor.**

- **AC1**: Parser diagnostics (syntax errors/warnings) are published on open and on change, debounced
  (250 ms), with code `smalltalk(parse)` and source `smalltalk`, severity as the parser emits it.
  Closing a document clears its diagnostics. Diagnostics never block typing.
- **AC2**: An opt-in setting `smalltalk.diagnostics.useGst` (default off) runs `gst` on save and via a
  **Smalltalk: Validate with gst** command, parses stderr (`<file>:<LINE>: <message>`), maps each to a
  whole-line range, tags source `gst` and code `smalltalk(gst)`. When `gst` is not resolvable the tier
  is silently inert (ADR-0001 — degrades to nothing); the parser tier is unaffected.
- **AC3**: `gst` processes time out (bounded) and are killed on the next edit or re-run; no zombie
  processes accumulate under rapid edits/saves. The parser tier is never blocked by the `gst` tier.
- **AC4**: Trivial code actions are offered where cheap: a quick fix to insert a missing `]` or `)`
  when the parse diagnostic is the corresponding "expected `]`/`)`".

## 5. Technical Design

### 5.1 Diagnostic mapping (shared)
A new `server/src/providers/diagnostics.ts` converts the LSP-free `LexDiagnostic`
(`server/src/parser/token.ts` — `message`, `severity: DiagnosticSeverity`, `start/end`,
`startPos/endPos`) to a `vscode-languageserver` `Diagnostic`: map `DiagnosticSeverity.Error|Warning`
→ LSP `DiagnosticSeverity`, `{startPos,endPos}` → `Range`, `source: 'smalltalk'`, `code: 'parse'`
(renders as `smalltalk(parse)`). Pure + unit-tested. The `gst` tier reuses the same range mapping with
`source: 'gst'` (Slice B).

### 5.2 Parser tier (Slice A — AC1)
- Extend `parseCache.ts`: store `diagnostics` on the cache entry (the `parse()` result already returns
  them — today they are dropped) and expose `getDiagnostics(doc)`. No second parse.
- `server.ts`: on `documents.onDidOpen` and `onDidChangeContent`, schedule a **dedicated** diagnostics
  timer (250 ms, mirroring `INDEX_DEBOUNCE_MS`, separate map) → `connection.sendDiagnostics({uri,
  diagnostics})`. Publish empty on `onDidClose` to clear. Cancel a document's pending timer on close.
- Advertise nothing new at init — push diagnostics are unsolicited (no capability flag needed); but we
  keep the existing `textDocumentSync` (Incremental). Front end never throws → empty list on failure.

### 5.3 `gst` tier (Slice B — AC2/AC3)
- Spawn lives **server-side** (the server owns `sendDiagnostics` and already receives `gnuSmalltalkPath`
  via `configureKernel`). New `server/src/gst/gstRunner.ts`: `child_process.execFile(gstPath,
  [filePath], { timeout })` with a kill on supersede; pure stderr→`Diagnostic[]` parsing extracted as
  `parseGstStderr(text, uri)` (unit-tested against the verified gst-3.2.5 format).
- Trigger: `documents.onDidSave` (the server gets these when the client sends `didSave`) when
  `smalltalk.diagnostics.useGst` is true, plus a client command **`smalltalk.validateWithGst`** that
  asks the server (custom request/notification) to run the tier on the active document.
- No-zombie discipline (US-301 lineage): one in-flight child per uri; a new edit/save kills the prior
  child (`child.kill()`), the timeout bounds runaway runs, and results from a superseded run are dropped.
- Merge model: parser and gst diagnostics are published to the **same** uri. We keep them distinct by
  source/code and republish the union (parser tier from cache + last gst result) so neither clobbers
  the other.
- Resolution reuse: factor the existing `resolveGst`/`gstLocator` logic so the server can resolve the
  executable the same way the client does (configured path wins, else PATH). Tests stay hermetic by
  injecting the probe (see [[dev-box-has-gst-installed]] discipline).

### 5.4 Code actions (Slice C — AC4)
- Advertise `codeActionProvider` in `onInitialize`. New `server/src/providers/codeAction.ts`: for a
  parser diagnostic whose message is `Expected "]"`/`Expected ")"`, offer a `QuickFix` `WorkspaceEdit`
  inserting the delimiter at the diagnostic range **start** — i.e. *before* the unexpected token the
  parser tripped on (inserting at the range *end* lands the `)` after that token and does **not** fix
  the parse — found in manual QA). When the error-tolerant parser reports several missing closers at
  the same spot (nested unclosed `[`/`(`, e.g. an unclosed method body *and* class), they are grouped
  into a single action that inserts all of them (`]]`), so one click fully closes them. Pure mapping,
  unit-tested (incl. "applying the fix re-parses clean"); e2e asserts the fix.

### 5.5 Settings & commands (package.json)
- `smalltalk.diagnostics.useGst`: boolean, default `false`, scope `resource`.
- Command `smalltalk.validateWithGst` (title "Smalltalk: Validate with gst"), `when` editorLangId ==
  smalltalk; client handler forwards to the server.

## 6. Risks & Limitations
- **gst stderr format is line-only.** Verified on this box (gst 3.2.5): `bad.st:4: parse error,
  expected ']'`, `e2.st:2: expected object`, `e3.st:2: Unterminated string, attempting recovery`,
  `e4.st:2: invalid class body element`. Pattern: `^(.+?):(\d+):\s*(.+)$` — **no column, no `error:`
  prefix, no severity** (the AC's `file.st:LINE: error: ...` was an assumption; corrected here). All
  gst diagnostics are Error severity, whole-line range. Some messages are recovery notes
  ("attempting recovery"), which we still surface as Errors.
- **Tier overlap.** Parser and gst will often flag the same syntax error at slightly different ranges;
  acceptable — distinct sources make the origin clear, and gst is the authoritative second opinion.
- **Error-tolerant parser noise.** Half-typed code transiently parses as broken; the 250 ms debounce
  plus respecting parser severity (it already distinguishes Error vs Warning) keeps flicker low.
- **gst availability is per-machine.** The tier is opt-in and silently inert without a resolvable gst;
  CI/eval must not depend on gst being installed (the gst-stderr unit tests parse fixture strings).
- **Save-only gst** means stale gst squiggles between save and the edit that supersedes them; we clear
  gst diagnostics on the first change after a save to avoid showing errors against edited text.
