# Specification: Diagnostics

**ID**: US-414
**Feature**: Diagnostics (live parser tier)
**Status**: Implemented — in release for v0.6.0 (parser-only; `gst`/runtime tier **deferred to EPIC-007**)
**Owner**: Leonardo Nascimento
**Created**: 2026-06-23
**Epic**: EPIC-004 (Language Intelligence — TypeScript LSP) · **Milestone**: 0.6.0

## 1. Overview
Surface code problems in the editor without leaving it. An **always-on parser tier** publishes syntax
diagnostics from the existing error-tolerant front end (US-411) as the user types — column-precise,
live, and with trivial quick fixes — working with **no `gst` installed** (ADR-0001).

> **Scope decision (2026-06-24):** the original story also proposed an **opt-in `gst` tier** (AC2/AC3 —
> run the real compiler on save). It was built and tested, then **deferred from 0.6.0**: GNU Smalltalk
> 3.2.5 is dynamic and emits only *syntax* errors, which the parser already catches **more precisely
> (column-level), live (on change), and fixably**. The gst tier was therefore largely redundant,
> line-only, save-only, and the highest-risk surface (process lifecycle). Its genuine value —
> *semantic* compile errors (undeclared variable, unknown selector, DNU) — needs a **running runtime**,
> so it belongs with the **Live Bridge (EPIC-007)**, where it is non-redundant. See [§7](#7-deferred-gstruntime-compile-diagnostics-epic-007).

## 2. Goals
- Publish parser syntax diagnostics on open/change, debounced (250 ms), respecting the parser-emitted
  severity (Error/Warning). Never block typing. **Badge:** VS Code renders a diagnostic as
  `source(code)`, so to display **`smalltalk(parse)`** we set `source: 'smalltalk'`, `code: 'parse'`.
- Offer trivial quick-fix code actions for the cheap, unambiguous cases: insert a missing closer
  (`]`, `)`, `}`, `>`) or close an unterminated string.
- Keep all three test layers + the eval harness green; add a new `evals/datasets/diagnostics/` dataset.
- **No runtime dependency:** everything ships in the bundled server and needs no `gst`.

## 3. Non-Goals
- **Anything requiring a runtime** — opt-in `gst`/image compile diagnostics (deferred to EPIC-007, §7).
- **Semantic analysis** — undeclared variables, unknown selectors, arity/DNU. The parser is syntax-only;
  static cross-reference intelligence is US-423/SPIKE-01, runtime compile feedback is EPIC-007.
- Diagnostics for non-`.st`/`.gst` documents; multi-file/project compile.
- Code actions beyond inserting a missing closer (`]`/`)`/`}`/`>`) or a closing string `'` — e.g.
  inserting a missing `.`, closing an unterminated *comment*/character/symbol (ambiguous close
  position), or rename fixes — deferred.

## 4. User Stories & Acceptance Criteria
**US-414**: As a **Smalltalk developer**, I want **error squiggles as I type**, so that **I catch
mistakes without leaving the editor.**

- **AC1**: Parser diagnostics (syntax errors/warnings) are published on open and on change, debounced
  (250 ms), with code `smalltalk(parse)` (source `smalltalk`, code `parse`), severity as the parser
  emits it. Closing a document clears its diagnostics. Diagnostics never block typing.
- **AC4**: Trivial code actions are offered where cheap: a quick fix to insert a missing closer
  (`]`, `)`, `}`, `>`) when the parse diagnostic is the corresponding `Expected "X"`, and to insert
  the closing `'` for an `Unterminated string literal`. Applying a fix re-parses clean; non-fixable
  parse errors (e.g. a truncated expression) offer no action.
- **AC2/AC3 (opt-in `gst` compile errors + no-zombie process discipline)**: **deferred to EPIC-007**
  (§7). Recorded here for traceability; not in the 0.6.0 scope.

## 5. Technical Design

### 5.1 Diagnostic mapping
`server/src/providers/diagnostics.ts` converts the LSP-free `LexDiagnostic`
(`server/src/parser/token.ts` — `message`, `severity: DiagnosticSeverity`, `start/end`,
`startPos/endPos`) to a `vscode-languageserver` `Diagnostic`: map `DiagnosticSeverity.Error|Warning`
→ LSP `DiagnosticSeverity`, `{startPos,endPos}` → `Range`, `source: 'smalltalk'`, `code: 'parse'`
(renders as `smalltalk(parse)`). Pure + unit-tested.

### 5.2 Parser tier (Slice A — AC1)
- Extend `parseCache.ts`: store `diagnostics` on the cache entry (the `parse()` result already returns
  them — they were dropped) and expose `getDiagnostics(doc)`. No second parse.
- `server.ts`: on `onDidChangeContent` (fires on open + change), schedule a **dedicated** diagnostics
  timer (250 ms, mirroring `INDEX_DEBOUNCE_MS`, separate map) → `connection.sendDiagnostics`. Publish
  empty on `onDidClose` to clear + cancel the pending timer.
- Advertise nothing new at init — push diagnostics are unsolicited; `textDocumentSync` stays the simple
  `Incremental`. Front end never throws → empty list on failure.

### 5.3 Code actions (Slice C — AC4)
- Advertise `codeActionProvider` in `onInitialize`. `server/src/providers/codeAction.ts`
  (`toCodeActions(uri, diagnostics, text)`):
  - **Missing closer** — an anchored `^Expected "([\])}>])"` match (covers `]`, `)`, `}`, `>`; an
    anchored regex avoids the ambiguous `Expected "." or "}" in dynamic array` and never offers to
    insert an *opener*). The closer is inserted at the diagnostic range **start** — *before* the
    unexpected token (inserting at the range *end* lands the closer after that token and does **not**
    fix the parse — found in manual QA). Several closers reported at one spot (nested unclosed `[`/`(`,
    e.g. method body *and* class) are grouped into one action that inserts all of them (`]]`).
  - **Unterminated string** — insert the closing `'` at the **end of the line where the string
    opened** (needs the document `text`): closes the common single-line typo and preserves the code a
    multi-line swallow ran over (vs. closing at EOF).
  Pure mapping, unit-tested (each closer + string, incl. "applying the fix re-parses clean", and a
  non-fixable parse error offers nothing); e2e asserts the `]` fix clears the squiggle.
- **Diagnostic positioning (parser):** the `}` (dynamic array) and `>` (pragma) scans run past the
  natural close point (a `}` hits EOF; a `<…` pragma otherwise swallows the method body), so their
  `Expected …` diagnostics anchor at the **end of the last consumed token** (`parser.ts:diagAfterPrev`)
  rather than the unexpected current token — otherwise the squiggle *and* the quick fix land on a line
  below the defect (a manual-QA finding). The pragma scan also stops at a `^` return so it no longer
  consumes the body. `]`/`)` are unchanged (their current token is already the right place).

## 6. Risks & Limitations
- **Error-tolerant parser noise.** Half-typed code transiently parses as broken; the 250 ms debounce
  plus respecting parser severity (it already distinguishes Error vs Warning) keeps flicker low.
- **Syntax-only.** The parser tier cannot see semantic errors (undeclared/unknown selector). That is by
  design (§3) — the gap is filled later by US-423 (static) and EPIC-007 (runtime), not this story.
- **Zero-width closer squiggles.** The `}`/`>` diagnostics anchor as zero-width markers at the right
  spot (correct position over a wide underline); acceptable.

## 7. Deferred: `gst`/runtime compile diagnostics (EPIC-007)
The opt-in compile-diagnostics tier (original AC2/AC3) is **deferred to the Live Bridge (EPIC-007)**.
Rationale and the value thesis for reviving it:

- **Why not gst 3.2.5 file-parse now:** it is dynamic and emits only *syntax* errors, which the parser
  tier already catches more precisely (column-level), live (on change), and fixably. As a syntax tier
  it was redundant, line-only (`<file>:<LINE>: <message>`, no column), save-only, not fixable, and the
  highest-risk surface (process spawn/timeout/kill-on-edit/no-zombie).
- **The real value is semantic, and needs a runtime:** undeclared variables, unknown selectors, DNU,
  arity — these require *compiling/loading into an image*, exactly what the **Live Bridge** provides.
  There the tier is **non-redundant** (it reports what the static front end fundamentally cannot) and
  the process lifecycle is justified by genuine new capability.
- **What the deferred work already established** (kept in git history on `feature/US-414-diagnostics`):
  a server-side runner with no-zombie discipline (one in-flight child per uri, timeout,
  kill-on-supersede), stderr→`Diagnostic` parsing, server-side `gst` resolution, a `useGst` setting and
  a *Validate* command, and the union/merge publishing model. These are the bones of an EPIC-007
  "runtime diagnostics" provider, to be generalized beyond gst-3.2.5 to any Live Bridge backend.
- **Tracking:** the EPIC-007 backlog carries this as runtime compile/semantic diagnostics (see
  `docs/product/user-stories.md` / `docs/ROADMAP.md`).
