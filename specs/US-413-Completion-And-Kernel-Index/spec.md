# Specification: Completion + GNU Smalltalk Kernel Index

**ID**: US-413
**Feature**: Auto-completion (selectors, classes, variables) backed by a kernel-library index
**Status**: Draft
**Owner**: Leonardo Nascimento
**Created**: 2026-06-21
**Architecture**: [ADR-0002 — Kernel symbol sourcing](../../docs/decisions/0002-kernel-symbol-sourcing.md)

## 1. Overview
The headline of **0.5.0** and the direct answer to **issue #1** (autocompletion, open since 2019). It
adds `textDocument/completion` over the US-411 symbol table + US-412 workspace index, and introduces
**kernel-library symbols** (`Object`, `Collection`, `String`, …) that do not live in the user's files.

Per [ADR-0002](../../docs/decisions/0002-kernel-symbol-sourcing.md), kernel symbols come from an
**installed-first, bundled-fallback** precedence chain over a **dialect-neutral index model** fed by
**per-dialect source adapters**:

1. **Workspace** (US-412) — always indexed; the user's own code.
2. **Kernel tier** — one active source, chosen by `smalltalk.completion.kernelLibrary`
   (`auto | bundled | off`): `auto` indexes a discoverable GST install's kernel **live**, else falls
   back to a **bundled** reference index shipped in the VSIX.

This honours ADR-0001 (works with **no `gst`** — the bundle covers the first-run user) *and* gives
users with GST installed version-correct completions from their **actual** kernel. To keep the
experience honest, completion items carry **provenance** (workspace / installed / bundled-reference)
and a **status-bar item** shows the resolved kernel identity. Because GST ships its kernel as readable
`.st`, the live and bundled paths share one parser-based directory indexer — no binary-metadata reader
(unlike vscode-java/JDT or vscode-csharp/Roslyn).

## 2. Goals
- A reusable, `vscode`-free **`.st`-directory indexer** producing a **dialect-neutral kernel index**
  (`dialect`, `library`, `version`, `source` header; classes → superclass + instance/class selectors
  with arity). **Facts only** (LGPL-2.1: no method-comment prose / bodies).
- A **build-time generator** that runs the indexer over the bundled `../smalltalk-3.2.5/kernel/` and
  emits the committed `server/data/kernel-index.json` (deterministic output).
- **Live installed-kernel indexing**: discover a GST kernel directory and index it with the *same*
  indexer; otherwise fall back to the bundle.
- `textDocument/completion`: keyword selectors after a receiver; class names in head position;
  temp/instance/class variables from scope; multi-part keyword selectors inserted as snippets.
- **Ranking** workspace > installed-kernel > bundled-kernel; prefix + camel-hump matching.
- The setting `smalltalk.completion.kernelLibrary` (`auto | bundled | off`, default `auto`) +
  `smalltalk.completion.kernelPath` override.
- **Provenance** on items + a **status-bar** indicator of the active kernel source; a one-time notice
  on first bundle fallback.
- Fully functional with **no `gst`**; sensible zero-config defaults (`auto`).

## 3. Non-Goals
- No hover (US-415), diagnostics surfaced to the editor (US-414), or formatting (US-416).
- **No method-comment prose** in the index or completions (LGPL; revisited for hover in US-415).
- **No image-based dialect adapter** (Pharo/Squeak reflective export) and **no `smalltalk.dialect`
  axis** yet — deferred follow-ups (ADR-0002). The model/adapter seam is built to accept them.
- No type inference: selector completion is offered from the union of known selectors, not resolved by
  receiver type (Smalltalk is dynamically typed). Ranking, not resolution, manages noise.
- No "live query a running image" mode (possible later enhancement, ADR-0002).
- No changes to the US-411 parser/symbol-table behaviour beyond additive, read-only consumption.

## 4. User Stories & Acceptance Criteria
**US-413**: As a Smalltalk developer, I want auto-completion for selectors, class names, and
local/instance variables — including standard kernel-library selectors — so that I can write code
faster with fewer lookups.

- **AC1**: A build-time generator parses a GNU Smalltalk kernel source directory (bundled
  `../smalltalk-3.2.5/kernel/*.st`) with our own parser into `server/data/kernel-index.json` —
  dialect-neutral (`dialect`/`library`/`version`/`source` header; classes, superclass chains,
  selectors + arity), built on a **reusable `.st`-directory indexer**, **facts only**.
- **AC2**: Completion offers keyword/unary selectors after a receiver (workspace > installed-kernel >
  bundled-kernel ranking, prefix + camel-hump matching).
- **AC3**: Completion offers class names in expression-head position and temp/instance/class variables
  from the symbol-table scopes at the cursor.
- **AC4**: Multi-part keyword selectors insert as snippets (e.g. `at:put:` → `at:${1} put:${2}`).
- **AC5**: `smalltalk.completion.kernelLibrary` (`auto | bundled | off`, default `auto`) selects the
  kernel sourcing strategy; `smalltalk.completion.kernelPath` overrides GST kernel-directory discovery.
- **AC6**: When a GST install is discoverable (via `kernelPath`, the `smalltalk.gnuSmalltalkPath`
  prefix, or common locations) the kernel tier indexes its **actual** kernel sources live via the same
  indexer; otherwise it falls back to the bundle.
- **AC7**: Completion items carry **provenance** (workspace / installed / bundled-reference); a
  status-bar item shows the resolved kernel identity (e.g. `bundled (gst 3.2.5)` / `installed` /
  `off`), with a one-time notice on first fallback to the bundle.

**Acceptance scenarios**
- *Given* the bundled index, *when* it is regenerated from the same corpus, *then* the output is
  byte-identical (deterministic) and contains no comment/prose fields. *(AC1)*
- *Given* a receiver `aCollection`, *when* I type `aCollection ` and trigger completion, *then* I see
  kernel `Collection` selectors (`do:`, `detect:ifNone:`, …) and any workspace selectors, workspace
  ranked first, each labelled by provenance. *(AC2/AC7)*
- *Given* a method with a temp `total` and an instance var `count`, *when* I complete inside it, *then*
  both appear, plus in-scope class names. *(AC3)*
- *Given* I accept `at:put:`, *when* it inserts, *then* I get `at:${1} put:${2}` as a snippet. *(AC4)*
- *Given* `kernelLibrary = off`, *when* I complete, *then* only workspace symbols are offered. *(AC5)*
- *Given* a real GST install on PATH/`kernelPath`, *when* `auto` resolves, *then* completions come from
  the installed kernel and the status bar reads `installed`. *(AC6/AC7)*
- *Given* no GST install, *when* `auto` resolves, *then* the bundle is used, the status bar reads
  `bundled (gst 3.2.5)`, and a one-time notice explains it. *(AC6/AC7)*

## 5. Technical Design
**Reuse, don't rebuild.** The kernel index is "a workspace index over a kernel directory" — it reuses
`parse` + `buildSymbolTable` (US-411), exactly as the kernel smoke test already does over all 122 files
with 0 diagnostics. New code is the neutral model, the directory indexer, the generator, the runtime
resolution, and the completion/LSP boundary.

- **`server/src/kernel/model.ts`** — `vscode`-free types: `KernelSelector { selector, arity }`,
  `KernelClass { superclass?, instanceSelectors[], classSelectors[] }`, and `KernelIndexData` with a
  header `{ dialect, library, version, source }` + `classes` + derived counts. Provenance enum
  (`workspace | installed | bundled`) used by completion.
- **`server/src/kernel/indexer.ts`** — `indexKernelDirectory(dir, meta): KernelIndexData`. Walk `*.st`,
  `parse` → `buildSymbolTable`, collect every `Class` node (top-level and namespace/nested), merge by
  class name across files/chunks (first non-empty superclass wins; selectors unioned, split by
  `classSide`), and emit **deterministically** (classes sorted by name, selectors sorted). Pure Node
  `fs`, never throws (skips unreadable/parse-failing files). The *same* function serves the bundled
  generator (AC1) and the live installed path (AC6).
- **`scripts/gen-kernel-index.ts`** (run via `npm run gen:kernel-index`) — call the indexer on the GST
  kernel dir (default `../smalltalk-3.2.5/kernel`, arg-overridable), tag
  `{ dialect:'gst', library:'gst-3.2.5', version:'3.2.5', source:'bundled' }`, write
  `server/data/kernel-index.json` with stable 2-space formatting (no timestamp). The corpus is absent
  in CI, so the JSON is a **committed artifact**.
- **`server/src/kernel/kernelIndexService.ts`** *(Slice B)* — load the bundled JSON; discover an
  installed kernel dir (`kernelPath` → `gnuSmalltalkPath` prefix → common locations); resolve the
  active source per `kernelLibrary`; expose lookups (selectors by prefix, classes) tagged with
  provenance + the resolved identity. Re-resolves on `didChangeConfiguration`.
- **`server/src/providers/completion.ts`** *(Slice C)* — at the cursor, use `parseCache` tokens/AST +
  the symbol-table scope to decide context (receiver → selectors; head → classes + variables); merge
  workspace + kernel candidates; rank workspace > installed > bundled with prefix/camel-hump; map to
  `CompletionItem[]` with provenance in `detail`/label-description and keyword-selector snippets
  (`InsertTextFormat.Snippet`).
- **`server/src/server.ts`** — advertise `completionProvider` (trigger characters incl. space and
  `:`), wire `onCompletion`; pull `smalltalk.completion.*` in the config handler (the same dynamic
  `didChangeConfiguration` path US-412 uses).
- **Client / packaging** — bundle `server/data/kernel-index.json` so the server can read it at runtime
  (esbuild copy or `resolveJsonModule` import); a status-bar item *(Slice D)* shows the resolved kernel
  identity and opens the setting on click; one-time fallback notice.

**Slices (each a reviewable PR; all three test layers green per slice):**
- **A** — neutral model + reusable directory indexer + bundled generator + committed JSON +
  snapshot/licensing tests (AC1).
- **B** — kernel index service: load bundle, discover install, resolve `auto|bundled|off`, provenance
  (AC5/AC6) + unit tests.
- **C** — completion provider over workspace + kernel, ranking + keyword snippets + provenance
  (AC2–AC4, AC7 items) + server/e2e tests.
- **D** — settings in `package.json`, status-bar indicator + fallback notice (AC5/AC7 UX) + e2e.

**Testing strategy.** (a) **Unit (`test:parser`)**: an index snapshot/invariant test that runs in CI
**without** the corpus (loads the committed JSON; asserts known classes, sorted order, arity ≥ 0,
and structurally **no prose fields** = the licensing gate), plus a **drift guard** that regenerates
in-memory and compares when the corpus *is* present (mirrors `kernel.test.ts`'s skip-when-absent
pattern); completion-context unit tests at cursor positions; resolution/discovery tests with a temp
fixture kernel dir. (b) **Server (`test:server`)**: extend the handshake test to drive real
`textDocument/completion` and assert capability advertisement. (c) **E2E (`test:e2e`)**: a fixture
workspace asserting kernel + workspace completions end-to-end. (d) **Manual**: §6.

## 6. Manual Verification Plan (gate for 0.5.0)
Automated tests prove the index/providers return the right data; **manual QA proves completion feels
right in the editor, on real code.** Runs in the **Extension Development Host** (F5) and must pass
before 0.5.0 is tagged. Full matrix in `verification.md`; the dimensions:

1. **Real-corpus workspace** — open `../smalltalk-3.2.5/kernel/` (122 real files); complete kernel +
   workspace selectors against genuine GNU Smalltalk. The strongest single manual test.
2. **Selector completion** — after a receiver, kernel selectors appear; workspace selectors rank above
   kernel; prefix + camel-hump both match.
3. **Class & variable completion** — class names in head position; temps/instance/class vars in scope.
4. **Keyword snippet** — accepting `at:put:` inserts `at:${1} put:${2}` with working tab stops.
5. **Sourcing — installed** — with GST installed (or `kernelPath` set), `auto` uses the installed
   kernel; status bar reads `installed`; completions match that version.
6. **Sourcing — bundled fallback** — with no GST, `auto` falls back to the bundle; status bar reads
   `bundled (gst 3.2.5)`; one-time notice appears; items show `bundled-reference` provenance.
7. **`off`** — kernel completions suppressed; only workspace symbols remain.
8. **Provenance honesty** — a user can tell where any suggestion came from (item detail + status bar).
9. **Robustness / live editing** — malformed file still completes usefully and never throws; newly
   typed methods/classes appear; rapid typing doesn't spike CPU.
10. **No-`gst` & zero-config** — defaults give useful kernel completion with nothing installed.
11. **Cross-platform sanity** — dev OS + CI (Linux/macOS/Windows builds); no path/URI glitches in
    discovery.
12. **Clean-install smoke** — `npm run package` → install the VSIX in clean VS Code → repeat headline
    checks (selector + keyword-snippet + bundled fallback) to confirm the shipped artifact behaves.

## 7. Risks & Limitations
- **Licensing (LGPL-2.1)** — the index must carry *facts only*. Mitigation: the model has no prose
  fields and a test asserts their absence; the generator copies no comments/bodies.
- **Version/dialect confusion** — a bundled suggestion mistaken for guaranteed availability.
  Mitigation: provenance on items + ambient status bar + one-time fallback notice + `off`; core kernel
  API is highly stable across GST 3.x, so bundled facts are rarely wrong.
- **Selector over-offering** (no type inference) — noise. Mitigation: prefix/camel-hump filtering +
  workspace-first ranking; documented as expected for a dynamically typed language.
- **Determinism / drift** — committed JSON could drift from the corpus. Mitigation: deterministic
  generator + the corpus-present regeneration drift guard (skipped in CI where the corpus is absent).
- **Install discovery brittleness** across OSes/distros. Mitigation: explicit `kernelPath` override +
  layered discovery + always-safe bundle fallback; never throws.
- **Scope growth** (ADR-0002 added AC6/AC7) — managed by the A–D slicing; image dialects stay out.
