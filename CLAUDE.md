# CLAUDE.md

Project brief for AI agents working in this repo. Read [`CONTRIBUTING.md`](CONTRIBUTING.md)
for the full process and [`docs/ROADMAP.md`](docs/ROADMAP.md) for where we're headed.

## What this is
`vscode-smalltalk` — a VS Code extension for **GNU Smalltalk** (`.st`/`.gst`). Published on the
Marketplace as `leocamello.vscode-smalltalk`.

## Current status (2026-06-27)
> **Direction (the end goal):** the language server is evolving into a dialect-agnostic
> **Console & Cartridges** engine (EPIC-005) — a neutral query/index **Console** that loads frozen,
> per-dialect **Cartridges** of resolved facts; GNU Smalltalk 3.2.5 is **Cartridge #01**. Features are
> written once on the Console; new dialects are additive cartridges (Principle IV). An *optional* Live
> Bridge (EPIC-007) adds runtime features when present, never required. See
> [`docs/ROADMAP.md`](docs/ROADMAP.md) for the vision, architecture diagram, milestone ladder
> (0.6→2.0) and parity scorecard, and [`epics.md`](docs/product/epics.md) EPIC-005–008.
- **Shipped:** **v0.9.1 — keyword-message signature help (US-425, EPIC-005)** — `textDocument/signatureHelp`
  for keyword sends (`providers/signatureHelp.ts`), **offline**: a backward token-stream scan reconstructs
  the keyword selector typed so far + the active parameter (the keyword being filled); matches it as an
  **honest prefix union** against the workspace ∪ active-kernel cartridge selector set (`Workspace ≻
  Installed ≻ Bundled` dedup), each signature provenance-tagged. Keyword-only by design (unary/binary/head
  cursors → null); the highlighted parameter is the keyword part (facts-only cartridge has no arg names).
  Complements completion (pick the selector up front) vs signature help (where-you-are mid-arguments). New
  `evals/datasets/signature-help/` + `specs/US-425-*/manual-qa-workspace/`. Closes #68. A follow-up
  **selector-surface coverage audit** (static snippets ∪ dynamic completion ∪ signature help — division of
  labour) was raised during QA and filed to the backlog.
- **Shipped:** **v0.9.0 — Cross-Reference Intelligence (US-423, EPIC-005)** — the System Browser's
  **Senders of / Implementors of** over a **two-tier union** (workspace ∪ kernel cartridge), **offline**,
  framed honestly as a lexical union (dynamic dispatch isn't statically resolvable; rank, never filter).
  `textDocument/references` (union, `Workspace ≻ Cartridge` dev-box dedup), **plural** go-to-definition,
  `callHierarchy` (incoming = senders, outgoing = sends), and branded `Smalltalk: Senders of… /
  Implementors of…` commands → a **Smalltalk References** panel (header disclaimer + per-row provenance
  using the status-bar identity + receiver-hint badge). Engine: `server/src/xref/` (`workspaceXref.ts`
  live send index + `forEachSend`; `resolve.ts` merge/dedup/rank), `providers/references.ts`,
  `callHierarchy.ts`, `crossReference.ts`; client `crossReferences.ts` (tree + `smalltalk-cartridge:`
  virtual doc). The **installed** kernel now builds its own `crossReference` tier (senders scanned from
  source), and its rows carry real `SourceLocation` so they open the actual installed `.st` file; the
  bundled reference (no shipped body) opens a read-only fact card. Open docs are indexed only when under a
  workspace folder. New `evals/datasets/references/` + `specs/US-423-*/manual-qa-workspace/`. Closes #66.
- **Shipped:** **v0.8.0 — cartridge-aware semantic tokens (US-422, EPIC-005)** — the **first user-facing
  consumer of the Console & Cartridges foundation**. `textDocument/semanticTokens` (full + range,
  `providers/semanticTokens.ts`) classifies by role off the US-411 AST + symbol scopes: ivar→`property`,
  classvar→`property`+`static`, temp→`variable`, args→`parameter`, selectors→`method`, pseudo-vars→`keyword`.
  The differentiator (AC2) is offline + cartridge-driven: a capitalized name is `class` **iff** it resolves
  in workspace ∪ the active cartridge (kernel classes carry `defaultLibrary`), else a global `variable`;
  with no cartridge it falls back to "capitalized ⇒ class" (AC4). New `evals/datasets/semantic-tokens/`
  output eval + a `specs/US-422-*/manual-qa-workspace/` (token-inspector matrix). Closes #65.
- **Shipped:** **v0.7.0 — hover (US-415, EPIC-004)** — selectors (signature + implementor list), classes
  (superclass chain), variables (kind + declaration site), numeric literals (radix/scaled-decimal decode),
  Markdown with code fences. **Comment prose gated by provenance** (`providers/hover.ts`, `parser/comments.ts`):
  the installed kernel + your workspace source carry comments; the **bundled reference stays facts-only**
  (LGPL — prose is read locally, never redistributed; [`specs/US-415-*/spec.md`](specs/US-415-Hover/spec.md) §4a).
  New `evals/datasets/hover/` output eval + a `specs/US-415-*/manual-qa-workspace/`. Manual-QA cleared (#27, #86).
  Shipped alongside a radix-integer **syntax-coloring fix** (`16rFF.` no longer swallows the period).
- **Shipped:** **v0.6.0 — diagnostics (US-414, EPIC-004), parser-only**: an always-on **parser tier**
  publishes syntax squiggles as you type (debounced 250 ms, badge `smalltalk(parse)`, severity as
  emitted, **no `gst`**); and trivial **quick fixes** insert a missing closer (`]`/`)`/`}`/`>`) or close
  an unterminated string. New `evals/datasets/diagnostics/` output eval. The **opt-in `gst`/runtime
  compile-diagnostics tier was deferred to EPIC-007** (Live Bridge): gst 3.2.5 emits only syntax errors
  the parser already catches better; its real value (semantic errors) needs a runtime
  ([`specs/US-414-*/spec.md`](specs/US-414-Diagnostics/spec.md) §7).
- v0.5.0 — **completion + a GNU Smalltalk kernel index** (US-413, closes #1) —
  selector/class/variable completion over the workspace + a kernel tier sourced **installed-first,
  bundled-fallback** ([ADR-0002](docs/decisions/0002-kernel-symbol-sourcing.md)); `kernelLibrary`
  (`auto|bundled|off`) + status-bar identity. Slices A–D (#51–#54) + eval (#55) + release (#56);
  manual-QA matrix signed off (`specs/US-413-*/verification.md`).
- v0.4.1 — **navigation polish** (semantic folding + scope-aware document highlight,
  US-417). v0.4.0 — **code navigation** (outline/breadcrumbs, workspace symbol search,
  go-to-definition; US-412) on the error-tolerant **lexer + parser + symbol table** (US-411, internal
  M3). All language intelligence runs with **no `gst`**. Earlier: v0.3.0 grammar/snippets/config +
  **Run Current File** (US-301) + the LSP scaffold (US-410).
- **Next:** the **selector-surface coverage audit** (snippets ∪ completion ∪ signature-help division of
  labour, raised in 0.9.1 QA); then **US-416** (formatting, EPIC-004, → ~1.0). EPIC-005 consumers now span
  completion (0.5), semantic tokens (0.8), cross-reference (0.9), and signature help (0.9.1) on one Console.
- **Spike done (SPIKE-01, SHELVE):** the unknown-selector heuristic was built behind a flag + measured on
  the GST kernel (21.7k sends): naive 58 false positives → **12** after the `self` subclass-union (Template
  Method) insight; zero-FP bar **unmet** (~7-8 residual cartridge-gap FPs) + low closed-world coverage
  (~27%) → **shelved**. Gate code parked (`server/src/diagnostics/unknownSelectorGate.ts` +
  `cartridgeClassWorld.ts`, tree-shaken, unit-tested); harness `scripts/spike-unknown-selector.ts`; memo +
  `corpus-report.txt` in `specs/SPIKE-01-*/`. Reconsider only after cartridge completeness + scope
  restriction. (It also surfaced a real kernel typo — `primtiveFailed` — to report upstream.)
- **Foundation:** **EPIC-005 foundation landed**
  (US-430, 0.8/0.9): the Dialect Cartridge schema (`server/src/types/knowledge-base.ts`) + GST
  **Cartridge #01** (`scripts/export-gst-cartridge.st` → `server/data/cartridges/gst-3.2.5-cartridge.json`,
  249 classes / 4746 signatures, `contentHash`-stamped) now **drive completion**: the runtime Console
  loader (`cartridgeLoader.ts`) projects the cartridge to the completion model, the installed adapter
  emits cartridge shape, and the old `kernel-index.json` is **retired**. Resolution is Tier-1 installed
  (preferred) / Tier-2 frozen reference floor ([ADR-0003](docs/decisions/0003-cartridge-resolution.md)).

### Language server — code map, tests & conventions (read before LSP work)
- **Source:** parser/symbols in `server/src/parser/` (`lexer.ts`, `parser.ts`, `ast.ts`, `symbols.ts`,
  `walk.ts`; GST containers/chunk live in `parser.ts`). LSP providers in `server/src/providers/`
  (`documentSymbol`, `workspaceSymbol` + `workspaceIndex`, `definition`, `foldingRange`,
  `documentHighlight`, `completion`, `diagnostics`, `codeAction`, `hover`, `semanticTokens`,
  `references`, `callHierarchy`, `crossReference`); the cross-reference engine in `server/src/xref/`
  (`workspaceXref`, `resolve`); `server/src/documents/parseCache.ts`
  memoizes AST/tokens/**diagnostics**/symbols by `(uri, version)`; wiring + advertised capabilities in
  `server/src/server.ts`.
- **Cross-reference / Senders-Implementors (US-423 → 0.9.0):** `server/src/xref/workspaceXref.ts` is the
  live `selector → send-site` index (off the US-411 AST; `forEachSend` is the shared send-walk reused by
  the installed cartridge adapter); `xref/resolve.ts` merges the workspace tier with the cartridge
  `crossReference` tier into a deduped, honestly-ranked union (`Workspace ≻ Cartridge` dev-box dedup; rank
  by `receiverHint`, never filter). `providers/references.ts` (union + plural `LocationLink[]` definition),
  `callHierarchy.ts` (incoming = senders, outgoing = sends), `crossReference.ts` (the branded-command
  result: header disclaimer + per-row provenance). Client `client/src/crossReferences.ts` renders the
  **Smalltalk References** tree + a read-only `smalltalk-cartridge:` virtual doc; installed-kernel rows
  carry a real `SourceLocation` and open the actual `.st`. Output eval: `evals/datasets/references/`.
  Only docs **under a workspace folder** are indexed (a kernel file opened to view isn't workspace source).
- **Diagnostics (US-414 → 0.6.0, parser-only):** `providers/diagnostics.ts` maps the parser's
  `LexDiagnostic`s to LSP `Diagnostic`s (always-on, badge `smalltalk(parse)`); `server.ts` publishes
  them debounced (own 250 ms timer) on open/change, clears on close. `providers/codeAction.ts` offers
  the insert-missing-closer (`]`/`)`/`}`/`>`, inserted at the diagnostic range **start**, grouped) and
  close-unterminated-string quick fixes. The `}`/`>` `Expected …` diagnostics anchor at the **end of
  the last token** (`parser.ts:diagAfterPrev`) so the squiggle/fix land on the defect line. Output eval:
  `evals/datasets/diagnostics/`. **The opt-in `gst`/runtime compile-diagnostics tier was deferred to
  EPIC-007** — its built-and-removed implementation (server-side runner, no-zombie discipline, stderr
  parsing, setting/command) lives in git history (commit `a02518d`); see `specs/US-414-*/spec.md` §7.
- **Kernel completion (US-413 → US-430):** `server/src/kernel/` — neutral `model.ts` (the
  `KernelIndexData` projection target the completion service consumes), `cartridgeLoader.ts` (inlines the
  committed cartridge, builds resolved views, and projects to `KernelIndexData` via
  `cartridgeToKernelIndex`), `cartridgeHash.ts` (deterministic `contentHash` over the fact tables),
  `indexer.ts` (`indexKernelDirectoryToCartridge` — the no-`gst` installed adapter), `discovery.ts`
  (installed-kernel discovery), and `kernelIndexService.ts` (resolve `auto|bundled|off` → Tier-1 installed
  / Tier-2 floor + provenance). The completion **output eval** lives in `evals/datasets/completion/` (run
  by `npm run eval`). The status-bar item is client-side (`client/src/extension.ts`).
- **Knowledge Graph / Cartridges (EPIC-005, US-430 — landed):** the dialect-neutral **Dialect Cartridge**
  schema lives in `server/src/types/knowledge-base.ts` (pure JSON, facts-only, class-side/instance-side
  split, superclass/traits resolution separate from an open `taxonomy` bag). GST **Cartridge #01** is
  generated by `scripts/export-gst-cartridge.st` (reflective, **build-time `gst` only** — runtime stays
  zero-dependency) into `server/data/cartridges/gst-3.2.5-cartridge.json`, then `contentHash`-stamped by
  `scripts/stamp-cartridge.ts` (`npm run stamp:cartridge`). The cartridge is now the **canonical fact
  source** driving completion; the static `indexKernelDirectoryToCartridge` is the runtime *installed*
  adapter. Resolution: **[ADR-0003](docs/decisions/0003-cartridge-resolution.md)** — a generated-and-cached
  user cartridge is *preferred* (Tier-1); a **rich frozen reference floor** (the full 249-class base image)
  ships per primary dialect for zero-install/offline (Tier-2).
- **Blueprints/fixtures:** `docs/research/gst-syntax/01-*`, `02-*`; `…/test-cases/*.st`; kernel corpus
  `../smalltalk-3.2.5/kernel/` (122 files, parses with **0 diagnostics**) — the corpus the reflective
  exporter runs against to build Cartridge #01 (249 classes / 4746 signatures).
- **Three test layers — keep all green per change:**
  1. `npm run test:parser` — tsx unit + golden snapshots + the kernel smoke test (`server/test/*.test.ts` via `run.ts`).
  2. `npm run test:server` — drives the **real bundled server** over LSP (`server/test/handshake.test.mjs`); fast, no Electron, runs in CI. Extend it for each new provider.
  3. `npm run test:e2e` — **Electron** (`@vscode/test-cli`, `client/test-e2e/`) against real VS Code; **local only** (downloads VS Code + needs a display) — not in the default CI job.
- **LSP gotchas learned (apply to new providers):**
  - The extension activates via **`activationEvents: workspaceContains:**/*.{st,gst}`** — `onLanguage` alone leaves workspace features dead until a file is opened.
  - `files.exclude` is honored only because the server **dynamically registers `didChangeConfiguration`** and re-indexes; pull config in the handler, not only at init.
  - Prefer **deriving ranges in the provider** (token stream / node positions) over adding fields to the AST — avoids snapshot churn.
  - The front end **never throws**; providers return empty results, not errors.
- **Release ritual (every version):** run a **doc-rot audit** (sync the status docs below) + the
  **manual-QA matrix/spot-check** in `specs/US-XXX-*/verification.md` (it caught both 0.4.0 bugs the
  automated layers missed), then bump `version` + CHANGELOG → create the `vX.Y.Z` GitHub Release →
  CI publishes (`MARKETPLACE` PAT, ~yearly expiry — check before tagging).

## How we work (spec-driven)
Per user story (`US-XXX`): **Clarify → Spec → Plan → Task → Acceptance Harness → Implement → Verify**.
The **Acceptance Harness** phase writes the e2e/unit tests for each user-observable AC *before* code (TDD:
red → green); `new-story` scaffolds the stub at `client/test-e2e/US-XXX.acceptance.test.js` and the AC
routing is gated in `requirements-validation.md` §3.5.
- Scaffold the spec package: `npm run new-story -- US-XXX "Title" --branch`
  (creates `specs/US-XXX-Title/` + branch `feature/US-XXX-slug`).
- Source of truth: `docs/product/user-stories.md` is the backlog; once a story starts, its
  `specs/US-XXX-*/spec.md` is the detailed source of truth. Architecture: `docs/decisions/` (ADRs);
  principles: `.specify/memory/constitution.md`.
- Quality harness (`evals/`): every change gets **Output Eval** (`npm run eval` + per-feature
  datasets) and **Trajectory Eval** (process rubric). The PR template gates merge on both + human
  sign-off. A feature isn't Done until its eval passes **and CI is green on Linux/macOS/Windows**.

## Conventions
- **Branches:** `feature/US-XXX-slug` (new) / `fix/US-XXX-slug` (fixes); PR to `master`, close the issue on merge.
- **Commits:** Conventional + scoped — `feat(US-XXX):`, `fix:`, `docs:`, `chore:`, `ci:`, `test:`. Use `Closes #N` (no markdown around it) to auto-close. **Never add a `Co-Authored-By` trailer.**
- **Outward/irreversible actions** (Marketplace publish, merges, issue/PR mutations): confirm with the owner first.

## Commands
```bash
npm install            # workspaces: root + client/ + server/
npm run compile        # esbuild → dist/extension.js + dist/server.js
npm run check-types    # tsc --noEmit (strict)
npm run lint           # eslint (client/src, server/src)
npm run test:client    # tsx unit tests (pure logic)
npm run test:server    # LSP handshake test against the bundled server
npm run eval           # grammar snapshot tests (Output eval; grows per milestone)
npm run stamp:cartridge # stamp the cartridge contentHash after (re)exporting it
npm run package        # vsce package --no-dependencies (clean VSIX smoke)
npm run new-story -- US-XXX "Title" --branch
```
Manual QA: **F5** → Extension Development Host. The built grammar JSON and `dist/` are gitignored
(produced by `vscode:prepublish`).

## Architecture (ADR-0001 + the Console & Cartridges vision)
Language server is **TypeScript, bundled** in the VSIX — fully functional **without** `gst`.
`gst` is an *optional* tool (Run Current File; opt-in compile diagnostics later). Parser is
layered (core ANSI + pluggable GST chunk/brace container formats) so other dialects can plug in.

The target shape (EPIC-005): a dialect-neutral **Console** (parser/AST/symbols + workspace index +
the two-tier query engine + feature providers) over swappable **Cartridges** (frozen per-dialect
fact bases) fed by **source adapters** (reflective `.st` export, static `.st` indexer; later image
reflective export for Pharo/Squeak). Build-time `gst`/VM is allowed for *generating* a committed
cartridge; the shipped extension still loads frozen JSON and needs no runtime. The full diagram +
roadmap live in [`docs/ROADMAP.md`](docs/ROADMAP.md).

## Releasing
Create a `vX.Y.Z` GitHub Release → CI publishes via `vsce` (auth = `MARKETPLACE` secret, an Azure
DevOps PAT that **expires ~yearly** — check it before tagging). Bump `version` + CHANGELOG first.
