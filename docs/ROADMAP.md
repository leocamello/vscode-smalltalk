# Roadmap

A snapshot of the milestone plan. Detail lives in `docs/product/` (epics, user stories) and
`docs/decisions/` (ADRs). Architecture baseline:
[ADR-0001 — TypeScript bundled LSP server](decisions/0001-typescript-bundled-lsp-server.md) +
[ADR-0002 — kernel symbol sourcing](decisions/0002-kernel-symbol-sourcing.md) +
[ADR-0003 — cartridge resolution](decisions/0003-cartridge-resolution.md).

## The vision: a dialect-agnostic static Smalltalk intelligence engine ("Console & Cartridges")

> Features are written **once** against a dialect-neutral **Console**; each dialect is a swappable
> **Cartridge** of frozen facts. Adding a dialect = adding a cartridge, **not** rewriting features.
> An **optional Live Bridge** lights up the soulful runtime features (Do-it/Print-it/Inspect-it,
> test run, debugging) when a VM/image is present — and degrades to nothing when it isn't. The
> result: image-grade Smalltalk tooling that is **multi-dialect** and **works with zero runtime by
> default** — something no image-bound competitor can match without abandoning its architecture.

```
                    VS CODE  (LSP client · commands · tree views)
┌──────────────────────────────────────────────────────────────────────┐
│  FEATURE PROVIDERS   — written ONCE, dialect-agnostic                   │
│  completion · hover · diagnostics · references · senders/implementors · │
│  call hierarchy · semantic tokens · formatting · refactorings ·         │
│  System Browser view · test explorer                                    │
├──────────────────────────────────────────────────────────────────────┤
│  THE CONSOLE   — dialect-neutral engine                                 │
│    ┌──────────────┐      ┌────────────────────────────────────────┐    │
│    │ Workspace    │◄────►│ Two-Tier Query Engine                  │    │
│    │ Index (live) │      │  reconcile · rank · HONEST union (DNU) │    │
│    └──────────────┘      └────────────────────────────────────────┘    │
│       ▲ parser/AST/symbols              ▲ Cartridge Loader              │
├──────────────────────────────────────────────────────────────────────┤
│  CARTRIDGES   — frozen, per-dialect knowledge (FACTS only)              │
│   [GST 3.2.5 #01 ✅]  [Pharo 12]  [Squeak]  [Cuis]  [GemStone]          │
├──────────────────────────────────────────────────────────────────────┤
│  SOURCE ADAPTERS   — build-time cartridge producers                     │
│   reflective .st export ✅ · static .st indexer ✅ ·                     │
│   image reflective export (Pharo/Squeak) · GCI export (GemStone)        │
└──────────────────────────────────────────────────────────────────────┘
   ┌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┐
   ╎ OPTIONAL LIVE BRIDGE  (only if a runtime/image is present)        ╎
   ╎  Do-it · Print-it · Inspect-it · run/debug tests · Playground/REPL╎
   ╎  …vanishes cleanly when absent — never required (ADR-0001)         ╎
   └╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┘
```

**Already real (✅):** the parser/AST/symbol table, the workspace index, GST **Cartridge #01**
(reflective + static adapters), and the **Console loader** that resolves it (Tier-1 installed /
Tier-2 frozen floor) and drives completion (US-430, merged). The foundation is poured; the rest
builds upward (more offline features) and outward (more dialects).

**How cartridges resolve ([ADR-0003](decisions/0003-cartridge-resolution.md)):** a user-specific
cartridge **generated-and-cached from the actual install** is *preferred* (no-runtime source parse
where the dialect ships source; opt-in reflective export for image-only dialects), over a **rich
frozen reference floor** that guarantees the zero-install/offline experience and the eval baseline.
So we ship **one rich floor per primary dialect — not a blob per version** — and the user's own
version/packages always win when present.

### Three streams converge into the vision

```
STREAM A — OFFLINE FEATURES  (the lead; parity with image-based rivals, no setup)
 grammar→nav→completion→diagnostics→hover→references→sem-tokens→formatting→refactor ─┐
                                                                                    │
STREAM B — KNOWLEDGE ARCHITECTURE  (THE MOAT — compounds, can't be 48-hr forked)    ├─► THE ULTIMATE
 symbols→kernel index→CARTRIDGE schema→CONSOLE→multi-cartridge→dialect auto-detect ──┤   MULTI-DIALECT
                                                                                    │   SMALLTALK
STREAM C — OPTIONAL LIVE  (the soul; only when a runtime exists)                     │   EXTENSION (2.0)
 Run File→test run→Do-it / Inspect-it→debugger (DAP)→Playground ─────────────────────┘
```

The complexity invested **once** in the Console + cross-reference engine + System Browser (0.8–1.4)
is amortized across **every** future dialect — that is the compounding moat.

## Milestone ladder

Legend: 🟢 done · 🔵 planned · 🟣 vision (new) · ➕ optional-runtime · 🏰 moat-builder · ⚖️ parity/lead

| Ver | Theme | Epic | Headline capability | Stream | Parity unlocked |
|---|---|---|---|---|---|
| 0.2 🟢 | Grammar / snippets / config | 002 | declarative editing | A | — |
| 0.3 🟢 | LSP scaffold + Run File | 003/004 | client↔server; `gst` opt-in run | A/C | — |
| 0.4 🟢 | Navigation | 004 | outline, workspace symbols, go-to-def, folding, highlight | A+B | core navigation |
| 0.5 🟢 | Completion + kernel index | 004 | completion; GST kernel (installed-first/bundled) | A+B | core IntelliSense |
| 0.6 🟢 | Diagnostics | 004 | live parser squiggles + insert-closer/close-string quick fixes (runtime compile errors → EPIC-007) | A ⚖️ | error-checking |
| 0.7 🟢 | Hover | 004 | selectors/classes/vars/literals + provenance-gated comment prose | A+B | hover |
| **0.8** 🟣 | **Console & Cartridge foundation** 🏰 | 005 | cartridge canonical + loader + **semantic tokens** (US-422/430) | **B** | semantic highlighting (image-IDE parity) |
| **0.9** 🟣 | **Cross-Reference Intelligence** 🏰 | 005 | **references · senders/implementors · call hierarchy** + signature help; unknown-selector spike (SPIKE-01) | **B** ⚖️ | senders/implementors offline (image-IDE parity, no image) |
| **0.10** 🔵 | **Hardening & Perf** | 901 | 1k files < 5 s, completion < 100 ms, no-telemetry verified, bug-bash | — | beta quality |
| **1.0** 🔵 | **Complete Offline GST IDE** | 416/902 | formatting + scope-rename; product polish; remove `preview`; **Open VSX** | A | **parity with image-based extensions for everything that doesn't need a runtime — at zero setup** |
| **~1.0** 🟣 | **Tonel read-only wedge** ("Trojan Horse") | 006 | **read-only Tonel** grammar + folding + outline (US-424) — *no cartridge, no seam* | A ⚖️ | best-in-class Tonel reading — lands the Pharo/GemStone/VA crowd early |
| **1.1–1.4** 🟣 | **Image-Grade Workbench** 🏰 | 008 | **System Browser view**, full-text method search, class-hierarchy view, more refactorings (extract method) | A+B | the "feels like Smalltalk" IDE (System Browser parity, offline) |
| **1.5** 🟣 | **THE SECOND DIALECT (Pharo)** 🏰🏰 | 006 | Pharo cartridge (image export) + full Tonel container seam (US-418) + `smalltalk.dialect` auto-detect **+ status-bar picker** (US-602) | **B — vision becomes real** | **multi-dialect — beyond ALL rivals** |
| **1.6+** 🟣➕ | **The Live Bridge** | 007 | Do-it / Print-it / **Inspect-it** / run-tests / Playground + **runtime compile/semantic diagnostics** (US-705) + **writable `smalltalk-image://` VFS** (US-706) — optional, per-dialect | C | live eval/inspect + real compile errors + live-image editing (image-IDE parity) |
| **1.x** 🟣➕ | **Debugging (DAP)** | sep. ext | `vscode-smalltalk-debugger`: breakpoints, step, stack, frame restart | C | debugging (image-IDE parity) |
| **2.0** 🟣 | **The Ultimate Multi-Dialect Extension** | all | N cartridges (Squeak/Cuis/GemStone), cartridge registry, full workbench + optional live per dialect, notebooks ➕ | A+B+C | **everything they do, across every dialect, zero-setup by default** |

**Deltas from the previous roadmap:** formatting moves 0.8 → 1.0 (it was already flagged droppable);
the old 0.9 hardening becomes **0.10**; **0.8/0.9 become the Console / cross-reference inflection**
(where the vision's architecture goes load-bearing); everything from **1.1** on is net-new vision
scope (EPIC-006/007/008). The 0.6 → 1.0 line is otherwise unchanged.

**Delta (2026-06-24 strategy review).** Three ideas from an external review are folded into the plan:
(1) **Tonel as the "Trojan Horse" — resequencing decision:** a **read-only** Tonel experience (grammar +
folding + outline, **US-424**) is **pulled forward to ~1.0** in **Stream A** — it needs *no* cartridge and
*no* `ContainerFormat` seam, is cheap, and lands the Pharo/GemStone/VA community long before the Pharo
cartridge. The **full** Tonel dialect (US-418 seam + Pharo cartridge) stays at **1.5**; US-424 becomes the
grammar layer beneath it (additive, not thrown away). (2) **Live-image Virtual FileSystem** (`smalltalk-image://`
via `registerFileSystemProvider`, writable `Ctrl+S`→compile) is captured as **US-706** under the Live
Bridge (1.6+), and US-801 gains a TreeView-vs-VFS primitive gate. (3) **Status-bar dialect picker:** US-602
now does **both** — auto-detect *and* a manual override picker. *What we did **not** adopt:* the review's
"decoupled external-LSP client" framing — our **Console & Cartridges** engine (own offline intelligence,
not a launcher for other servers) is the moat; an external live server stays an *optional* EPIC-007 bonus.

## Parity scorecard — vs. the assessed extensions

`✅ have · ⏳ planned-ver · ➕ optional-live · — none`

| Capability | Us @0.5 | Us →1.0 | Us →2.0 | Image LSP extension | Image IDE extension |
|---|---|---|---|---|---|
| Highlight / snippets / config | ✅ | ✅ | ✅ | ✅ | ✅ |
| **Tonel read-only (grammar/fold/outline)** | — | ⏳~1.0 | ✅ | partial | partial |
| Completion | ✅ | ✅ | ✅ | ✅ (image) | ✅ (image) |
| Outline / symbols / go-to-def | ✅ | ✅ | ✅ | ✅ | ✅ |
| Diagnostics | — | ✅0.6 | ✅ | ✅ (image) | ✅ (image) |
| Hover | — | ✅0.7 | ✅ | ✅ | ✅ |
| Semantic tokens | — | ⏳0.8 | ✅ | — | ✅ |
| References / **Senders / Implementors** | — | ⏳0.9 | ✅ | partial | ✅ (image) |
| Call hierarchy | — | ⏳0.9 | ✅ | — | ✅ |
| Formatting | — | ⏳1.0 | ✅ | ✅ | ✅ |
| Refactorings (rename → extract) | — | ⏳1.0+ | ✅ | ✅ (famous) | some |
| **System Browser view** | — | ⏳1.x | ✅ | (is the image) | ✅ |
| SUnit test explorer | — | — | ✅➕ | ✅ (image) | ✅ |
| Do-it / Print-it / **Inspect-it** | — | — | ✅➕ | ✅ (image) | ✅ |
| Debugging (DAP) | — | — | ✅➕ | ✅ (image) | ✅ |
| Notebooks | — | — | ✅➕ | — | ✅ |
| **Multi-dialect** | — | — | **✅✅** | ❌ Pharo-only | ❌ GemStone-only |
| **Works with zero runtime** | **✅** | **✅** | **✅** | ❌ | ❌ |

The bottom two rows are the point. By **1.0** we equal the rivals on everything that doesn't strictly
need a VM — *with no setup*. By **2.0** the runtime-dependent features arrive as an *optional* bonus,
**and** we remain the only one that is multi-dialect and zero-setup.

## Next up

1. **0.6.0 / US-414 — Diagnostics** is implemented and **in release**, **parser-only**: the always-on
   parser tier (debounced squiggles, badge `smalltalk(parse)`, severity as emitted) + insert-missing-
   closer (`]`/`)`/`}`/`>`) and close-unterminated-string quick fixes — **no `gst`**. The opt-in
   `gst`/runtime compile-diagnostics tier (original AC2/AC3) was built then **deferred to EPIC-007**
   (Live Bridge): gst 3.2.5 emits only syntax errors the parser already catches better; real value
   (semantic errors) needs a runtime. **0.7.0 / US-415 — hover — shipped; next: 0.8 / EPIC-005 (US-422 semantic tokens) or US-416 (formatting).**
2. **EPIC-005 foundation has landed** (US-430, merged #82) ahead of its 0.8/0.9 milestones: the Dialect
   Cartridge schema (`server/src/types/knowledge-base.ts`) + GST **Cartridge #01**
   (`scripts/export-gst-cartridge.st` → `server/data/cartridges/gst-3.2.5-cartridge.json`, 249 classes /
   4746 signatures, `contentHash`-stamped) now **drive completion** via the runtime Console loader
   (`cartridgeLoader.ts`); the installed adapter emits cartridge shape (Tier-1 installed / Tier-2 frozen
   floor, ADR-0003) and the old `kernel-index.json` is **retired**. Next EPIC-005 consumers: **US-422**
   (cartridge-aware semantic tokens) and **US-423** (references/senders via the `crossReference` tier).
3. **Optional v0.5.1** — the two near-term reconciliation fixes: **US-420** (#60, completion
   pseudo-variables) + **US-421** (#61, CI kernel fixtures). Both `size:S`.

## Backlog & sequencing (0.5.0 plan reconciliation)

Small follow-ups where the implementation diverged from the (now-archived) genesis `plan.md`; full
stories in [`docs/product/user-stories.md`](product/user-stories.md).

| Story | What | When to do it | Issue |
|---|---|---|---|
| US-424 | Tonel **read-only** wedge (grammar + folding + outline) | **Pull forward to ~1.0** (Stream A, no cartridge/seam — EPIC-006) | TBD |
| US-418 | Container-format seam (dialect door) — *full* Tonel parsing | **Trigger fires at 1.5** (Pharo/Tonel — EPIC-006) | #58 |
| US-419 | Kernel-index method categories | Ride with **US-415 hover** (needs the grouping) | #59 |
| US-420 | Completion pseudo-variables | Near-term — v0.5.1 or front of US-414 | #60 |
| US-421 | CI vendored kernel smoke fixtures | Near-term — v0.5.1 or front of US-414 | #61 |

## EPIC map

| Epic | Theme | Stream | Status |
|---|---|---|---|
| EPIC-004 | Language Intelligence — TypeScript LSP (offline, single-dialect) | A | In progress (→1.0) |
| EPIC-005 | Offline Knowledge Graph — Console & Cartridges | B | In progress (foundation landed — US-430; next US-422/423) |
| EPIC-006 | Multi-Dialect Expansion (2nd+ cartridges, dialect detection, container seam) | A+B | Planned (read-only Tonel wedge US-424 ~1.0; full second dialect 1.5) |
| EPIC-007 | The Live Bridge (optional runtime delegation) | C | Planned (1.6+) |
| EPIC-008 | Image-Grade Workbench (System Browser, refactorings, search) | A+B | Planned (1.1–1.4) |
| — | Debugging (DAP) — separate `vscode-smalltalk-debugger` extension | C | Future |

## How each milestone ships (the repeatable ritual)

Per story: **Clarify → Spec → Plan → Task → Acceptance Harness → Implement → Verify** (`npm run new-story -- US-XXX "…" --branch`).
The **Acceptance Harness** phase pins each user-observable AC with a failing e2e test before code (TDD).
Each slice keeps the **three test layers** green (`test:parser`, `test:server`, `test:e2e`) **plus an
output-eval dataset** in `evals/datasets/<feature>/` (use `completion/` as the template). Then the
**release ritual**: doc-rot audit + the **manual-QA matrix** (`specs/US-XXX-*/verification.md`, real
corpus + clean VSIX) → bump version + CHANGELOG → **check the `MARKETPLACE` PAT** → cut the `vX.Y.Z`
Release (CI publishes). Full detail in [`CLAUDE.md`](../CLAUDE.md) and [`CONTRIBUTING.md`](../CONTRIBUTING.md).

_Last updated: 2026-06-24 — **0.6.0 / US-414 (diagnostics) implemented, parser-only**: always-on parser
squiggles + insert-closer/close-string quick fixes; new `evals/datasets/diagnostics/` output eval. The
opt-in `gst`/runtime compile-diagnostics tier was built then **deferred to EPIC-007** (Live Bridge) —
redundant with the parser for syntax, real value (semantic errors) needs a runtime; shipped **v0.6.0**.
**EPIC-005 foundation landed** earlier: US-430 (Console loader + cartridge
convergence) merged (#82) — completion runs off GST Cartridge #01. **US-415 hover shipped (v0.7.0).** Next EPIC-005
consumers: US-422 (semantic tokens) / US-423 (references/senders). **2026-06-24 strategy review folded in:**
US-424 (read-only Tonel "Trojan Horse" — resequenced to ~1.0, Stream A), US-706 (writable `smalltalk-image://`
VFS under the Live Bridge), and a status-bar dialect picker on US-602; the external-LSP-client framing was
considered and declined in favor of the Console & Cartridges moat._

**Dialect scope:** GNU Smalltalk through 1.0 (the complete offline GST IDE). A **read-only Tonel
wedge** (US-424) lands around **~1.0** as a cheap Stream-A grab of the image-based community — it needs
no cartridge and no seam. The **second dialect (Pharo)** — full Tonel *parsing* + the Pharo cartridge —
lands at **1.5** (EPIC-006), which is when the *pluggable* `ContainerFormat` seam (US-418) is finally
built — see [ADR-0001 §Update](decisions/0001-typescript-bundled-lsp-server.md). The Console/cartridge
architecture (EPIC-005) is what makes that an additive change, not a rewrite (Constitution Principle IV,
*Dialect Agnostic*).

**Sustainability:** every milestone is independently shippable; work-in-progress is kept to one
milestone at a time; formatting (1.0) and all optional-runtime features (EPIC-007, debugging) are
explicitly droppable and never required for the core offline experience.
