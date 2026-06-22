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
(reflective + static adapters). The foundation is poured; the rest builds upward (more offline
features) and outward (more dialects).

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
| **0.6** 🔵 | **Diagnostics** | 004 | parser squiggles; opt-in `gst` compile errors | A ⚖️ | error-checking |
| **0.7** 🔵 | **Hover** | 004 | selectors/classes/vars/literals + kernel facts | A+B | hover |
| **0.8** 🟣 | **Console & Cartridge foundation** 🏰 | 005 | cartridge canonical + loader + **semantic tokens** (US-422/430) | **B** | semantic highlighting (image-IDE parity) |
| **0.9** 🟣 | **Cross-Reference Intelligence** 🏰 | 005 | **references · senders/implementors · call hierarchy** + signature help; unknown-selector spike (SPIKE-01) | **B** ⚖️ | senders/implementors offline (image-IDE parity, no image) |
| **0.10** 🔵 | **Hardening & Perf** | 901 | 1k files < 5 s, completion < 100 ms, no-telemetry verified, bug-bash | — | beta quality |
| **1.0** 🔵 | **Complete Offline GST IDE** | 416/902 | formatting + scope-rename; product polish; remove `preview`; **Open VSX** | A | **parity with image-based extensions for everything that doesn't need a runtime — at zero setup** |
| **1.1–1.4** 🟣 | **Image-Grade Workbench** 🏰 | 008 | **System Browser view**, full-text method search, class-hierarchy view, more refactorings (extract method) | A+B | the "feels like Smalltalk" IDE (System Browser parity, offline) |
| **1.5** 🟣 | **THE SECOND DIALECT (Pharo)** 🏰🏰 | 006 | Pharo cartridge (image export) + Tonel container seam (US-418) + `smalltalk.dialect` auto-detect | **B — vision becomes real** | **multi-dialect — beyond ALL rivals** |
| **1.6+** 🟣➕ | **The Live Bridge** | 007 | Do-it / Print-it / **Inspect-it** / run-tests / Playground — optional, per-dialect | C | live eval/inspect (image-IDE parity) |
| **1.x** 🟣➕ | **Debugging (DAP)** | sep. ext | `vscode-smalltalk-debugger`: breakpoints, step, stack, frame restart | C | debugging (image-IDE parity) |
| **2.0** 🟣 | **The Ultimate Multi-Dialect Extension** | all | N cartridges (Squeak/Cuis/GemStone), cartridge registry, full workbench + optional live per dialect, notebooks ➕ | A+B+C | **everything they do, across every dialect, zero-setup by default** |

**Deltas from the previous roadmap:** formatting moves 0.8 → 1.0 (it was already flagged droppable);
the old 0.9 hardening becomes **0.10**; **0.8/0.9 become the Console / cross-reference inflection**
(where the vision's architecture goes load-bearing); everything from **1.1** on is net-new vision
scope (EPIC-006/007/008). The 0.6 → 1.0 line is otherwise unchanged.

## Parity scorecard — vs. the assessed extensions

`✅ have · ⏳ planned-ver · ➕ optional-live · — none`

| Capability | Us @0.5 | Us →1.0 | Us →2.0 | Image LSP extension | Image IDE extension |
|---|---|---|---|---|---|
| Highlight / snippets / config | ✅ | ✅ | ✅ | ✅ | ✅ |
| Completion | ✅ | ✅ | ✅ | ✅ (image) | ✅ (image) |
| Outline / symbols / go-to-def | ✅ | ✅ | ✅ | ✅ | ✅ |
| Diagnostics | — | ⏳0.6 | ✅ | ✅ (image) | ✅ (image) |
| Hover | — | ⏳0.7 | ✅ | ✅ | ✅ |
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

1. **0.6.0 / US-414 — Diagnostics** is the next shipping milestone. Slice A is small (the US-411 parser
   already emits `parse().diagnostics`): publish them on change, debounced, code `smalltalk(parse)`. Then
   the opt-in `gst`-on-save path (timeout/kill-on-edit, no zombies), then trivial code actions.
2. **EPIC-005 foundation is underway** ahead of its 0.8/0.9 milestones: the Dialect Cartridge schema
   (`server/src/types/knowledge-base.ts`) and GST **Cartridge #01** (`scripts/export-gst-cartridge.st` →
   `server/data/cartridges/gst-3.2.5-cartridge.json`, 249 classes / 4746 signatures, validated) are
   built; remaining is the Console loader + the kernel-index **convergence** (US-430).
3. **Optional v0.5.1** — the two near-term reconciliation fixes: **US-420** (#60, completion
   pseudo-variables) + **US-421** (#61, CI kernel fixtures). Both `size:S`.

## Backlog & sequencing (0.5.0 plan reconciliation)

Small follow-ups where the implementation diverged from the (now-archived) genesis `plan.md`; full
stories in [`docs/product/user-stories.md`](product/user-stories.md).

| Story | What | When to do it | Issue |
|---|---|---|---|
| US-418 | Container-format seam (dialect door) | **Trigger fires at 1.5** (Pharo/Tonel — EPIC-006) | #58 |
| US-419 | Kernel-index method categories | Ride with **US-415 hover** (needs the grouping) | #59 |
| US-420 | Completion pseudo-variables | Near-term — v0.5.1 or front of US-414 | #60 |
| US-421 | CI vendored kernel smoke fixtures | Near-term — v0.5.1 or front of US-414 | #61 |

## EPIC map

| Epic | Theme | Stream | Status |
|---|---|---|---|
| EPIC-004 | Language Intelligence — TypeScript LSP (offline, single-dialect) | A | In progress (→1.0) |
| EPIC-005 | Offline Knowledge Graph — Console & Cartridges | B | In progress (foundation) |
| EPIC-006 | Multi-Dialect Expansion (2nd+ cartridges, dialect detection, container seam) | B | Planned (1.5) |
| EPIC-007 | The Live Bridge (optional runtime delegation) | C | Planned (1.6+) |
| EPIC-008 | Image-Grade Workbench (System Browser, refactorings, search) | A+B | Planned (1.1–1.4) |
| — | Debugging (DAP) — separate `vscode-smalltalk-debugger` extension | C | Future |

## How each milestone ships (the repeatable ritual)

Per story: **Clarify → Spec → Plan → Task → Implement → Verify** (`npm run new-story -- US-XXX "…" --branch`).
Each slice keeps the **three test layers** green (`test:parser`, `test:server`, `test:e2e`) **plus an
output-eval dataset** in `evals/datasets/<feature>/` (use `completion/` as the template). Then the
**release ritual**: doc-rot audit + the **manual-QA matrix** (`specs/US-XXX-*/verification.md`, real
corpus + clean VSIX) → bump version + CHANGELOG → **check the `MARKETPLACE` PAT** → cut the `vX.Y.Z`
Release (CI publishes). Full detail in [`CLAUDE.md`](../CLAUDE.md) and [`CONTRIBUTING.md`](../CONTRIBUTING.md).

_Last updated: 2026-06-22 — roadmap evolved to the **Console & Cartridges** vision: added the
architecture diagram, convergence-stream view, the post-1.0 vision ladder (EPIC-006/007/008), and the
competitor parity scorecard. EPIC-005 foundation (cartridge schema + GST Cartridge #01) built &
validated. Near-term shipping focus unchanged: 0.6.0 / US-414 (diagnostics)._

**Dialect scope:** GNU Smalltalk through 1.0 (the complete offline GST IDE). The **second dialect
(Pharo)** lands at **1.5** (EPIC-006), which is when the *pluggable* `ContainerFormat` seam (US-418)
is finally built — see [ADR-0001 §Update](decisions/0001-typescript-bundled-lsp-server.md). The
Console/cartridge architecture (EPIC-005) is what makes that an additive change, not a rewrite
(Constitution Principle IV, *Dialect Agnostic*).

**Sustainability:** every milestone is independently shippable; work-in-progress is kept to one
milestone at a time; formatting (1.0) and all optional-runtime features (EPIC-007, debugging) are
explicitly droppable and never required for the core offline experience.
