# vscode-smalltalk Epics

---

## EPIC-001: Improve User Onboarding & Documentation

* **ID:** EPIC-001
* **Status:** Done (v0.2.0)
* **Priority:** High
* **Phase:** Phase 1
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento 

**Goal / Value Proposition:**
> To provide clear, comprehensive, and accurate documentation that enables users (especially those new to Smalltalk in VS Code or using GNU Smalltalk) to easily install, configure, and effectively use the extension's foundational features. A good onboarding experience is crucial for adoption and user satisfaction.

**Scope & Description:**
* Revamp the main `README.md` file to align with best practices observed in other popular VS Code extensions.
* Include clear sections for Prerequisites (installing/configuring GNU Smalltalk), Quick Start, Features (accurately reflecting current capabilities), Configuration Settings, and Troubleshooting.
* Ensure instructions are specific to the target audience (GNU Smalltalk, file-based workflow).
* Potentially create simple example `.st` files or link to a sample project.

**Target Users:**
* All users, especially new users and those migrating from other Smalltalk environments or editors.
* Users focused on GNU Smalltalk.

**Related User Stories:**
* [US-101: Revamp README Structure & Introduction]
* [US-102: Add Clear Prerequisites Section to README]
* [US-103: Create Quick Start Guide in README]
* [US-104: Update Features List in README]
* [US-105: Document Configuration Settings in README]
* [US-106: Add Basic Troubleshooting Section to README]

**Success Metrics (Optional):**
* Reduced number of user issues related to basic setup and configuration.
* Positive feedback on documentation clarity.

---

## EPIC-002: Enhance Declarative Editing Features

* **ID:** EPIC-002
* **Status:** Done (v0.2.0)
* **Priority:** Medium
* **Phase:** Phase 1
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento 

**Goal / Value Proposition:**
> To ensure the core editing experience provided by declarative features (syntax highlighting, snippets, language configuration) is accurate, reliable, and helpful for writing GNU Smalltalk code in `.st` files, improving developer productivity and reducing friction.

**Scope & Description:**
* Review and refine the TextMate grammar (`.tmLanguage.json`) for accurate syntax highlighting of GNU Smalltalk constructs, guided by analysis of `lex.c`/`gst-parse.c`.
* Update or create useful code snippets (`snippets.json`) for common Smalltalk patterns.
* Verify and enhance the `language-configuration.json` for correct bracket matching/closing, comment toggling, auto-indentation rules, and folding markers relevant to Smalltalk syntax.

**Target Users:**
* All users writing Smalltalk code within VS Code.

**Related User Stories:**
* [US-200: Investigate GST Syntax & TextMate Grammar Implementation]
* [US-201: Refine Syntax Highlighting Grammar] *(Depends on US-200)*
* [US-202: Create/Update Code Snippets]
* [US-203: Enhance Language Configuration]

**Success Metrics (Optional):**
* Improved accuracy of syntax highlighting reported by users.
* Increased usage of provided snippets.
* Fewer issues related to incorrect bracket handling or commenting.

---

## EPIC-003: Basic Workflow Integration

* **ID:** EPIC-003
* **Status:** Done (v0.3.0)
* **Priority:** Low
* **Phase:** Phase 1
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento 

**Goal / Value Proposition:**
> To provide simple, integrated commands that allow users to perform basic GNU Smalltalk actions directly from VS Code, offering a more streamlined workflow than switching to an external terminal for common tasks.

**Scope & Description:**
* Implement a VS Code command (e.g., `Smalltalk: Run Current File`) accessible via the Command Palette.
* This command should execute the currently active `.st` file using the GNU Smalltalk interpreter (`gst`).
* Requires a configuration setting for the user to specify the path to their `gst` executable (`smalltalk.gnuSmalltalkPath`).
* Output from the `gst` process should be displayed in the VS Code Output panel or integrated Terminal.
* **Architectural Note:** Implementation must use appropriate VS Code APIs (e.g., `tasks`, `terminal`) cleanly, avoiding assumptions that could complicate future LSP/DAP integration.

**Target Users:**
* Users performing simple script execution or testing with GNU Smalltalk.

**Related User Stories:**
* [US-301: Implement 'Run Current File' Command]

**Success Metrics (Optional):**
* User adoption of the "Run File" command.
* Successful execution of simple `.st` files from within VS Code.

---

## EPIC-004: Language Intelligence — TypeScript LSP

* **ID:** EPIC-004
* **Status:** In Progress (US-410 scaffold, US-411 parser/symbols, US-412 navigation, US-417 folding+highlight, US-413 completion + kernel index all done; v0.4.0, v0.4.1, v0.5.0 shipped. Next: US-414 diagnostics)
* **Priority:** Medium
* **Phase:** Phase 2
* **Date Proposed:** 2025-05-02
* **Date Revised:** 2026-06-13 (architecture decision: bundled TypeScript server, not a Smalltalk backend)
* **Owner:** Leonardo Nascimento 

**Goal / Value Proposition:**
> To deliver Smalltalk language intelligence (navigation, completion, diagnostics, hover, formatting) via a **TypeScript language server bundled with the extension**, working out of the box without any external Smalltalk installation.

**Scope & Description:**
* Build the extension client (`vscode-languageclient`) and a bundled server (`vscode-languageserver-node`), wired with esbuild.
* Implement a hand-written, error-tolerant Smalltalk parser + symbol table (core ANSI layer + pluggable GST chunk/brace container formats).
* Layer LSP features on the parser: document/workspace symbols + go-to-definition, completion (with a GNU Smalltalk kernel-library index), diagnostics, hover, and formatting.
* Treat `gst` as an *optional* external tool only (Run Current File; opt-in compile diagnostics) — never a hard dependency.

**Target Users:**
* GNU Smalltalk developers using file-based (`.st`/`.gst`) workflows.

**Related User Stories:**
* [US-410: TypeScript LSP scaffold (client + bundled server)]
* [US-411: Error-tolerant Smalltalk parser + symbol table]
* [US-412: Document/workspace symbols + go-to-definition]
* [US-413: Completion + GNU Smalltalk kernel index]
* [US-414: Diagnostics (parser live; gst opt-in)]
* [US-415: Hover]
* [US-416: Formatting]

**Superseded Stories (closed):**
* ~~US-401: Research Smalltalk JSON-RPC Libraries~~ (obsolete — server is TypeScript, no custom JSON-RPC)
* ~~US-402: Prototype LSP Process Management & Communication~~ (obsolete — bundled server, no external Smalltalk process)
* ~~US-403: Research Smalltalk Code Analysis Techniques~~ (replaced by US-411, the TypeScript parser)

**Success Metrics (Optional):**
* Navigation and completion work in a fresh install with no `gst` present.
* Parser passes the snapshot/mutation suite and the GNU Smalltalk kernel smoke test.

---

## EPIC-005: Offline Knowledge Graph ("Console & Cartridges")

* **ID:** EPIC-005
* **Status:** In Progress (foundation — US-430 cartridge schema + GST Cartridge #01)
* **Priority:** High
* **Phase:** Phase 2 (extends EPIC-004)
* **Date Proposed:** 2026-06-21
* **Owner:** Leonardo Nascimento

**Goal / Value Proposition:**
> Reframe the language server as a dialect-agnostic *static Smalltalk intelligence engine*: a **Console** (the dialect-neutral query/index core) that loads compiled, frozen **Cartridges** (per-dialect knowledge bases of resolved facts). GNU Smalltalk 3.2.5 is **Cartridge #01**. This is the project's durable, hard-to-copy asset (ADR-0002) — image-grade cross-reference knowledge delivered **offline, with zero runtime dependency**, which image-bound competitors cannot match.

**Scope & Description:**
* A pure-JSON **Dialect Cartridge** schema (`server/src/types/knowledge-base.ts`): resolved facts only (no ASTs), explicit class-side/instance-side split, **structural resolution** (superclass chains, Pharo traits) separated from an **open `taxonomy` bag** so new dialects plug in without schema change.
* A **two-tier lookup engine** reconciling the live **Workspace Index** with the frozen **Cartridge(s)**, with an honest dynamic-dispatch UX posture (lexical-union results, never false precision).
* Per-dialect cartridge **compilers/exporters** emitting that schema: a GST **reflective exporter** now (`scripts/export-gst-cartridge.st`, build-time `gst` only); Pharo/Squeak image-export later.
* Cartridge-native LSP features: semantic tokens (US-422), references + senders/implementors + call hierarchy (US-423), and a guarded unknown-selector heuristic (SPIKE-01 → future story).

**Target Users:**
* GNU Smalltalk developers now; Pharo/Squeak/Cuis/GemStone users as future cartridges land.

**Related User Stories:**
* [US-430: Dialect Cartridge Schema + GST Cartridge #01 (reflective exporter)]
* [US-422: Semantic Tokens (Cartridge-aware)]
* [US-423: References + Senders/Implementors (Two-Tier Engine)]
* [US-425: Signature Help]
* [US-426: Scope-aware Rename]
* [SPIKE-01: Unknown-Selector Heuristic — false-positive validation]

**Success Metrics (Optional):**
* Senders/Implementors over kernel + workspace resolve **offline** in < 10 ms typical.
* Adding a second dialect requires a **new cartridge only** — no Console/schema change.
* The unknown-selector heuristic ships only if SPIKE-01 shows **≈0 false positives**.

**Architectural Note:**
* Build-time `gst` (the reflective exporter) does **not** breach ADR-0001: it generates a *committed* JSON artifact; the shipped extension loads the frozen cartridge and needs no `gst` at runtime. GST kernel comments are LGPL-2.1, so Cartridge #01 is **facts-only** (`carriesProse: false`).

---

## EPIC-006: Multi-Dialect Expansion

* **ID:** EPIC-006
* **Status:** Planned (milestone **1.5** — "The Second Dialect")
* **Priority:** High *(this is where the Console & Cartridges vision becomes real)*
* **Phase:** Phase 3
* **Date Proposed:** 2026-06-22
* **Owner:** Leonardo Nascimento

**Goal / Value Proposition:**
> Turn the single-dialect engine into a genuinely **multi-dialect** one by adding a *second* dialect (Pharo) — the moment the EPIC-005 architecture pays off. Every offline feature (completion, hover, diagnostics, references, semantic tokens, System Browser) must light up for the new dialect **without a feature rewrite**, proving that adding a dialect is *additive data* (a new Cartridge + adapter), not a fork of the core. This is the differentiator no image-bound competitor can follow: **multi-dialect, zero-runtime by default**.

**Scope & Description:**
* **Second cartridge — Pharo:** build it via an **image reflective export** adapter (run the Pharo VM headless against its image, dump `allSubclasses`/selectors/arity/traits/package-tags to the `DialectCartridge` schema), or ship that export as a `bundled` cartridge. Pharo is MIT, so prose (class/method comments) may be carried (`carriesProse: true`).
* **Container-format seam (US-418):** finally build the deferred `ContainerFormat` pluggability so Tonel (Pharo/Cuis) parses alongside GST brace/chunk without touching the parser core. **This epic is the trigger for US-418.**
* **Dialect axis:** a `smalltalk.dialect` setting with **auto-detection** (file extension, Tonel markers, workspace cues) selecting which cartridge(s) load; `kernelLibrary` stays the orthogonal *sourcing* axis (ADR-0002 §5).
* Provenance/status surface the active dialect(s) honestly.

**Target Users:**
* Pharo developers using a file-based workflow in VS Code (and, downstream, Squeak/Cuis/GemStone users as further cartridges land in 2.0).

**Related User Stories:**
* US-418 (#58): Container-Format Seam (Dialect Door) — *trigger fires here*
* US-601 (#70): Pharo cartridge via image reflective export (adapter)
* US-602 (#71): `smalltalk.dialect` axis + auto-detection
* US-603 (#72): Multi-cartridge Console loader (load/rank across active dialects)

**Success Metrics (Optional):**
* Adding Pharo requires a **new cartridge + adapter only** — no change to the Console or feature providers (the architecture test).
* All EPIC-004/005 features work on a Pharo workspace with no Pharo VM present (offline), VM optional for the live tier.

---

## EPIC-007: The Live Bridge (Optional Runtime Delegation)

* **ID:** EPIC-007
* **Status:** Planned (milestone **1.6+**)
* **Priority:** Medium *(the "soul"; strictly optional — never required)*
* **Phase:** Phase 3
* **Date Proposed:** 2026-06-22
* **Owner:** Leonardo Nascimento

**Goal / Value Proposition:**
> Capture Smalltalk's *live* magic — Do-it / Print-it / **Inspect-it**, running tests, a Playground/REPL — by **optionally** delegating to a running runtime/image **when one is present**, per dialect. This is ADR-0002's "optional delegation to a live image" made real. It is the one place we can match the visceral experience of image-based environments — but it must **degrade to nothing** when no runtime exists, so the zero-runtime guarantee (ADR-0001, Constitution) is never broken.

**Scope & Description:**
* **Evaluate Selection:** Do-it (run), Print-it / Display-it (inline result), all via the optional dialect runtime (`gst` for GST; Pharo VM for Pharo), reusing the US-301/US-414 process discipline (timeout, kill-on-edit, no zombies).
* **Inspect-it:** a structured object inspector. **DoR gate:** justify any webview vs. a native tree view against Constitution I (*avoid webviews unless strictly necessary*).
* **SUnit Test Explorer:** static detection of `TestCase` subclasses + `test*` methods (offline, via the Console) surfaced in the VS Code Testing API + CodeLens; **run/debug via the optional runtime**.
* **Playground / REPL:** a scratch evaluation surface backed by the optional runtime.

**Target Users:**
* Developers who *do* have a runtime/image installed and want the live loop — without the offline users ever paying for it.

**Related User Stories:**
* US-701 (#73): Evaluate Selection (Do-it / Print-it / Display-it) — optional runtime
* US-702 (#74): SUnit Test Explorer (static detect; optional run/debug)
* US-703 (#75): Inspect-it (structured inspector)
* US-704 (#76): Playground / REPL

**Success Metrics (Optional):**
* With no runtime present, the extension behaves **exactly** as the offline build (features silently absent, no errors).
* No zombie processes under rapid edits; evaluation never blocks the UI.

---

## EPIC-008: Image-Grade Workbench (System Browser, Search & Refactoring)

* **ID:** EPIC-008
* **Status:** Planned (milestones **1.1–1.4**)
* **Priority:** High *(the layer that makes it "feel like Smalltalk" — still offline)*
* **Phase:** Phase 3
* **Date Proposed:** 2026-06-22
* **Owner:** Leonardo Nascimento

**Goal / Value Proposition:**
> Deliver the *browsing soul* of Smalltalk — the System Browser muscle memory — over the offline Knowledge Graph, with **no running image**. Where EPIC-007 captures the *execution* soul (and needs a runtime), this captures the *navigation/cross-reference* soul (and does not). This is the unique "omniscient cross-reference, instantly, offline" experience that image-bound tools can only offer against a live VM.

**Scope & Description:**
* **System Browser view:** a VS Code-native tree (packages/namespaces → classes → protocols → methods) over the workspace + cartridge index, with senders/implementors/references panes — the file-based equivalent of the classic multi-pane System Browser.
* **Full-text method search** across workspace + cartridge (extends `workspace/symbol`).
* **Class-hierarchy view** (super/subclasses, including kernel chains from the cartridge).
* **Refactorings:** scope-aware rename first (US-426, lands 1.0), then extract method / extract temp and other AST-driven rewrites; selector rename across the system treated with extreme caution (dynamic dispatch).

**Target Users:**
* All users — this is the "real IDE" layer that closes the experiential gap with image-based Smalltalk environments while staying zero-runtime.

**Related User Stories:**
* US-801 (#77): System Browser tree view (over the Console)
* US-802 (#78): Full-text method search
* US-803 (#79): Class-hierarchy view
* US-804 (#80): Extract-method / extract-temp refactorings (builds on US-426 scope rename)

**Success Metrics (Optional):**
* Senders/implementors/references browsing over kernel + workspace resolves offline in < 10 ms typical.
* The browser works identically across every loaded cartridge/dialect (EPIC-006).

---

