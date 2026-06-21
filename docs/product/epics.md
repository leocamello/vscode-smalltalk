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

