# vscode-smalltalk Epics

---

## EPIC-001: Improve User Onboarding & Documentation

* **ID:** EPIC-001
* **Status:** Defined
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
* **Status:** Proposed
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
* **Status:** Proposed
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

## EPIC-004: LSP Backend Technical Investigation

* **ID:** EPIC-004
* **Status:** Proposed
* **Priority:** Medium
* **Phase:** Phase 1
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento 

**Goal / Value Proposition:**
> To de-risk the Phase 2 LSP implementation by performing upfront technical research and prototyping for the Smalltalk LSP backend and its integration with the VS Code client. This ensures a smoother transition to building language intelligence features.

**Scope & Description:**
* Research and evaluate potential Smalltalk libraries (in GST and/or Pharo) for handling JSON-RPC communication.
* Investigate and prototype methods for spawning, managing (lifecycle, errors), and communicating (via stdio) with a Smalltalk LSP server process from the TypeScript extension client (`vscode-languageclient`).
* Define the specific data format and conventions for JSON-RPC messages between client and server.
* Research best approaches for parsing/analyzing `.st` files within the chosen Smalltalk environment for LSP purposes (e.g., leveraging compiler tools, reflection).
* Produce findings, recommendations, and potentially basic proof-of-concept code.

**Target Users:**
* Development Team, Architect.

**Related User Stories:**
* [US-401: Research Smalltalk JSON-RPC Libraries]
* [US-402: Prototype LSP Process Management & Communication]
* [US-403: Research Smalltalk Code Analysis Techniques for LSP]

**Success Metrics (Optional):**
* Clear recommendations for libraries and communication mechanisms.
* Working prototype of client-server process management and basic communication.
* Reduced uncertainty for Phase 2 planning and estimation.

---

