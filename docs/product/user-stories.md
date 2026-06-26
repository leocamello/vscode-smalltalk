# vscode-smalltalk User Stories

> **Status summary (2026-06-26).** v0.2.0–v0.9.0 are shipped. **Done:** US-101–106 & US-200–203 (declarative foundation, v0.2.0), US-301 (Run Current File, v0.3.0), US-410 (LSP scaffold, v0.3.0), **US-411** (error-tolerant parser + symbol table, internal milestone M3), **US-412** (outline + workspace symbols + go-to-definition, v0.4.0), **US-417** (semantic folding + scope-aware document highlight, v0.4.1), and **US-413** (completion + GNU Smalltalk kernel index, **v0.5.0** — closes #1), and **US-414** (diagnostics — **parser-only**: live squiggles + insert-closer/close-string quick fixes; the opt-in `gst` compile tier was deferred to EPIC-007 / **US-705**, **v0.6.0**), and **US-415** (hover — selectors/classes/variables/literals + provenance-gated comment prose; radix-integer coloring fix, **v0.7.0** — closes #27), and **US-422** (cartridge-aware semantic tokens — role-accurate highlighting + the offline known-class vs unknown-global distinction; the first user-facing cartridge consumer, **v0.8.0** — closes #65), and **US-423** (references + senders/implementors + call hierarchy over the two-tier engine; honest lexical union; installed-tier parity + real-source navigation, **v0.9.0** — closes #66). **Next:** **US-416** (formatting, EPIC-004, → ~1.0) and **US-425** (signature help, EPIC-005); **SPIKE-01** (unknown-selector heuristic). **Planned:** US-416. **Backlog (0.5.0 plan reconciliation):** US-418 (dialect seam, deferred), US-419 (kernel categories), US-420 (completion pseudo-variables), US-421 (CI kernel fixtures), US-901 (0.10.0 hardening/perf), US-902 (1.0.0 polish + Open VSX). **Superseded by [ADR-0001](../decisions/0001-typescript-bundled-lsp-server.md):** US-401–403 (the server is TypeScript, not Smalltalk). The per-story `Status` fields below reflect this; see [`docs/ROADMAP.md`](../ROADMAP.md) for the live milestone view. **EPIC-005 (Offline Knowledge Graph / "Console & Cartridges")** opens the next arc: US-430 (cartridge schema + GST Cartridge #01 reflective exporter), US-422 (cartridge-aware semantic tokens), US-423 (references/senders/implementors, two-tier engine), SPIKE-01 (unknown-selector heuristic), and US-431 (kernel-sourcing transparency — installed version label + settings UX, deferred from US-430). The post-1.0 vision continues in **EPIC-006** (multi-dialect — US-601–603 + US-418, plus **US-424 read-only Tonel — the "Trojan Horse" wedge pulled forward to ~1.0**), **EPIC-007** (optional Live Bridge — US-701–706, incl. **US-705 runtime compile/semantic diagnostics deferred from US-414** and **US-706 writable `smalltalk-image://` VFS**) and **EPIC-008** (image-grade workbench — US-426, US-801–804). Three ideas from a 2026-06-24 strategy review are folded in: US-424 (Tonel-first), US-706 (live-image VFS), and a status-bar dialect picker on US-602.

---

## US-101: Revamp README Structure & Introduction

* **ID:** US-101
* **Status:** Done (v0.2.0 — documentation)
* **Epic:** EPIC-001
* **Priority:** High
* **Estimate:** S
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **potential user or new user** of the `vscode-smalltalk` extension, I want **a well-structured README with a clear introduction** so that **I can quickly understand the extension's purpose, value proposition, and find relevant information easily.**

**Acceptance Criteria (AC):**
* AC1: The README has standard sections like Introduction, Prerequisites, Quick Start, Features, Configuration, Commands (if any), Troubleshooting, Contributing, License.
* AC2: The Introduction clearly states the extension's goal (Smalltalk support in VS Code) and primary target (GNU Smalltalk, file-based workflow).
* AC3: The README uses clear headings, formatting (like lists, code blocks), and potentially badges (like Marketplace version/installs) for readability, similar to popular extensions (e.g., Java, Python, Go).
* AC4: The overall structure is logical and easy to navigate.

**Definition of Ready (DoR) Checklist:**
* [X] Story clearly defines user value (Why).
* [X] Story has clear scope (What).
* [X] Acceptance Criteria are defined, clear, and testable.
* [X] Dependencies identified and manageable (N/A for pure doc).
* [X] Technical approach is feasible (N/A for pure doc).
* [X] Story is estimated/sized.
* [X] Team understands the story.

**Definition of Done (DoD) Checklist:**
* [ ] Code implemented & follows style guides (TypeScript & Smalltalk as applicable).
* [ ] Code successfully reviewed & merged to main branch.
* [ ] **TypeScript Client:** Unit tests written & passing (covering ACs).
* [ ] **Language Server (TypeScript):** Unit/Integration tests written & passing (if applicable to this story).
* [ ] **End-to-End:** Integration tests (`@vscode/test-cli`/`@vscode/test-electron`) written & passing (if applicable to this story).
* [ ] All Acceptance Criteria demonstrably met.
* [ ] Performance meets requirements (if applicable).
* [ ] Documentation updated (README, code comments, JSDoc/TSDoc, architecture docs).
* [ ] Feature works in target environment (e.g., VS Code with GNU Smalltalk).
* [ ] PO accepts the completed story.

**Notes / Questions / Assumptions:**
* We should look at the READMEs of extensions like `redhat.java`, `ms-python.python`, `golang.go` as primary examples for structure and style.

---

## US-102: Add Clear Prerequisites Section to README

* **ID:** US-102
* **Status:** Done (v0.2.0 — documentation)
* **Epic:** EPIC-001
* **Priority:** High
* **Estimate:** S
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **new user**, I want **a clear "Prerequisites" section in the README** so that **I know exactly what software (like GNU Smalltalk) I need to install and configure *before* using the extension effectively.**

**Acceptance Criteria (AC):**
```gherkin
Scenario: User checks prerequisites
  Given the user opens the README file
  When the user navigates to the "Prerequisites" section
  Then the section clearly lists "GNU Smalltalk" as a requirement
  And the section provides a link to the official GNU Smalltalk website or installation guide
  And the section mentions the need for `gst` to be accessible (e.g., in the system PATH or configured via a setting - anticipate US-105)
  And the section specifies any minimum supported version of GNU Smalltalk (if applicable/known).
```

* AC5: The section is easy to find within the README structure (defined in US-101).

**Definition of Ready (DoR) Checklist:**
* [X] Story clearly defines user value (Why).
* [X] Story has clear scope (What).
* [X] Acceptance Criteria are defined, clear, and testable.
* [X] Dependencies identified and manageable (Requires knowing if a setting exists/will exist for `gst` path).
* [X] Technical approach is feasible (N/A for pure doc).
* [X] Story is estimated/sized.
* [X] Team understands the story.

**Definition of Done (DoD) Checklist:**
* [ ] Code implemented & follows style guides (TypeScript & Smalltalk as applicable).
* [ ] Code successfully reviewed & merged to main branch.
* [ ] **TypeScript Client:** Unit tests written & passing (covering ACs).
* [ ] **Language Server (TypeScript):** Unit/Integration tests written & passing (if applicable to this story).
* [ ] **End-to-End:** Integration tests (`@vscode/test-cli`/`@vscode/test-electron`) written & passing (if applicable to this story).
* [ ] All Acceptance Criteria demonstrably met.
* [ ] Performance meets requirements (if applicable).
* [ ] Documentation updated (README, code comments, JSDoc/TSDoc, architecture docs).
* [ ] Feature works in target environment (e.g., VS Code with GNU Smalltalk).
* [ ] PO accepts the completed story.

**Notes / Questions / Assumptions:**
* Decision (PO & Architect): The configuration setting for the GNU Smalltalk executable path will be `smalltalk.gnuSmalltalkPath`. This needs to be documented in this story and US-105, anticipating implementation in EPIC-003.
* Decision (PO & Architect): For the minimum GNU Smalltalk version, the README should state that the latest stable version is recommended, but no specific minimum is enforced for current features. Future features (like LSP) might introduce minimum version requirements later.

---

## US-103: Create Quick Start Guide in README

* **ID:** US-103
* **Status:** Done (v0.2.0 — documentation)
* **Epic:** EPIC-001
* **Priority:** High
* **Estimate:** M
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **new user**, I want **a simple "Quick Start" guide in the README** so that **I can quickly get the extension installed and see it working with a basic Smalltalk file.**

**Acceptance Criteria (AC):**
```gherkin
Scenario: User follows Quick Start guide
  Given the user has met the prerequisites (installed GNU Smalltalk)
  And the user opens the README file
  When the user follows the steps in the "Quick Start" section
  Then the steps include installing the `vscode-smalltalk` extension from the Marketplace
  And the steps include opening a folder/workspace in VS Code
  And the steps include creating a simple `.st` file (e.g., with a basic Transcript show: 'Hello')
  And the steps demonstrate that basic syntax highlighting is active on the `.st` file
  And the guide explains how to ensure 'gst' is accessible (checking system PATH or configuring the 'smalltalk.gnuSmalltalkPath' setting in VS Code) for current and future features
  And the guide briefly shows where/how to configure the 'smalltalk.gnuSmalltalkPath' setting if needed.
```

* AC6: The guide is concise and focuses on the absolute minimum to get started.
* AC7: The guide uses clear, numbered steps.

**Definition of Ready (DoR) Checklist:**
* [X] Story clearly defines user value (Why).
* [X] Story has clear scope (What).
* [X] Acceptance Criteria are defined, clear, and testable.
* [X] Dependencies identified and manageable (Depends on US-102 for prerequisites, potentially US-105 for config setting).
* [X] Technical approach is feasible (N/A for pure doc).
* [X] Story is estimated/sized.
* [X] Team understands the story.

**Definition of Done (DoD) Checklist:**
* [ ] Code implemented & follows style guides (TypeScript & Smalltalk as applicable).
* [ ] Code successfully reviewed & merged to main branch.
* [ ] **TypeScript Client:** Unit tests written & passing (covering ACs).
* [ ] **Language Server (TypeScript):** Unit/Integration tests written & passing (if applicable to this story).
* [ ] **End-to-End:** Integration tests (`@vscode/test-cli`/`@vscode/test-electron`) written & passing (if applicable to this story).
* [ ] All Acceptance Criteria demonstrably met.
* [ ] Performance meets requirements (if applicable).
* [ ] Documentation updated (README, code comments, JSDoc/TSDoc, architecture docs).
* [ ] Feature works in target environment (e.g., VS Code with GNU Smalltalk).
* [ ] PO accepts the completed story.

**Notes / Questions / Assumptions:**
* The Quick Start should focus only on features available *now* (initially, just declarative features like syntax highlighting). It should be updated as new features (like LSP, commands) are added.

---

## US-104: Update Features List in README

* **ID:** US-104
* **Status:** Done (v0.2.0 — documentation)
* **Epic:** EPIC-001
* **Priority:** Medium
* **Estimate:** S
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **user evaluating or using the extension**, I want **an accurate and up-to-date "Features" list in the README** so that **I understand the current capabilities of the extension.**

**Acceptance Criteria (AC):**
* AC1: A "Features" section exists in the README.
* AC2: The list accurately reflects the state of the **declarative features targeted for completion in Phase 1** (Syntax highlighting, Snippets, Bracket handling, Commenting, Indentation, Basic Folding), assuming they have been successfully implemented/verified per EPIC-002.
* AC3: Features planned for Phase 2 (LSP features like Hover, Auto completion, Jump to definition, Error checking, Formatting, Refactoring, Semantic Folding) are **either omitted or clearly marked as "Future" or "Planned"**. Debugging/DAP features are not mentioned.
* AC4: The list is clear and concise, possibly using bullet points.
* AC5: (Optional) Include small screenshots or GIFs demonstrating key features if helpful.

**Definition of Ready (DoR) Checklist:**
* [X] Story clearly defines user value (Why).
* [X] Story has clear scope (What).
* [X] Acceptance Criteria are defined, clear, and testable.
* [X] Dependencies identified and manageable (Requires accurate knowledge of current features).
* [X] Technical approach is feasible (N/A for pure doc).
* [X] Story is estimated/sized.
* [X] Team understands the story.

**Definition of Done (DoD) Checklist:**
* [ ] Code implemented & follows style guides (TypeScript & Smalltalk as applicable).
* [ ] Code successfully reviewed & merged to main branch.
* [ ] **TypeScript Client:** Unit tests written & passing (covering ACs).
* [ ] **Language Server (TypeScript):** Unit/Integration tests written & passing (if applicable to this story).
* [ ] **End-to-End:** Integration tests (`@vscode/test-cli`/`@vscode/test-electron`) written & passing (if applicable to this story).
* [ ] All Acceptance Criteria demonstrably met.
* [ ] Performance meets requirements (if applicable).
* [ ] Documentation updated (README, code comments, JSDoc/TSDoc, architecture docs).
* [ ] Feature works in target environment (e.g., VS Code with GNU Smalltalk).
* [ ] PO accepts the completed story.

**Notes / Questions / Assumptions:**
* This story documents the *intended state* of features for the release incorporating Phase 1 work. It relies on the successful completion of work under EPIC-002 (Enhance Declarative Editing Features).
* This story needs to be revisited and updated whenever significant features are added or changed in subsequent phases/releases.

---

## US-105: Document Configuration Settings in README

* **ID:** US-105
* **Status:** Done (v0.2.0 — documentation)
* **Epic:** EPIC-001
* **Priority:** Medium
* **Estimate:** S
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **user**, I want **a "Configuration" section in the README listing available settings** so that **I know how I can customize the extension's behavior (e.g., specifying the path to `gst`).**

**Acceptance Criteria (AC):**
* AC1: A "Configuration" or "Settings" section exists in the README.
* AC2: The section lists all user-configurable settings provided by the extension.
* AC3: For each setting, the documentation includes:
    * The setting ID (e.g., `smalltalk.gnuSmalltalkPath`).
    * A clear description of what the setting does.
    * The type of value expected (e.g., string, boolean).
    * The default value.
* AC4: Instructions on *how* to change settings in VS Code (e.g., via `settings.json` or the Settings UI) are provided or linked to.
* AC5: Includes documentation for the setting smalltalk.gnuSmalltalkPath (as decided in US-102 review).

**Definition of Ready (DoR) Checklist:**
* [X] Story clearly defines user value (Why).
* [X] Story has clear scope (What).
* [X] Acceptance Criteria are defined, clear, and testable.
* [X] Dependencies identified and manageable (Requires knowing which settings actually exist or will exist soon).
* [X] Technical approach is feasible (N/A for pure doc).
* [X] Story is estimated/sized.
* [X] Team understands the story.

**Definition of Done (DoD) Checklist:**
* [ ] Code implemented & follows style guides (TypeScript & Smalltalk as applicable).
* [ ] Code successfully reviewed & merged to main branch.
* [ ] **TypeScript Client:** Unit tests written & passing (covering ACs).
* [ ] **Language Server (TypeScript):** Unit/Integration tests written & passing (if applicable to this story).
* [ ] **End-to-End:** Integration tests (`@vscode/test-cli`/`@vscode/test-electron`) written & passing (if applicable to this story).
* [ ] All Acceptance Criteria demonstrably met.
* [ ] Performance meets requirements (if applicable).
* [ ] Documentation updated (README, code comments, JSDoc/TSDoc, architecture docs).
* [ ] Feature works in target environment (e.g., VS Code with GNU Smalltalk).
* [ ] PO accepts the completed story.

**Notes / Questions / Assumptions:**
* Initially, the only setting to document will be `smalltalk.gnuSmalltalkPath`. This section will grow as more settings are added.
* Assumption: The setting `smalltalk.gnuSmalltalkPath` will be defined in the extension's `package.json` as part of Phase 1 work, making it discoverable and available for documentation, even if its functional implementation comes later.

---

## US-106: Add Basic Troubleshooting Section to README

* **ID:** US-106
* **Status:** Done (v0.2.0 — documentation)
* **Epic:** EPIC-001
* **Priority:** Low
* **Estimate:** S
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **user encountering a problem**, I want **a "Troubleshooting" section in the README** so that **I can find solutions to common issues or know how to report bugs effectively.**

**Acceptance Criteria (AC):**
* AC1: A "Troubleshooting" or "Known Issues" section exists in the README.
* AC2: The section provides guidance on checking basic setup (e.g., Is GNU Smalltalk installed correctly? Is the gst path configured via smalltalk.gnuSmalltalkPath or PATH?).
* AC3: The section explains where diagnostic logs would typically be found (e.g., VS Code Output Panel under a specific channel like 'Smalltalk Language Support') **or states that detailed logging is planned for future releases.**
* AC4: The section provides clear instructions on how to report issues (e.g., link to the GitHub Issues page).
* AC5: The section includes a subsection for 'Known Issues', **initially stating 'No common issues identified yet' (or similar) and linking to the GitHub Issues page for searching existing reports.**

**Definition of Ready (DoR) Checklist:**
* [X] Story clearly defines user value (Why).
* [X] Story has clear scope (What).
* [X] Acceptance Criteria are defined, clear, and testable.
* [X] Dependencies identified and manageable (Requires knowing common issues, logging mechanisms).
* [X] Technical approach is feasible (N/A for pure doc).
* [X] Story is estimated/sized.
* [X] Team understands the story.

**Definition of Done (DoD) Checklist:**
* [ ] Code implemented & follows style guides (TypeScript & Smalltalk as applicable).
* [ ] Code successfully reviewed & merged to main branch.
* [ ] **TypeScript Client:** Unit tests written & passing (covering ACs).
* [ ] **Language Server (TypeScript):** Unit/Integration tests written & passing (if applicable to this story).
* [ ] **End-to-End:** Integration tests (`@vscode/test-cli`/`@vscode/test-electron`) written & passing (if applicable to this story).
* [ ] All Acceptance Criteria demonstrably met.
* [ ] Performance meets requirements (if applicable).
* [ ] Documentation updated (README, code comments, JSDoc/TSDoc, architecture docs).
* [ ] Feature works in target environment (e.g., VS Code with GNU Smalltalk).
* [ ] PO accepts the completed story.

**Notes / Questions / Assumptions:**
* This section will evolve as logging is implemented and common issues are identified. The initial implementation focuses on setup checks, bug reporting, and placeholders for logs/known issues.

---

## US-200: Investigate GST Syntax & TextMate Grammar Implementation

* **ID:** US-200
* **Status:** Done (v0.2.0 — research)
* **Epic:** EPIC-002
* **Priority:** Medium
* **Estimate:** M
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento 

**User Story:**
> As a **developer working on the `vscode-smalltalk` extension**, I want **to thoroughly understand GNU Smalltalk's syntax rules by analyzing its source code (`lex.c`, `gst-parse.c`) and learn TextMate grammar best practices**, so that **I can accurately implement or refine the syntax highlighting grammar (US-201).**

**Acceptance Criteria (AC):**
* AC1: Relevant GNU Smalltalk source files (`lex.c`, `gst-parse.c` or others identified) have been analyzed to identify key tokenization and parsing rules for keywords, literals, identifiers, operators, comments, blocks, etc.
* AC2: Key findings and extracted syntax rules relevant to TextMate grammar scopes are documented.
* AC3: Best practices for developing maintainable and performant TextMate grammars (e.g., using `.YAML-tmLanguage`, scope naming conventions) have been researched and summarized.
* AC4: A representative collection of GNU Smalltalk code snippets demonstrating common constructs and potential edge cases has been gathered or created for testing purposes (to be used in US-201 validation).
* AC5: (Optional) A small proof-of-concept TextMate grammar snippet demonstrating the application of a specific complex rule derived from the C source analysis is created.
* AC6: A recommendation or summary outlining the approach for implementing US-201 based on the findings is produced.

**Definition of Ready (DoR) Checklist:**
* [X] Story clearly defines user value (Why - enables accurate highlighting).
* [X] Story has clear scope (What - research, analysis, documentation of findings).
* [X] Acceptance Criteria are defined, clear, and testable (via review of documentation, code examples, PoC).
* [X] Dependencies identified and manageable (Requires access to GNU Smalltalk source code, TextMate documentation).
* [X] Technical approach is feasible (Analysis and research).
* [X] Story is estimated/sized. 
* [X] Team understands the story. 

**Definition of Done (DoD) Checklist:**
* [ ] Code implemented & follows style guides (TypeScript & Smalltalk as applicable).
* [ ] Code successfully reviewed & merged to main branch.
* [ ] **TypeScript Client:** Unit tests written & passing (covering ACs).
* [ ] **Language Server (TypeScript):** Unit/Integration tests written & passing (if applicable to this story).
* [ ] **End-to-End:** Integration tests (`@vscode/test-cli`/`@vscode/test-electron`) written & passing (if applicable to this story).
* [ ] All Acceptance Criteria demonstrably met.
* [ ] Performance meets requirements (if applicable).
* [ ] Documentation updated (README, code comments, JSDoc/TSDoc, architecture docs).
* [ ] Feature works in target environment (e.g., VS Code with GNU Smalltalk).
* [ ] PO accepts the completed story.

**Notes / Questions / Assumptions:**
* This story is primarily research and preparation for US-201.
* Requires access to the GNU Smalltalk source code repository (specifically `lex.c`, `gst-parse.c`, and potentially related headers/files).
* Deliverable is primarily documentation and understanding, potentially some PoC code.
* This story must be completed before US-201 can be started. US-201 depends on US-200.

---

## US-201: Refine Syntax Highlighting Grammar

* **ID:** US-201
* **Status:** Done (v0.2.0)
* **Epic:** EPIC-002
* **Priority:** Medium
* **Estimate:** M (or L)
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Smalltalk developer using VS Code**, I want **accurate and distinct syntax highlighting for GNU Smalltalk constructs** in `.st` files, so that **I can easily read, understand, and identify errors in my code.**

**Acceptance Criteria (AC):**
* AC1: The TextMate grammar (`smalltalk.tmLanguage.json` or similar, potentially `.YAML-tmLanguage`) correctly identifies and assigns distinct scopes to standard GNU Smalltalk keywords (e.g., `self`, `super`, `true`, `false`, `nil`, `thisContext`), guided by the analysis from US-200.
* AC2: The grammar correctly identifies and scopes different types of literals (Numbers, Characters, Strings, Symbols, Byte Arrays, Literal Arrays), guided by the analysis from US-200.
* AC3: The grammar correctly identifies and scopes comments (double-quoted).
* AC4: The grammar correctly identifies and scopes different types of identifiers (Local variables, Instance variables, Class variables, Global variables / Class names), guided by the analysis from US-200.
* AC5: The grammar correctly identifies and scopes operators (binary, keyword, assignment `:=`), guided by the analysis from US-200.
* AC6: The grammar correctly identifies block syntax (`[...]`) and distinguishes block arguments (`:arg |`), guided by the analysis from US-200.
* AC7: The grammar correctly handles primitive calls (`<primitive: ...>`).
* AC8: Highlighting remains accurate for nested constructs and edge cases identified during US-200.
* AC9: Validation against the curated set of GNU Smalltalk code examples gathered in US-200 demonstrates correct highlighting.

**Definition of Ready (DoR) Checklist:**
* [X] Story clearly defines user value (Why).
* [X] Story has clear scope (What - the `.tmLanguage.json` file).
* [X] Acceptance Criteria are defined, clear, and testable (via inspection and test files).
* [ ] **Dependencies identified and manageable (Blocked pending completion of US-200).**
* [X] Technical approach is feasible (Requires TextMate grammar knowledge, guided by US-200).
* [X] Story is estimated/sized.
* [X] Team understands the story.

**Definition of Done (DoD) Checklist:**
* [ ] Code implemented & follows style guides (TypeScript & Smalltalk as applicable).
* [ ] Code successfully reviewed & merged to main branch.
* [ ] **TypeScript Client:** Unit tests written & passing (covering ACs).
* [ ] **Language Server (TypeScript):** Unit/Integration tests written & passing (if applicable to this story).
* [ ] **End-to-End:** Integration tests (`@vscode/test-cli`/`@vscode/test-electron`) written & passing (if applicable to this story).
* [ ] All Acceptance Criteria demonstrably met.
* [ ] Performance meets requirements (if applicable).
* [ ] Documentation updated (README, code comments, JSDoc/TSDoc, architecture docs).
* [ ] Feature works in target environment (e.g., VS Code with GNU Smalltalk).
* [ ] PO accepts the completed story.

**Notes / Questions / Assumptions:**
* **Dependency:** This story **cannot start** until US-200 (Investigate GST Syntax & TextMate Grammar Implementation) is completed and its findings are available.
* Implementation should be directly guided by the analysis of `lex.c`/`gst-parse.c` and TextMate best practices documented in US-200.
* Requires expertise in writing TextMate grammars (.tmLanguage.json or .YAML-tmLanguage).
* Testing will use the suite of `.st` files gathered in US-200. Automated scope inspection via tests is recommended.
* Estimate 'M' or 'L' reflects that TextMate grammars can be complex, even with guidance.

---

## US-202: Create/Update Code Snippets

* **ID:** US-202
* **Status:** Done (v0.2.0)
* **Epic:** EPIC-002
* **Priority:** Medium
* **Estimate:** S
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento **User Story:**
> As a **Smalltalk developer using VS Code**, I want **access to useful code snippets for common GNU Smalltalk patterns**, so that **I can write code faster and with fewer errors.**

**Acceptance Criteria (AC):**
* AC1: A `snippets.json` (or similar, following VS Code conventions) file exists and is correctly configured in `package.json` to provide snippets for the `smalltalk` language scope.
* AC2: Snippets are provided for fundamental constructs frequently used in file-based GNU Smalltalk, including: Basic method definition (with placeholders for selector, arguments, temporaries, body), Common block structures (e.g., `do:`, `collect:`, `select:`, `ifTrue:`, `ifFalse:`, `ifTrue:ifFalse:`), and Temporary variable declaration (`| temp |`).
* AC3: Each snippet has a clear, concise `prefix` that is intuitive to type.
* AC4: Each snippet has a meaningful `description` explaining what the snippet does, visible in the IntelliSense suggestion list.
* AC5: Snippets utilize VS Code snippet syntax correctly, including placeholders (`$1`, `$2`, `${1:placeholder}`), tab stops, and choices where appropriate, to guide the user.
* AC6: The provided snippets are idiomatic for common GNU Smalltalk usage.

**Definition of Ready (DoR) Checklist:**
* [X] Story clearly defines user value (Why).
* [X] Story has clear scope (What - the `snippets.json` file and its content).
* [X] Acceptance Criteria are defined, clear, and testable (via inspection and testing snippet activation).
* [X] Dependencies identified and manageable (Requires knowledge of common GST patterns, VS Code snippet syntax).
* [X] Technical approach is feasible (Creating JSON definitions).
* [X] Story is estimated/sized.
* [X] Team understands the story.

**Definition of Done (DoD) Checklist:**
* [ ] Code implemented & follows style guides (TypeScript & Smalltalk as applicable).
* [ ] Code successfully reviewed & merged to main branch.
* [ ] **TypeScript Client:** Unit tests written & passing (covering ACs).
* [ ] **Language Server (TypeScript):** Unit/Integration tests written & passing (if applicable to this story).
* [ ] **End-to-End:** Integration tests (`@vscode/test-cli`/`@vscode/test-electron`) written & passing (if applicable to this story).
* [ ] All Acceptance Criteria demonstrably met.
* [ ] Performance meets requirements (if applicable).
* [ ] Documentation updated (README, code comments, JSDoc/TSDoc, architecture docs).
* [ ] Feature works in target environment (e.g., VS Code with GNU Smalltalk).
* [ ] PO accepts the completed story.

**Notes / Questions / Assumptions:**
* The initial set of snippets in AC2 can be expanded later based on user feedback. Class definition snippet deferred.
* Requires familiarity with VS Code snippet syntax: [https://code.visualstudio.com/docs/editor/userdefinedsnippets](https://code.visualstudio.com/docs/editor/userdefinedsnippets)
* A consistent prefix convention should be decided during implementation (e.g., all start with `st-`?).

---

## US-203: Enhance Language Configuration

* **ID:** US-203
* **Status:** Done (v0.2.0)
* **Epic:** EPIC-002
* **Priority:** Medium
* **Estimate:** S
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento 

**User Story:**
> As a **Smalltalk developer using VS Code**, I want **reliable language configuration for features like bracket matching, comment toggling, and auto-indentation**, so that **basic code editing is intuitive and efficient.**

**Acceptance Criteria (AC):**
* AC1: A `language-configuration.json` file exists and is correctly referenced in `package.json` for the `smalltalk` language.
* AC2: The configuration correctly defines standard bracket pairs for matching and auto-closing/surrounding: `[]`, `()`, `{}`.
* AC3: The configuration correctly defines the block comment characters (`"`, `"`) enabling correct comment toggling (e.g., using `Ctrl+/` or `Cmd+/`).
* AC4: Basic auto-indentation rules are defined to provide minimally helpful indentation (e.g., indenting within `[]` blocks), acknowledging limitations for complex Smalltalk syntax.
* AC5: Auto-closing pairs are configured for quotes (`"`, `'`) and brackets (`[`, `(`, `{`).

**Definition of Ready (DoR) Checklist:**
* [X] Story clearly defines user value (Why).
* [X] Story has clear scope (What - the `language-configuration.json` file).
* [X] Acceptance Criteria are defined, clear, and testable (via editor interaction).
* [X] Dependencies identified and manageable (Requires knowledge of VS Code language configuration options and common GST editing conventions).
* [X] Technical approach is feasible (Creating/editing JSON).
* [X] Story is estimated/sized.
* [X] Team understands the story.

**Definition of Done (DoD) Checklist:**
* [ ] Code implemented & follows style guides (TypeScript & Smalltalk as applicable).
* [ ] Code successfully reviewed & merged to main branch.
* [ ] **TypeScript Client:** Unit tests written & passing (covering ACs).
* [ ] **Language Server (TypeScript):** Unit/Integration tests written & passing (if applicable to this story).
* [ ] **End-to-End:** Integration tests (`@vscode/test-cli`/`@vscode/test-electron`) written & passing (if applicable to this story).
* [ ] All Acceptance Criteria demonstrably met.
* [ ] Performance meets requirements (if applicable).
* [ ] Documentation updated (README, code comments, JSDoc/TSDoc, architecture docs).
* [ ] Feature works in target environment (e.g., VS Code with GNU Smalltalk).
* [ ] PO accepts the completed story.

**Notes / Questions / Assumptions:**
* Focus is on the *declarative* language configuration provided by VS Code. More advanced indentation or folding logic will be handled by an LSP (Phase 2).
* Indentation rules (AC4) should be kept simple; perfect Smalltalk indentation is not expected from this configuration alone.
* Declarative folding markers are explicitly out of scope; semantic folding will be addressed via LSP.
* Requires familiarity with VS Code Language Configuration: [https://code.visualstudio.com/api/language-extensions/language-configuration-guide](https://code.visualstudio.com/api/language-extensions/language-configuration-guide)

---

## US-301: Implement 'Run Current File' Command

* **ID:** US-301
* **Status:** Done (v0.3.0)
* **Epic:** EPIC-003
* **Priority:** Low
* **Estimate:** M
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento

**User Story:**

> As a **Smalltalk developer**, I want **a command to run my current `.st` file using the configured GNU Smalltalk interpreter**, so that **I can quickly execute and test my code without leaving VS Code.**

**Acceptance Criteria (AC):**

* AC1: A VS Code command with the ID `smalltalk.runCurrentFile` (or similar) and title "Smalltalk: Run Current File" is registered by the extension.
* AC2: The command is accessible and searchable via the Command Palette (Ctrl+Shift+P / Cmd+Shift+P).
* AC3: The command is only enabled/visible when the active editor contains a file with the `.st` extension (via `when` clause in `package.json`).
* AC4: The extension defines the configuration setting `smalltalk.gnuSmalltalkPath` in its `package.json` contributions (string type, description explaining its purpose).
* AC5: When the command is executed, it first attempts to resolve the path to the `gst` executable by reading the `smalltalk.gnuSmalltalkPath` setting.
* AC6: If the `smalltalk.gnuSmalltalkPath` setting is empty or not configured, the extension attempts to find `gst` in the system's PATH environment variable.
* AC7: If `gst` cannot be found via setting or PATH, an informative error message is displayed to the user (e.g., using `vscode.window.showErrorMessage`) guiding them to configure the setting.
* AC8: If `gst` is found, the command executes the currently active `.st` file using the resolved `gst` path (e.g., by running `<gst_path> <current_file_path>`).
* AC9: The execution occurs within the **VS Code Integrated Terminal** (using `vscode.window.createTerminal`), ensuring the execution context is clearly associated with the action.
* AC10: Standard output (`stdout`) and standard error (`stderr`) from the `gst` process are displayed clearly within the Integrated Terminal.
* AC11: The command correctly handles file paths, including those containing spaces, when passing the path to `gst`.

**Definition of Ready (DoR) Checklist:**

* [X] Story clearly defines user value (Why).
* [X] Story has clear scope (What - the run command functionality).
* [X] Acceptance Criteria are defined, clear, and testable.
* [X] Dependencies identified and manageable (Requires VS Code API knowledge for commands, settings, terminal, process execution).
* [X] Technical approach is feasible (Using standard VS Code APIs, specifically Integrated Terminal).
* [X] Story is estimated/sized.
* [X] Team understands the story.

**Definition of Done (DoD) Checklist:** *(Using updated template)*

* [ ] `smalltalk.gnuSmalltalkPath` setting defined in `package.json`.
* [ ] `smalltalk.runCurrentFile` command implemented in TypeScript.
* [ ] Code successfully reviewed & merged to main branch.
* [ ] **TypeScript Client:** Unit tests written & passing (covering setting reading, path resolution, error handling logic, command registration).
* [ ] **Language Server (TypeScript):** Unit/Integration tests written & passing (N/A for this story).
* [ ] **End-to-End:** Integration tests (`@vscode/test-cli`/`@vscode/test-electron`) written & passing (Verifying command execution, terminal output, error messages for missing `gst`).
* [ ] All Acceptance Criteria demonstrably met.
* [ ] Performance meets requirements (Command execution is reasonably fast).
* [ ] Documentation updated (README Features/Commands list, Configuration section for the new setting).
* [ ] Feature works in target environment (VS Code with `.st` files and configured/discoverable `gst`).
* [ ] PO accepts the completed story.

**Notes / Questions / Assumptions:**

* This story implements the functional aspect of the `smalltalk.gnuSmalltalkPath` setting documented in US-105.
* Requires careful implementation using VS Code APIs (`workspace.getConfiguration`, `commands.registerCommand`, `window.createTerminal`, potentially Node.js `child_process` for path handling/validation, handling platform differences for path resolution).
* Error handling for `gst` execution failures (e.g., syntax errors in the `.st` file) should be surfaced via the `stderr` output in the terminal (AC10).
* Decision confirmed: Use Integrated Terminal for output (AC9).

---

## US-401: Research Smalltalk JSON-RPC Libraries

* **ID:** US-401
* **Status:** Superseded (2026-06-13) — server is now TypeScript, no custom JSON-RPC. Closed as GitHub issue #14. See EPIC-004 and US-410.
* **Epic:** EPIC-004
* **Priority:** Medium
* **Estimate:** M *(Adjusted based on architect feedback)*
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **developer on the LSP team**, I want **to identify and evaluate potential libraries in GNU Smalltalk and Pharo for handling JSON-RPC communication**, so that **we can select the best foundation for the LSP server backend.**

**Acceptance Criteria (AC):**
* AC1: A search for existing JSON-RPC libraries/frameworks suitable for use within GNU Smalltalk and Pharo environments is conducted.
* AC2: Potential candidates are documented, noting their features, maturity, dependencies, licensing, and ease of integration.
* AC3: A comparative analysis (pros/cons) of the top candidates is documented.
* AC4: A recommendation for the preferred library/approach for handling JSON-RPC in the Smalltalk LSP server is provided, with justifications.

**Definition of Ready (DoR) Checklist:**
* [X] Story clearly defines user value (Why - informs core technical choice).
* [X] Story has clear scope (What - research JSON-RPC libs).
* [X] Acceptance Criteria are defined, clear, and testable (via review of research document).
* [X] Dependencies identified and manageable (Access to Smalltalk package repositories/documentation).
* [X] Technical approach is feasible (Research).
* [X] Story is estimated/sized.
* [X] Team understands the story.

**Definition of Done (DoD) Checklist:** *(Using updated template)*
* [ ] Research conducted and findings documented.
* [ ] Analysis and recommendation documented.
* [ ] **TypeScript Client:** Unit tests written & passing (N/A).
* [ ] **Language Server (TypeScript):** Unit/Integration tests written & passing (N/A).
* [ ] **End-to-End:** Integration tests (`@vscode/test-cli`/`@vscode/test-electron`) written & passing (N/A).
* [ ] All Acceptance Criteria demonstrably met (Research document produced and reviewed).
* [ ] Performance meets requirements (N/A).
* [ ] Documentation updated (Internal design/research notes).
* [ ] Feature works in target environment (N/A).
* [ ] PO accepts the completed story (based on research deliverables).

**Notes / Questions / Assumptions:**
* Focus is on libraries suitable for building an LSP server (handling requests/responses over stdio).

---

## US-402: Prototype LSP Process Management & Communication

* **ID:** US-402
* **Status:** Superseded (2026-06-13) — bundled TypeScript server, no external Smalltalk process to manage. Closed as GitHub issue #15. See EPIC-004 and US-410.
* **Epic:** EPIC-004
* **Priority:** Medium
* **Estimate:** M
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **developer on the LSP team**, I want **to prototype spawning, managing the lifecycle (start/stop/error), and communicating via stdio with a basic Smalltalk process from the TypeScript VS Code extension client**, so that **we can establish a reliable client-server interaction mechanism for the LSP.**

**Acceptance Criteria (AC):**
* AC1: A TypeScript prototype within the extension structure demonstrates spawning a simple Smalltalk script/process (e.g., one that echoes input).
* AC2: The prototype uses `vscode-languageclient` or appropriate VS Code APIs to manage the lifecycle of the spawned process (start, stop on extension deactivate).
* AC3: The prototype demonstrates sending a simple message (e.g., a JSON string) from the TS client to the Smalltalk process via its `stdin`.
* AC4: The prototype demonstrates receiving a simple message from the Smalltalk process's `stdout` back to the TS client.
* AC5: Basic error handling for process spawning failures is demonstrated.
* AC6: Findings regarding challenges, best practices, and necessary configurations for reliable client-server process management and stdio communication are documented.

**Definition of Ready (DoR) Checklist:**
* [X] Story clearly defines user value (Why - verifies core interaction mechanism).
* [X] Story has clear scope (What - TS client prototype for process mgmt/stdio).
* [X] Acceptance Criteria are defined, clear, and testable (via code review and running prototype).
* [X] Dependencies identified and manageable (Requires basic TS/Node.js process knowledge, simple Smalltalk script).
* [X] Technical approach is feasible (Prototyping).
* [X] Story is estimated/sized.
* [X] Team understands the story.

**Definition of Done (DoD) Checklist:** *(Using updated template)*
* [ ] TypeScript prototype code implemented.
* [ ] Simple Smalltalk script for testing created.
* [ ] Code successfully reviewed & potentially merged to a feature/spike branch.
* [ ] **TypeScript Client:** Unit tests written & passing (Potentially for helper functions used in prototype).
* [ ] **Language Server (TypeScript):** Unit/Integration tests written & passing (N/A - simple script).
* [ ] **End-to-End:** Integration tests (`@vscode/test-cli`/`@vscode/test-electron`) written & passing (Demonstrating prototype runs within VS Code context).
* [ ] All Acceptance Criteria demonstrably met (Prototype works as described).
* [ ] Performance meets requirements (N/A for prototype).
* [ ] Documentation updated (Documented findings, prototype code comments).
* [ ] Feature works in target environment (Prototype runs in VS Code).
* [ ] PO accepts the completed story (based on prototype and findings).

**Notes / Questions / Assumptions:**
* Focus is on the client-side mechanics and basic stdio plumbing, not full LSP implementation.
* The Smalltalk process can be extremely simple for this prototype (e.g., read line, print line).

---

## US-403: Research Smalltalk Code Analysis Techniques for LSP

* **ID:** US-403
* **Status:** Superseded (2026-06-13) — replaced by US-411 (hand-written TypeScript parser + symbol table). Closed as GitHub issue #16. See EPIC-004.
* **Epic:** EPIC-004
* **Priority:** Medium
* **Estimate:** M
* **Date Proposed:** 2025-05-02
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **developer on the LSP team**, I want **to research and document methods for parsing and analyzing Smalltalk code within GNU Smalltalk (and potentially Pharo) to extract semantic information needed for LSP features** (like completion, go-to-definition, diagnostics), so that **we can choose an effective strategy for the LSP server's core logic.**

**Acceptance Criteria (AC):**
* AC1: Research conducted into GNU Smalltalk's (and optionally Pharo's) built-in capabilities for code parsing (e.g., `Compiler` class, related tools).
* AC2: Research conducted into reflection capabilities for inspecting classes, methods, variables, etc.
* AC3: Potential external libraries or tools for Smalltalk code analysis are identified (if any).
* AC4: A document is produced summarizing the available techniques for obtaining semantic information (ASTs, symbol tables, scopes, types if available) needed for key LSP features (completion, hover, definition, diagnostics).
* AC5: The pros, cons, and potential challenges of each technique (e.g., performance, accuracy, completeness) are discussed.
* AC6: A preliminary recommendation on the most promising approach(es) for implementing the core analysis logic of the LSP server is provided.

**Definition of Ready (DoR) Checklist:**
* [X] Story clearly defines user value (Why - informs LSP core logic strategy).
* [X] Story has clear scope (What - research analysis techniques).
* [X] Acceptance Criteria are defined, clear, and testable (via review of research document).
* [X] Dependencies identified and manageable (Access to GST/Pharo documentation, potentially environments for experimentation).
* [X] Technical approach is feasible (Research).
* [X] Story is estimated/sized.
* [X] Team understands the story.

**Definition of Done (DoD) Checklist:** *(Using updated template)*
* [ ] Research conducted and findings documented.
* [ ] Analysis and recommendation documented.
* [ ] **TypeScript Client:** Unit tests written & passing (N/A).
* [ ] **Language Server (TypeScript):** Unit/Integration tests written & passing (N/A - research).
* [ ] **End-to-End:** Integration tests (`@vscode/test-cli`/`@vscode/test-electron`) written & passing (N/A).
* [ ] All Acceptance Criteria demonstrably met (Research document produced and reviewed).
* [ ] Performance meets requirements (N/A).
* [ ] Documentation updated (Internal design/research notes).
* [ ] Feature works in target environment (N/A).
* [ ] PO accepts the completed story (based on research deliverables).

**Notes / Questions / Assumptions:**
* Focus is on *how* to get the necessary semantic information out of the Smalltalk environment to serve LSP requests.
* Does not require implementing the analysis, only researching the methods.

---

## US-410: TypeScript LSP Scaffold (Client + Bundled Server)

* **ID:** US-410
* **Status:** Done (v0.3.0)
* **Epic:** EPIC-004
* **Priority:** Medium
* **Estimate:** M
* **Date Proposed:** 2026-06-13
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **developer**, I want **a TypeScript extension client and a bundled `vscode-languageserver-node` server wired together with esbuild**, so that **language features can be built on a server that ships inside the VSIX and runs without any external Smalltalk installation.**

**Acceptance Criteria (AC):**
* AC1: The repo is restructured into `client/` and `server/` (npm workspaces) with a shared strict `tsconfig`.
* AC2: esbuild bundles `dist/extension.js` and `dist/server.js`; `vscode:prepublish` builds both plus the grammar.
* AC3: The client starts the bundled server over IPC with `documentSelector` for `smalltalk`; the server restarts on crash.
* AC4: A no-op server (capabilities only) connects and the client reports it running via a trace setting.
* AC5: The server requires no `gst`.

**Definition of Ready (DoR) Checklist:**
* [X] Story clearly defines user value (foundation for all LSP features).
* [X] Clear scope (scaffold only, no features).
* [X] Acceptance Criteria are testable (`@vscode/test-cli` integration).
* [X] Dependencies identified (esbuild, vscode-languageclient/server 9.x, engines ^1.82).
* [X] Estimated/sized.

**Definition of Done (DoD) Checklist:**
* [ ] Client + server scaffold implemented and bundled.
* [ ] **TypeScript Client:** Unit/integration tests written & passing.
* [ ] **Language Server:** Server boots and reports capabilities.
* [ ] **End-to-End:** `@vscode/test-cli` confirms client↔server handshake.
* [ ] Documentation updated (README dev setup).
* [ ] PO accepts the story.

**Notes / Questions / Assumptions:**
* Modeled on `microsoft/vscode-extension-samples` `lsp-sample`.

---

## US-411: Error-Tolerant Smalltalk Parser + Symbol Table

* **ID:** US-411
* **Status:** Done (M3 / internal milestone — foundation for v0.4.0). Delivered as 5 PRs: lexer (#31), expression parser (#32), GST brace containers (#33), GST chunk + primaries (#34), symbol table + recovery hardening (#35). Kernel smoke test green (122 files, 0 crashes, 0 diagnostics, 206 classes), satisfying the gate for US-412+.
* **Epic:** EPIC-004
* **Priority:** High
* **Estimate:** XL *(compiler front-end; the foundation for every feature)*
* **Date Proposed:** 2026-06-13
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **developer**, I want **a hand-written, error-tolerant lexer + recursive-descent parser producing an AST with positions and a per-document symbol table**, so that **all downstream LSP features have accurate semantic data even while code is being edited.**

**Acceptance Criteria (AC):**
* AC1: Lexer covers the GST token inventory (radix/scaled numbers, `$c`/`$<n>` chars, strings, symbols, `#(...)`/`#[...]`/`{...}`, scoped names, `#{...}`, shebang, chunk `!`), per `docs/research/gst-syntax/01-*`.
* AC2: Parser covers ANSI expressions (unary>binary>keyword precedence), cascades, blocks, method patterns, and primitives, per `docs/research/gst-syntax/02-*`.
* AC3: Container formats are pluggable: GST brace (`Object subclass: Foo [ ... ]`, `Foo class >> sel [ ... ]`) and GST chunk (`!Foo methodsFor: '...'! ... ! !`).
* AC4: The parser never throws; it recovers (synchronizing on `.`, `!`, `]`, method-pattern starts), emitting error nodes + a diagnostics list, and always returns an AST.
* AC5: A symbol table records classes, methods (selector + arity), instance/class/temp variables, and scopes.

**Definition of Ready (DoR) Checklist:**
* [X] Clear value (foundation for navigation/completion/diagnostics).
* [X] Scope bounded to ANSI + GST container formats (no Tonel).
* [X] Spec exists (`docs/research/gst-syntax/01-*`, `02-*`, `test-cases/*`).
* [X] Estimated/sized (XL).

**Definition of Done (DoD) Checklist:**
* [X] Lexer, parser, container layers, AST, and symbol table implemented (`server/src/parser/`).
* [X] **Language Server:** Snapshot AST/symbol tests reuse the 15 `test-cases/*.st` categories; mutation/garbage-input tests confirm no-throw recovery.
* [X] **Kernel smoke test:** all 122 `../smalltalk-3.2.5/kernel/*.st` parse with **0 crashes and 0 diagnostics** (`server/test/kernel.test.ts`).
* [X] Documentation updated — design notes in `specs/US-411-Parser-And-Symbol-Table/plan.md`.
* [X] PO accepts the story (foundation accepted; shipped behind v0.4.0 navigation).

**Notes / Questions / Assumptions:**
* Gate: no LSP feature work (US-412+) begins until the kernel smoke test passes. **(Met — gate cleared.)**

---

## US-412: Document/Workspace Symbols + Go-To-Definition

* **ID:** US-412
* **Status:** Done (v0.4.0). Delivered as 3 slices — documentSymbol (#36), workspace/symbol (#37), definition + freshness + Electron e2e (#38) — plus two manual-QA fixes (#39 `files.exclude` at runtime, #40 activate on `workspaceContains`). Manual matrix M1–M14 passed.
* **Epic:** EPIC-004
* **Priority:** Medium
* **Estimate:** L
* **Date Proposed:** 2026-06-13
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Smalltalk developer**, I want **an outline, workspace symbol search, and go-to-definition**, so that **I can navigate a codebase quickly without an external tool.**

**Acceptance Criteria (AC):**
* AC1: `textDocument/documentSymbol` returns a hierarchy (class → methods) for both chunk- and brace-format files.
* AC2: `workspace/symbol` finds classes and selectors across the workspace (scan `**/*.{st,gst}`, respecting `files.exclude`).
* AC3: `textDocument/definition` jumps from a message send to implementor candidates and from a class reference to its definition (returns all matches; Smalltalk is dynamically typed).
* AC4: Results update as files change (debounced parse cache).

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-411 (parser/symbols).
* [X] Acceptance Criteria testable via integration tests.
* [X] Estimated/sized.

**Definition of Done (DoD) Checklist:**
* [X] documentSymbol, workspace/symbol, definition implemented (`server/src/providers/`).
* [X] **Language Server:** unit tests for symbol extraction + provider mapping (`server/test/providers.test.ts`).
* [X] **End-to-End:** integration tests asserting outline/symbol/definition on a fixture workspace (`client/test-e2e/`, Electron; `npm run test:e2e`) + real-server LSP tests (`server/test/handshake.test.mjs`).
* [X] Works with no `gst` present (verified: navigation has zero `gst`/process dependency).
* [X] PO accepts the story (manual-QA matrix M1–M14 passed; shipped in v0.4.0).

**Notes / Questions / Assumptions:**
* Semantic `foldingRange` and `documentHighlight` are near-free bonuses if time allows. *(Deferred — not built in 0.4.0.)*

---

## US-413: Completion + GNU Smalltalk Kernel Index

* **ID:** US-413
* **Status:** Done — shipped in **v0.5.0** (slices A–D #51–#54, eval #55, release #56; manual-QA signed off; closes #1)
* **Epic:** EPIC-004
* **Priority:** High *(directly answers issue #1)*
* **Estimate:** L *(grown to XL by [ADR-0002](../decisions/0002-kernel-symbol-sourcing.md): adds live installed-kernel indexing + provenance/status UX)*
* **Date Proposed:** 2026-06-13
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Smalltalk developer**, I want **auto-completion for selectors, class names, and local/instance variables, including standard kernel-library selectors**, so that **I can write code faster with fewer lookups.**

**Architecture:** kernel symbols follow [ADR-0002](../decisions/0002-kernel-symbol-sourcing.md) — an *installed-first, bundled-fallback* precedence chain over a **dialect-neutral index model** fed by **per-dialect source adapters** (GST file adapter now; image-based adapters later, per Constitution Principle IV).

**Acceptance Criteria (AC):**
* AC1: A build-time generator parses a GNU Smalltalk kernel source directory (the bundled `../smalltalk-3.2.5/kernel/*.st`) with our own parser into `server/data/kernel-index.json` — dialect-neutral model (`dialect`, `library`, `version`, `source` header; classes, superclass chains, selectors + arity). Built on a **reusable `.st`-directory indexer** (the same one AC6 uses), **facts only** (no comment prose).
* AC2: Completion offers keyword selectors after a receiver (workspace > installed-kernel > bundled-kernel ranking, prefix + camel-hump matching).
* AC3: Completion offers class names in expression-head position and temp/instance variables from the symbol-table scopes.
* AC4: Multi-part keyword selectors insert as snippets (e.g. `at:put:` → `at:${1} put:${2}`).
* AC5: A setting `smalltalk.completion.kernelLibrary` (`auto` | `bundled` | `off`, default `auto`) selects the kernel sourcing strategy; `smalltalk.completion.kernelPath` overrides the GST kernel-directory discovery.
* AC6: When a GST install is discoverable (via `kernelPath`, the `smalltalk.gnuSmalltalkPath` prefix, or common locations), the kernel tier indexes its **actual** kernel sources live via the same directory indexer; otherwise it falls back to the bundled index.
* AC7: Completion items carry **provenance** (workspace / installed / bundled-reference); a status-bar item shows the resolved kernel identity (e.g. "bundled (gst 3.2.5)" / "installed" / "off"), with a one-time notice on first fallback to the bundle.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-411 + US-412 indexes.
* [X] Licensing reviewed (kernel selector names/arities are facts; method comment prose is LGPL 2.1 — names/signatures only in v0.5.0).
* [X] Sourcing architecture decided ([ADR-0002](../decisions/0002-kernel-symbol-sourcing.md)).
* [X] Estimated/sized.

**Definition of Done (DoD) Checklist:**
* [X] Kernel-index model + bundled generator + live installed-kernel indexing + completion provider implemented (slices A–D, #51–#54).
* [X] **Language Server:** unit tests at cursor positions; index generator snapshot test; licensing (no-prose) test; resolution/discovery tests.
* [X] **End-to-End:** integration test asserting kernel + workspace completions (`client/test-e2e/completion.test.js`).
* [X] **Output eval:** `evals/datasets/completion/` (8/8) wired into `npm run eval` (CI, 3 OSes).
* [X] GitHub issue #1 closed with a demo.
* [X] Manual-QA matrix (`verification.md` §3) executed + signed off.
* [X] PO accepts the story.

**Notes / Questions / Assumptions:**
* Decide on shipping kernel method-comment text (with attribution) before hover (US-415).
* Image-based dialect adapter (Pharo/Squeak, via reflective export) and a `smalltalk.dialect` axis are deferred follow-ups (ADR-0002).

---

## US-414: Diagnostics (Live Parser Tier)

* **ID:** US-414
* **Status:** Done (v0.6.0 — in release), **parser-only**. Shipped: live parser squiggles (AC1) + quick fixes that insert a missing closer (`]`/`)`/`}`/`>`) or close an unterminated string (AC4). Decisions: parser debounce 250 ms; severity as-emitted; quick fixes inserted at the diagnostic range **start** (before the offending token); `}`/`>` diagnostics anchored at the end of the last token so squiggle/fix land on the defect line — both manual-QA findings. **AC2/AC3 (opt-in `gst` compile tier) were built then deferred to EPIC-007** (scope decision 2026-06-24): gst 3.2.5 emits only syntax errors the parser already catches better; semantic value needs a runtime. See `specs/US-414-*/spec.md` §7 and **US-705** below.
* **Epic:** EPIC-004
* **Priority:** Medium
* **Estimate:** L
* **Date Proposed:** 2026-06-13
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Smalltalk developer**, I want **error squiggles as I type**, so that **I catch mistakes without leaving the editor.**

**Acceptance Criteria (AC):**
* AC1: Parser diagnostics (syntax errors/warnings) are published on change, debounced, with code `smalltalk(parse)`. ✅
* AC4: Trivial code actions (insert a missing closer `]`/`)`/`}`/`>`, or close an unterminated string) are offered where cheap. ✅
* ~~AC2: opt-in `smalltalk.diagnostics.useGst` runs `gst` on save → diagnostics.~~ **Deferred to EPIC-007 (US-705).**
* ~~AC3: gst processes time out / are killed on edit; never block typing.~~ **Deferred to EPIC-007 (US-705).**

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-411 (parser diagnostics are free from it).
* [X] Estimated/sized.

**Definition of Done (DoD) Checklist:**
* [X] Live parser diagnostics + AC4 insert-closer/close-string quick fixes implemented.
* [X] **Language Server:** unit tests for diagnostic ranges + code actions (`server/test/{diagnostics,codeAction}.test.ts`).
* [X] **End-to-End:** integration test asserting squiggles on malformed input + the quick fix clears it — `client/test-e2e/US-414.acceptance.test.js`; handshake asserts `publishDiagnostics`/capabilities.
* [X] AC2/AC3 (gst tier) descoped to EPIC-007 (US-705); implementation preserved in git history (`a02518d`).
* [ ] PO accepts the story (manual-QA matrix in `specs/US-414-*/verification.md` + clean VSIX).

**Notes / Questions / Assumptions:**
* gst stderr verified on GNU Smalltalk 3.2.5: `<file>:<LINE>: <message>` — line-only, no column/severity (the AC2 `error:` form was an assumption). gst is dynamic (undeclared vars print `nil`), so it adds no *semantic* value over the parser — the reason the tier moved to EPIC-007.

---

## US-415: Hover

* **ID:** US-415
* **Status:** Done (v0.7.0 — closes #27). Provider for selectors/classes/variables/literals + provenance-gated comment prose (installed kernel + workspace carry comments; bundled reference facts-only). Shipped with a radix-integer syntax-coloring fix; manual-QA workspace cleared.
* **Epic:** EPIC-004
* **Priority:** Medium
* **Estimate:** M
* **Date Proposed:** 2026-06-13
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Smalltalk developer**, I want **hover information for selectors, classes, variables, and literals**, so that **I can understand code without navigating away.**

**Acceptance Criteria (AC):**
* AC1: Hover on a selector shows its signature and implementor list (and kernel method comment if licensing allows).
* AC2: Hover on a class shows its superclass chain and class comment.
* AC3: Hover on a variable shows its kind and declaration site.
* AC4: Hover on a numeric literal renders its radix/scaled-decimal value.
* AC5: Hover content is Markdown with code fences.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-411/412/413 indexes.
* [X] Estimated/sized.

**Definition of Done (DoD) Checklist:**
* [X] Hover provider implemented (`server/src/providers/hover.ts`; selectors/classes/variables/literals).
* [X] **Language Server:** unit tests for hover content per symbol kind (`server/test/hover.test.ts`, `comments.test.ts`).
* [X] **End-to-End:** integration test asserting hover Markdown (`client/test-e2e/US-415.acceptance.test.js`).
* [ ] PO accepts the story (pending release: manual-QA matrix + CI green).

**Notes / Questions / Assumptions:**
* Reuses the workspace + kernel indexes from US-412/US-413.
* Comment prose is gated on **provenance** (the deferred "ship kernel comments?" decision, line 862):
  bundled reference = facts-only; the locally-built **installed** cartridge + the user's **workspace**
  source may carry comments (read locally, never redistributed — LGPL-clean). See `specs/US-415-Hover/spec.md` §4a.

---

## US-416: Formatting

* **ID:** US-416
* **Status:** Planned
* **Epic:** EPIC-004
* **Priority:** Low *(droppable from 1.0 to 1.1 if it slips)*
* **Estimate:** L
* **Date Proposed:** 2026-06-13
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Smalltalk developer**, I want **conservative, idempotent code formatting**, so that **I can normalize style without risking my code.**

**Acceptance Criteria (AC):**
* AC1: `rangeFormatting` + `onTypeFormatting` ship first (indent after `[`, dedent on `]`, align cascades).
* AC2: `documentFormatting` reprints from the AST, preserving comments and blank lines, normalizing only indentation and keyword-message wrapping.
* AC3: Property tests hold: `format(format(x)) === format(x)` and the lexer token stream is unchanged before/after.
* AC4: Formatting is off by default (`smalltalk.format.enable`) for at least one release.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-411 (AST + lossless comments).
* [X] Risk acknowledged (data loss = trust loss); idempotence is mandatory.
* [X] Estimated/sized.

**Definition of Done (DoD) Checklist:**
* [ ] Range/on-type formatting then document formatting implemented.
* [ ] **Language Server:** idempotence + token-round-trip property tests passing.
* [ ] **End-to-End:** integration test asserting formatted output.
* [ ] Default-off setting honored.
* [ ] PO accepts the story.

**Notes / Questions / Assumptions:**
* If it slips, it ships in v1.1 and must not block v1.0.

---

## US-417: Navigation Polish — Semantic Folding + Document Highlight

* **ID:** US-417
* **Status:** Done (v0.4.1). Delivered as 2 slices — semantic folding (#45) + scope-aware document highlight (#46); the retired-Marketplace-badge README fix shipped alongside (#44).
* **Epic:** EPIC-004
* **Priority:** Low
* **Estimate:** S
* **Date Proposed:** 2026-06-20
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Smalltalk developer**, I want **semantic code folding and highlighting of a symbol's other occurrences**, so that **navigating and reading a file feels as polished as the outline and go-to-definition already do.**

**Acceptance Criteria (AC):**
* AC1: `textDocument/foldingRange` returns ranges for class bodies, method bodies, blocks, and multi-line comments, walked from the US-411 AST (in addition to VS Code's default indentation/marker folding).
* AC2: `textDocument/documentHighlight` highlights the occurrences of the symbol under the cursor within the current file — **scope-aware** (the same variable/parameter, or the same selector for message sends), not a naive same-text match.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-411 (ranged AST) and US-412 (provider plumbing) — both shipped.
* [X] Scoped as a bonus/polish item; explicitly droppable.
* [X] Estimated/sized (S).

**Definition of Done (DoD) Checklist:**
* [X] foldingRange + documentHighlight providers implemented over the parse cache.
* [X] **Language Server:** unit tests (folding ranges per construct; scoped highlight resolution) + real-server LSP + Electron e2e checks.
* [X] Works with no `gst` (reuses the navigation engine; zero process dependency).
* [X] PO accepts the story (shipped in v0.4.1; manual spot-check passed).

**Notes / Questions / Assumptions:**
* Captured from the US-412 spec's "near-free bonuses" note. `foldingRange` is the truly near-free half (pure AST walk, reuses `parseCache`); `documentHighlight` needs the scope-aware resolution, hence its own AC. Ships as a 0.4.x point release, before/independently of 0.5.0 (US-413 completion).

---

> **Backlog from the 0.5.0 plan reconciliation (2026-06-21).** US-418–421 capture implementation
> divergences from the genesis `plan.md` that were never explicitly ratified; US-901/902 migrate
> plan-only definitions (perf budgets, no-telemetry, Open VSX) into the backlog. See the reconciliation
> ledger in the 0.5.0 close-out and [ADR-0002](../decisions/0002-kernel-symbol-sourcing.md) /
> [ADR-0001](../decisions/0001-typescript-bundled-lsp-server.md).

## US-418: Container-Format Seam (Dialect Door)

* **ID:** US-418
* **Status:** Backlog — #58 (trigger fires at **milestone 1.5 / EPIC-006** — the second dialect (Pharo); built then, not before)
* **Epic:** EPIC-004
* **Priority:** Low
* **Estimate:** M
* **Date Proposed:** 2026-06-21
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **maintainer**, I want the brace/chunk container parsing extracted behind a `ContainerFormat` seam, so that **a second dialect (Tonel/Pharo/Cuis) can be added via an adapter without rewriting `parser.ts`.**

**Acceptance Criteria (AC):**
* AC1: Container formats live behind a `ContainerFormat` interface (`server/src/parser/containers/`), with the GST brace and chunk formats as two implementations.
* AC2: The parser core is dialect-neutral; adding a new container format needs no change to the core or to existing format modules.
* AC3: Existing parser/symbol snapshots are unchanged (pure refactor).

**Definition of Ready (DoR) Checklist:**
* [X] Reconciles `plan.md` (planned `containers/{gstBrace,gstChunk,index}.ts`) with reality (folded into `parser.ts`); ADR-0001 updated with the deferral note.
* [ ] A concrete second dialect is in scope (the trigger).

**Definition of Done (DoD) Checklist:**
* [ ] `ContainerFormat` seam extracted; brace + chunk are adapters; snapshots green.
* [ ] PO accepts.

**Notes / Questions / Assumptions:**
* Honors Constitution IV (*Dialect Agnostic*). Don't build speculatively with one dialect — see [ADR-0001 §Update](../decisions/0001-typescript-bundled-lsp-server.md).

---

## US-419: Kernel Index — Method Categories

* **ID:** US-419
* **Status:** Backlog — #59
* **Epic:** EPIC-004
* **Priority:** Low
* **Estimate:** S
* **Date Proposed:** 2026-06-21
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Smalltalk developer**, I want **methods grouped by their GST category**, so that **outline/hover can present them the way the image does.**

**Acceptance Criteria (AC):**
* AC1: The kernel/workspace indexer captures each method's `category` (from the `<category: '…'>` pragma / `methodsFor:` argument) into the dialect-neutral model — **facts only** (category strings are facts; no comment prose).
* AC2: Hover/outline can group or label methods by category.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-413 kernel index (model is extensible).
* [X] Estimated/sized (S).

**Definition of Done (DoD) Checklist:**
* [ ] Category captured in the index + a consumer (hover/outline) uses it.
* [ ] PO accepts.

**Notes / Questions / Assumptions:**
* `plan.md` listed categories in the kernel index; 0.5.0 (ADR-0002) shipped classes/superclass/selectors+arity only. Add when hover (US-415) needs grouping.

---

## US-420: Completion — Pseudo-Variables

* **ID:** US-420
* **Status:** Ready — #60 (small near-term fix; candidate for v0.5.1)
* **Epic:** EPIC-004
* **Priority:** Medium
* **Estimate:** S
* **Date Proposed:** 2026-06-21
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Smalltalk developer**, I want **`self super nil true false thisContext` offered in completion**, so that **typing `sup` → `super` works like other identifiers.**

**Acceptance Criteria (AC):**
* AC1: Head/primary context offers the six pseudo-variables (prefix + camel-hump), ranked alongside in-scope variables.
* AC2: They are **not** offered in selector position (after a receiver).

**Definition of Ready (DoR) Checklist:**
* [X] Builds on the US-413 completion provider (`server/src/providers/completion.ts`).
* [X] Estimated/sized (S).

**Definition of Done (DoD) Checklist:**
* [ ] Pseudo-variables offered in head context; unit test at a cursor position.
* [ ] PO accepts.

**Notes / Questions / Assumptions:**
* `plan.md` listed pseudo-variables for US-413 completion; gap in shipped 0.5.0. Small static list.

---

## US-421: CI — Vendored Kernel Smoke-Test Fixtures

* **ID:** US-421
* **Status:** Ready — #61 (near-term)
* **Epic:** EPIC-004
* **Priority:** Medium
* **Estimate:** S
* **Date Proposed:** 2026-06-21
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **maintainer**, I want **~10 representative kernel `.st` files vendored as CI fixtures**, so that **the "parses cleanly" guarantee is enforced in CI** (today the kernel smoke test skips when the corpus is absent — CI never runs it).

**Acceptance Criteria (AC):**
* AC1: A small vendored fixture set (e.g. `server/test/fixtures/kernel/`) is parsed in CI with **0 crashes / 0 diagnostics**.
* AC2: The full local-corpus smoke test (`server/test/kernel.test.ts`) remains and still skips gracefully when `../smalltalk-3.2.5/kernel` is absent.

**Definition of Ready (DoR) Checklist:**
* [X] Reconciles `plan.md` (vendor ~10 files) with reality (0 vendored; smoke is local-only).
* [X] Estimated/sized (S).

**Definition of Done (DoD) Checklist:**
* [ ] Fixtures vendored + a CI-run smoke test asserts clean parse.
* [ ] PO accepts.

**Notes / Questions / Assumptions:**
* Licensing: vendoring a handful of LGPL-2.1 kernel sources as test fixtures is fine with attribution (source files retain their headers).

---

## US-901: Hardening, Performance & Beta Polish

* **ID:** US-901
* **Status:** Planned (hardening pass before 1.0 — milestone **0.10.0** in the evolved ladder)
* **Epic:** EPIC-004 (cross-cutting)
* **Priority:** Medium
* **Estimate:** L
* **Date Proposed:** 2026-06-21
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **user**, I want the extension to **stay fast and reliable on large codebases**, so that **it is beta-quality.**

**Acceptance Criteria (AC):**
* AC1: Indexing a **1,000-file** workspace completes in **< 5 s**; completion responds in **< 100 ms** (Constitution Performance budgets).
* AC2: Cancellation tokens honored; memory caps / file-size guards in place; untrusted-/virtual-workspace capabilities declared.
* AC3: **No telemetry** (Constitution VII) — verified: no analytics/network calls beyond opt-in `gst`.
* AC4: Bug-bash against `learning-smalltalk/` + the GST kernel; no P1 bugs for 2+ weeks.

**Definition of Ready (DoR) Checklist:**
* [X] Migrated from `plan.md` (perf budgets + no-telemetry stance) into the backlog + Constitution v1.2.0.
* [X] Estimated/sized (L).

**Definition of Done (DoD) Checklist:**
* [ ] Perf budgets met + measured; resilience capabilities declared; no-telemetry verified.
* [ ] PO accepts.

**Notes / Questions / Assumptions:**
* Perf budgets also live in the Constitution Performance section (v1.2.0).

---

## US-902: Product Polish & Open VSX (1.0.0)

* **ID:** US-902
* **Status:** Planned (milestone **1.0.0**)
* **Epic:** EPIC-004 (cross-cutting)
* **Priority:** Medium
* **Estimate:** M
* **Date Proposed:** 2026-06-21
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **user**, I want a **polished 1.0 with discoverable docs and availability beyond the VS Code Marketplace**, so that **the extension reads as a finished, trustworthy product.**

**Acceptance Criteria (AC):**
* AC1: README hero GIFs (highlighting / outline / completion / Run / diagnostics); CONTRIBUTING linked; `.github/ISSUE_TEMPLATE/` + labels (`area:parser`, `area:lsp`, `good-first-issue`).
* AC2: `"preview": true` removed from `package.json`.
* AC3: **Published to Open VSX** (`ovsx` + `OVSX_PAT` secret) alongside `vsce` in CI.
* AC4: `docs/product/*` synced to shipped reality.

**Definition of Ready (DoR) Checklist:**
* [X] Migrated from `plan.md` (Open VSX, remove preview, README polish).
* [X] Estimated/sized (M).

**Definition of Done (DoD) Checklist:**
* [ ] 1.0 polish complete; Open VSX publish working in CI.
* [ ] PO accepts.

**Notes / Questions / Assumptions:**
* Open VSX needs an `OVSX_PAT` secret (owner-provisioned), analogous to `MARKETPLACE`.

---

> **EPIC-005 — Offline Knowledge Graph ("Console & Cartridges") (2026-06-21).** Reframes the
> server as a dialect-agnostic static intelligence engine: a dialect-neutral **Console** that loads
> frozen, per-dialect **Cartridges** of resolved facts. GNU Smalltalk 3.2.5 is **Cartridge #01**.
> Foundation: US-430 (schema + reflective exporter). Consumers: US-422 (semantic tokens), US-423
> (references/senders/implementors), SPIKE-01 (unknown-selector heuristic). Schema lives in
> `server/src/types/knowledge-base.ts`; see [`epics.md`](epics.md) EPIC-005 and
> [ADR-0002](../decisions/0002-kernel-symbol-sourcing.md).

## US-430: Dialect Cartridge Schema + GST Cartridge #01 (Reflective Exporter)

* **ID:** US-430
* **Status:** Done — merged #82 (closes #64), 2026-06-23 — Slices A–D: schema + reflective exporter + committed `gst-3.2.5-cartridge.json` (**249 classes / 4746 signatures**, facts-only, `contentHash`-stamped); runtime `cartridgeLoader.ts` projects it to drive completion; installed adapter (`indexKernelDirectoryToCartridge`) emits cartridge shape; Tier-1 installed / Tier-2 floor resolution; old `kernel-index.json` + `gen-kernel-index.ts` **retired**. All test layers + completion eval green; CI green on 3 OSes; manual `verification.md` signed off by PO. Ships in 0.8/0.9. Follow-up: US-431 (installed version label + settings UX).
* **Epic:** EPIC-005
* **Priority:** High *(hard dependency for US-422/423/SPIKE-01)*
* **Estimate:** L
* **Date Proposed:** 2026-06-21
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **maintainer**, I want **a pure-JSON Dialect Cartridge schema and a GST exporter that compiles GNU Smalltalk 3.2.5 into Cartridge #01**, so that **the Console can serve image-grade kernel knowledge offline, and new dialects become a new cartridge rather than a rewrite.**

**Acceptance Criteria (AC):**
* AC1: `server/src/types/knowledge-base.ts` defines the `DialectCartridge` interfaces — pure JSON (no AST/instances/cycles), resolved facts only, explicit class-side/instance-side split, structural resolution (superclass/traits) separate from an open `taxonomy` bag.
* AC2: A loader assertion (or generation test) proves a cartridge round-trips through `JSON.parse` with no functions/cycles and validates against the schema.
* AC3: `scripts/export-gst-cartridge.st` runs headlessly (`gst -f …`), walks `Smalltalk`, and emits `server/data/cartridges/gst-3.2.5-cartridge.json` matching the schema (classes + `crossReference` tier).
* AC4: The exporter is **facts-only** (`carriesProse: false`) — no LGPL method/class comment prose in the output; a test asserts no prose fields are present.
* AC5: Deterministic output (sorted object keys) so the committed cartridge has stable diffs; `contentHash` stamped by the TS build step.
* AC6: The exporter is heavily commented as a **template** for future Pharo/Squeak (image-export) exporters; build-time `gst` only — runtime stays zero-dependency (ADR-0001).

**Definition of Ready (DoR) Checklist:**
* [X] Schema design reviewed and approved (this session).
* [X] Reflection approach chosen (image-export over static parse — ADR-0002 'image-export' adapter).
* [X] Licensing reviewed (facts-only; LGPL prose excluded).
* [X] Estimated/sized (L).

**Definition of Done (DoD) Checklist:**
* [X] Schema committed; exporter validated against local gst 3.2.5; cartridge generated.
* [X] **Language Server:** schema round-trip + no-prose tests; cartridge load smoke test; projection-equivalence + loader inheritance/trait resolution.
* [X] Cartridge committed under `server/data/cartridges/` (+ generation documented in CLAUDE.md).
* [X] Reconciliation resolved: the cartridge **supersedes** `kernel-index.json`; `gen-kernel-index.ts` retired, replaced by `scripts/stamp-cartridge.ts` (contentHash). The static indexer now emits cartridge shape (Tier-1 installed adapter, ADR-0003).
* [ ] PO accepts the story.

**Notes / Questions / Assumptions:**
* Reflective export gives 100%-accurate method tables/arities/categories vs. static parsing. The literal-frame send scan under-counts special-selector bytecodes (`+`, `at:put:`, `do:` when inlined) — accepted for v1; a bytecode-disassembly pass can refine `senders` later.
* Resolution & convergence decided in **[ADR-0003](../decisions/0003-cartridge-resolution.md)**: the cartridge is the canonical model; the static `indexKernelDirectory` becomes the **preferred, cached Tier-1** source (from the user's install, no runtime), and the committed cartridge is the **rich frozen floor (Tier-2)** for zero-install/offline + the eval baseline. The old `kernel-index.json` folds into the cartridge format.

---

## US-431: Kernel-Sourcing Transparency — installed version label + settings UX

* **ID:** US-431
* **Status:** Backlog — split out of US-430 (deferred by PO, 2026-06-23)
* **Epic:** EPIC-005 *(may fold into US-603 generate-and-cache, which already spawns gst)*
* **Priority:** Low
* **Estimate:** S

**User Story:**
> As a **user with GNU Smalltalk installed**, I want the status bar to show my **actual kernel version**
> (`installed (gst 3.2.5)`, not just `installed (gst)`) and the resolved kernel dir/binary to be **visible**,
> so I can trust which source is feeding completion without it being silently wrong.

**Context / decisions already made (this session):**
* The status label is already **version-aware** (`identityLabel` in `kernelIndexService.ts`): it appends the
  version when one is known, so it renders `installed (gst X.Y.Z)` the moment a version is available. Today
  the static no-runtime adapter has no version, so it shows `installed (gst)`.
* **No-runtime nuance**: the principle protects the *floor / first-run-no-gst* user, **not** the installed
  path (which by definition has gst — the extension already spawns gst for Run Current File, US-301). So a
  bounded `gst --version` probe on the installed path is acceptable.
* **Same-installation safety (the key correctness rule)**: the version MUST come from the gst binary
  **co-located in the same install prefix** as the parsed kernel dir — `<prefix>/share/smalltalk/kernel` →
  `<prefix>/bin/gst` (add `gstBinaryForKernelDir`, the inverse of `deriveKernelDirFromGst`). **Never** read
  an unrelated configured `gnuSmalltalkPath`, or `kernelPath`=3.2.5 + `gnuSmalltalkPath`=2.5.1 would mislabel.
  If no co-located binary answers, keep `installed (gst)`.

**Acceptance Criteria (AC):**
* AC1: A bounded, cached `gst --version` probe (short timeout, no zombies — US-301/US-414 discipline) on the
  binary co-located with the resolved kernel dir → `installed (gst <version>)`; absent/mismatched → `installed (gst)`.
* AC2: Resolution cross-derives the two paths at runtime (kernelPath↔binary by shared prefix) **without
  writing to settings** (auto stays auto; no scope churn).
* AC3: A richer status-bar **tooltip** showing the resolution (source dir, binary, config provenance) and a
  `Smalltalk: Show Kernel Status` command surfacing the same — read-only transparency.
* AC4: Settings descriptions clarify that `kernelPath` applies only when `kernelLibrary = auto`.

**Notes / Rejected for now:**
* **Auto-writing discovered paths into settings — rejected**: breaks the `auto` contract (pins a path that
  goes stale on upgrade), ambiguous User-vs-Workspace scope, git churn, anti-Zero-Config. Surface resolved
  values read-only instead.
* **Disabling `kernelPath` when bundled/off — not possible**: VS Code has no conditional enable/disable for
  settings fields (no `when` for configuration properties). Mitigate via description wording; it's already a
  functional no-op outside `auto`.

---

## US-422: Semantic Tokens (Cartridge-Aware)

* **ID:** US-422
* **Status:** Done (v0.8.0, EPIC-005) — closes #65
* **Epic:** EPIC-005 *(consumes the EPIC-004 parser)*
* **Priority:** Medium
* **Estimate:** S–M
* **Date Proposed:** 2026-06-21
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Smalltalk developer**, I want **role-accurate semantic highlighting** — instance var vs class var vs temp vs block/method arg vs *known class/global* vs unknown global vs keyword-selector part vs pseudo-variable — so that **I read code by meaning, not just token shape, even for kernel classes I never declared.**

**Acceptance Criteria (AC):**
* AC1: `textDocument/semanticTokens/full` (+ `range`) classifies the role set above from the US-411 AST + symbol-table scopes; reuses `parseCache`.
* AC2: A capitalized identifier is colored `class` **iff** it resolves to a `ClassId` in `workspace ∪ cartridge`; otherwise `variable.other.global`. (First visible cartridge consumer — kernel classes light up with no `gst` present.)
* AC3: Keyword-message parts and pseudo-variables (`self super nil true false thisContext`) carry distinct token types/modifiers.
* AC4: Degrades cleanly with no cartridge loaded (capitalization fallback; never errors).
* AC5: Works with no `gst`; output-eval dataset `evals/datasets/semantic-tokens/`.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-411 AST + US-430 cartridge load.
* [X] Scoped to token classification only.
* [X] Estimated/sized.

**Definition of Done (DoD) Checklist:**
* [ ] Provider over `parseCache` implemented.
* [ ] **Language Server:** unit tests per role; AC2 cartridge-vs-unknown class test.
* [ ] **End-to-End:** token-range assertion.
* [ ] Output eval green on 3 OSes; no-`gst` verified.
* [ ] PO accepts the story.

**Notes / Questions / Assumptions:**
* Honest framing: highlighting is table-stakes hygiene; the cartridge-driven class/global distinction (AC2) is the part that is ours and offline.

---

## US-423: References + Senders/Implementors (Two-Tier Engine)

* **ID:** US-423
* **Status:** Done — **v0.9.0** (closes #66)
* **Epic:** EPIC-005
* **Priority:** High *(flagship usability lead)*
* **Estimate:** M *(call hierarchy folded in — same index)*
* **Date Proposed:** 2026-06-21
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Smalltalk developer**, I want **"Senders of" and "Implementors of" across my workspace and the bundled kernel, instantly and offline**, so that **I get the System Browser's cross-reference muscle memory without a running image — and I am told honestly that results are a union, because dynamic dispatch cannot be statically resolved.**

**Acceptance Criteria (AC):**
* AC1: `textDocument/references` returns the de-duplicated union of workspace + cartridge send sites/definitions, with `Workspace ≻ Cartridge` precedence (handles the dev-box overlap where the kernel source is also open in the workspace).
* AC2: Commands `Smalltalk: Senders of…` and `Smalltalk: Implementors of…` return a structured tree with a **header node stating the union/uncertainty contract** and **per-row provenance** (`workspace` / `cartridge:<dialect>@<version>`).
* AC3: Go-to-definition on a message send returns **plural** `LocationLink[]`; never collapses to one.
* AC4: `callHierarchy/incoming|outgoingCalls` reuse the same cross-reference index (incoming = senders, outgoing = sends within a method).
* AC5: Query path does **no** parsing/I-O — O(1) lookups + O(k) merge; meets the completion-class budget (target < 10 ms typical).
* AC6: `receiverHint` ranks *likely* responders above *possible* ones; the long tail is never filtered out.
* AC7: Works with no `gst`.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-430 (`crossReference` tier) + US-412/413 indexes.
* [X] Dynamic-dispatch UX posture defined (lexical union + disclaimer).
* [X] Estimated/sized.

**Definition of Done (DoD) Checklist:**
* [X] references + senders/implementors + call-hierarchy providers implemented (+ plural definition; installed-tier cross-reference parity + real-source navigation).
* [X] **Language Server:** unit tests (merge/dedup/precedence, union packaging, dev-box overlap, ranking) — `workspaceXref`/`resolve`/`callHierarchy`/`crossReference` suites + `test:server`.
* [X] **End-to-End:** peek references + plural definition + call hierarchy + command-result assertions (`US-423.acceptance.test.js`, 27-test suite green).
* [X] Output eval `evals/datasets/references/` (5/5); no-`gst` verified.
* [X] PO accepts the story — **accepted 2026-06-26**, shipped **v0.9.0** (see `verification.md` §4/§5).

**Notes / Questions / Assumptions:**
* Graded honestly as a **Transient Lead** — copyable, and a static shadow of the image's whole-world query — but the highest-value navigation win and the most visible proof of the offline knowledge graph. **Sequencing (resolved):** lands at **0.9.0** (Cross-Reference Intelligence) in the evolved ladder; formatting (US-416) moves to 1.0.

---

## SPIKE-01: Unknown-Selector Heuristic — False-Positive Validation

* **ID:** SPIKE-01
* **Status:** Ready — #67 *(time-boxed research; gates any feature story)*
* **Epic:** EPIC-005
* **Priority:** Medium
* **Estimate:** S *(time-box: 2–3 days)*
* **Date Proposed:** 2026-06-21
* **Owner:** Leonardo Nascimento

**Goal:**
> Prove or kill the "Heuristic of Extreme Caution" for marking a message send as an unknown selector. **Decision gate: adopt only if the false-positive rate is effectively zero** on a real corpus. A linter that squiggles valid dynamic code is a ten-minute uninstall.

**Acceptance Criteria (AC):**
* AC1: Implement `shouldEmitMissingSelectorWarning(selector, receiverNode, workspace, cartridge)` per the gate — emit **only** when: receiver resolves to a *closed-world* class (`self`/`super` in a fully-indexed class, or a literal known-class receiver), the selector is absent from the resolved table (own ∪ inherited ∪ traits), the enclosing method parses clean, and no escape hatch fires — **behind a flag, no published diagnostics.**
* AC2: Escape hatches enforced (force silence): custom `doesNotUnderstand:` in the chain (excluding the root error DNU); any `perform:`-family send; proxy/forwarding signals (`*Proxy/*Mock/*Stub`, forwarding traits); incomplete/extension/un-indexed receiver tables; a small reflective allowlist; explicit opt-out pragma/setting.
* AC3: Run against `learning-smalltalk/` + the GST 3.2.5 kernel; report **precision** (zero false positives is the bar) plus sample true catches, and the **closed-world coverage** (fraction of sends the heuristic can even speak to).
* AC4: Written recommendation: **adopt → file a follow-up unknown-selector feature story** (default severity `Hint`, opt-out, allowlist) or **shelve** with failure evidence. Low closed-world coverage at zero-FP is also a kill signal.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-430 cartridge + US-411/412 indexes.
* [X] Decision criteria explicit (≈0 false positives).
* [X] Time-boxed.

**Definition of Done (DoD) Checklist:**
* [ ] Gated heuristic implemented (no published diagnostics).
* [ ] Corpus report: precision + closed-world coverage.
* [ ] Go/no-go memo.
* [ ] PO accepts the recommendation.

**Notes / Questions / Assumptions:**
* This spike exists so we never ship a false-positive linter. It is the static shadow of `doesNotUnderstand:` — the only zero-runtime feature that gestures at Smalltalk's live soul — and earns a story only on evidence.

---

## US-424: Tonel Read-Only Editing Experience (the "Trojan Horse")

* **ID:** US-424
* **Status:** Backlog (vision — pulled forward to **~1.0**, ahead of the rest of EPIC-006) — #93
* **Epic:** EPIC-006 *(but ships as a Stream-A wedge, decoupled from the cartridge/seam work)*
* **Priority:** High *(cheap go-to-market wedge: wins the Pharo/GemStone/VA crowd before any cartridge exists)*
* **Estimate:** M
* **Date Proposed:** 2026-06-24
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Pharo / GemStone / VA Smalltalk developer reviewing Tonel files in Git**, I want **correct syntax highlighting, method folding, and a document outline for Tonel (`.class.st`) files**, so that **VS Code is the best place to read my image-exported code — long before a Pharo cartridge exists.**

**Context / Why now:**
> Tonel is the modern, human-readable flat-file format image-based Smalltalkers use to export code to
> Git. Today every Smalltalk highlighter on the Marketplace **mangles Tonel's class-definition headers**
> (the `Class { #name : … }` STON-ish preamble), so reading Tonel in VS Code is a mediocre experience.
> A **read-only** Tonel experience is *cheap* — TextMate grammar + a lightweight outline — needs **no
> cartridge and no parser-core dialect work**, and instantly captures the corporate/Pharo audience.
> This is deliberately the **read-only** slice: full Tonel *parsing* as a first-class dialect (the
> `ContainerFormat` seam + Pharo cartridge) stays at **1.5** (US-418 / EPIC-006).

**Acceptance Criteria (AC):**
* AC1: TextMate grammar for Tonel files (`*.class.st` and the Tonel header form) — the `Class { … }` /
  `Trait { … }` / `Extension { … }` preamble, method `>>` separators, pragmas, and method bodies all
  colorize correctly; the header's STON metadata does not bleed into method-body scopes.
* AC2: **Method folding** — each `Class >> selector [ … ]` method folds independently.
* AC3: **Document outline** — packages/classes → methods appear in the breadcrumb/outline (a light,
  Tonel-aware symbol pass; **no full parser-core change**, honoring US-418's deferral).
* AC4: Read-only scope only — **no** completion/diagnostics/hover claims on Tonel yet; those arrive
  with the Pharo cartridge (EPIC-006). Existing GST `.st`/`.gst` behavior is unchanged.

**Definition of Ready (DoR) Checklist:**
* [X] Decoupled from US-418 (no `ContainerFormat` seam needed for read-only grammar + outline).
* [ ] Tonel header grammar mapped (extend the `docs/research/` grammar work for the STON preamble).
* [ ] Estimated/sized (M).

**Definition of Done (DoD) Checklist:**
* [ ] Tonel grammar + folding + outline; grammar-snapshot eval; manual-QA on a real Tonel repo (e.g. a Pharo package export).
* [ ] PO accepts.

**Notes / Questions / Assumptions:**
* The friend's sharpest tactical point (2026-06-24 review): a read-only Tonel experience is the
  **Trojan Horse** that lands the image-based community in Stream A, before the cartridge moat (Stream B)
  reaches them at 1.5. Resequencing decision recorded in [`docs/ROADMAP.md`](../ROADMAP.md) (milestone ladder + Deltas).
* When EPIC-006 builds the full Tonel `ContainerFormat` adapter (US-418) and the Pharo cartridge
  (US-601), this read-only experience becomes the *grammar layer* under the full dialect — additive, not thrown away.

---

## US-425: Signature Help

* **ID:** US-425
* **Status:** Ready — #68
* **Epic:** EPIC-005
* **Priority:** Low
* **Estimate:** S
* **Date Proposed:** 2026-06-22
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Smalltalk developer**, I want **keyword-message signature help as I type**, so that **I see selector signatures and the active parameter without navigating away.**

**Acceptance Criteria (AC):**
* AC1: `textDocument/signatureHelp` for keyword sends from the workspace + cartridge index, with active-parameter tracking.
* AC2: Works with no `gst`.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-430 (cartridge) + US-413 index.
* [X] Estimated/sized (S).

**Definition of Done (DoD) Checklist:**
* [ ] signatureHelp provider; unit test at a keyword-send cursor; PO accepts.

**Notes / Questions / Assumptions:**
* Parity-plus filler graded honestly; small. Lands ~0.9 alongside US-423.

---

## US-426: Scope-aware Rename

* **ID:** US-426
* **Status:** Ready — #69
* **Epic:** EPIC-005
* **Priority:** Medium
* **Estimate:** M
* **Date Proposed:** 2026-06-22
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Smalltalk developer**, I want **safe scope-aware rename of temps/args/instance vars (NOT selector rename)**, so that **I can refactor within a method/class without risk in a dynamically-typed language.**

**Acceptance Criteria (AC):**
* AC1: Rename temps/args/ivars within their resolved scope only; **selector rename explicitly out of scope** (the dangerous one).
* AC2: Idempotent; no cross-symbol bleed; works with no `gst`.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-411 (symbols) + US-423 (references).
* [X] Risk acknowledged (selector rename excluded).
* [X] Estimated/sized (M).

**Definition of Done (DoD) Checklist:**
* [ ] rename provider (scope-only); property test (no cross-symbol bleed); PO accepts.

**Notes / Questions / Assumptions:**
* Lands ~1.0; US-804 (extract-method, EPIC-008) builds on it.

---

> **EPIC-006 — Multi-Dialect Expansion (milestone 1.5).** The second dialect (Pharo) — when the
> Console & Cartridges architecture pays off and the US-418 container seam is finally built. See
> [`epics.md`](epics.md) EPIC-006.

## US-601: Pharo Cartridge via Image Reflective Export

* **ID:** US-601
* **Status:** Backlog (vision — milestone 1.5) — #70
* **Epic:** EPIC-006
* **Priority:** High
* **Estimate:** L
* **Date Proposed:** 2026-06-22
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Pharo developer**, I want **a Pharo cartridge built via headless image reflective export**, so that **all offline features light up for Pharo with no feature rewrite.**

**Acceptance Criteria (AC):**
* AC1: An image-export adapter dumps `allSubclasses`/selectors/arity/**traits**/package-tags to the `DialectCartridge` schema (`carriesProse` may be `true`; Pharo is MIT).
* AC2: The exporter mirrors `scripts/export-gst-cartridge.st` as the template (JsonWriter reused verbatim).

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-430 schema (already accommodates traits + package-tags via `taxonomy`).
* [X] Estimated/sized (L).

**Definition of Done (DoD) Checklist:**
* [ ] Pharo cartridge generated + committed; loads via the Console; PO accepts.

**Notes / Questions / Assumptions:**
* The litmus that the schema is genuinely dialect-agnostic (see ADR-0002 / US-430 mock snippets).
* Per **[ADR-0003](../decisions/0003-cartridge-resolution.md)**, the Pharo cartridge is generated via the **opt-in reflective adapter + cache** (Tier-1) on the user's machine; a **rich frozen floor** is generated in CI from a pinned Pharo image for the zero-install case.

---

## US-602: `smalltalk.dialect` Axis + Auto-Detection

* **ID:** US-602
* **Status:** Backlog (vision — milestone 1.5) — #71
* **Epic:** EPIC-006
* **Priority:** High
* **Estimate:** M
* **Date Proposed:** 2026-06-22
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **user**, I want **a `smalltalk.dialect` axis with auto-detection** (extension / Tonel markers / workspace cues), so that **the right cartridge(s) load automatically.**

**Acceptance Criteria (AC):**
* AC1: Auto-detect + explicit override; **orthogonal** to `kernelLibrary` sourcing (ADR-0002 §5).
* AC2: Active dialect shown in status/provenance.
* AC3: **Do both — auto-detect *and* a manual status-bar picker.** The active dialect is a clickable
  status-bar item (à la the VS Code Python interpreter switcher); clicking it offers a quick-pick to
  override the auto-detected dialect for the workspace. Auto-detection is the default; the picker is the
  escape hatch when detection is ambiguous or the user wants to force a dialect.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-603 (multi-cartridge loader).
* [X] Estimated/sized (M).

**Definition of Done (DoD) Checklist:**
* [ ] Dialect detection + status-bar override picker + status; tests; PO accepts.

**Notes / Questions / Assumptions:**
* `bundled` (sourcing) and `dialect` (which library) are the two orthogonal axes from ADR-0002.
* The status-bar picker is the friend's suggested UX (2026-06-24 review); we lean on auto-detection but
  let the user override — detect *and* offer the switcher, not one or the other.

---

## US-603: Multi-Cartridge Console Loader

* **ID:** US-603
* **Status:** Backlog (vision — milestone 1.5) — #72
* **Epic:** EPIC-006
* **Priority:** Medium
* **Estimate:** M
* **Date Proposed:** 2026-06-22
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **maintainer**, I want **the Console loader to load and rank across multiple active cartridges**, so that **workspace + N dialect cartridges resolve in one query.**

**Acceptance Criteria (AC):**
* AC1: Multi-cartridge load + ranking; provenance per dialect.
* AC2: Adding a cartridge needs **no feature-provider change** (the architecture test).

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-430.
* [X] Estimated/sized (M).

**Definition of Done (DoD) Checklist:**
* [ ] Multi-cartridge loader + ranking; architecture test; PO accepts.

**Notes / Questions / Assumptions:**
* This is the proof that features are written once on the Console (Constitution IV).
* Implements the **[ADR-0003](../decisions/0003-cartridge-resolution.md)** Cartridge Resolution Chain (Tier 0 workspace ≻ Tier 1 generated-and-cached ≻ Tier 2 rich frozen floor), including cache keying/invalidation by `dialect + version + source-hash`.

---

> **EPIC-007 — The Live Bridge (milestone 1.6+, OPTIONAL runtime).** The soul (Do-it/Inspect-it/run
> tests) — strictly optional, degrades to nothing without a runtime (ADR-0001). See
> [`epics.md`](epics.md) EPIC-007.

## US-701: Evaluate Selection (Do-it / Print-it / Display-it)

* **ID:** US-701
* **Status:** Backlog (vision — milestone 1.6+) — #73
* **Epic:** EPIC-007
* **Priority:** Medium
* **Estimate:** M
* **Date Proposed:** 2026-06-22
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **developer with a runtime present**, I want **Do-it / Print-it / Display-it on a selection**, so that **I get inline evaluation.**

**Acceptance Criteria (AC):**
* AC1: Evaluate selection via the optional dialect runtime; inline result.
* AC2: **Degrades to nothing** when no runtime present (ADR-0001); process timeout/kill-on-edit, no zombies (US-301/US-414 discipline).

**Definition of Ready (DoR) Checklist:**
* [X] Optional-runtime contract explicit.
* [X] Estimated/sized (M).

**Definition of Done (DoD) Checklist:**
* [ ] Evaluate-selection commands; no-runtime no-op verified; no zombies; PO accepts.

**Notes / Questions / Assumptions:**
* Soul without betraying zero-runtime; not our moat (graded honestly), but high delight when a runtime exists.

---

## US-702: SUnit Test Explorer (Static Detect; Optional Run/Debug)

* **ID:** US-702
* **Status:** Backlog (vision — milestone 1.6+) — #74
* **Epic:** EPIC-007
* **Priority:** Medium
* **Estimate:** L
* **Date Proposed:** 2026-06-22
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **developer**, I want **a SUnit Test Explorer** — static detection of `TestCase` subclasses + `test*` methods (offline), with run/debug via the optional runtime — so that **tests are discoverable even with no runtime, and runnable when one is present.**

**Acceptance Criteria (AC):**
* AC1: Static detection surfaced in the VS Code Testing API + CodeLens, **offline**.
* AC2: Run/debug only when a runtime is present; absent ⇒ detection-only, no errors.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-430 (detect) + US-701 (run).
* [X] Estimated/sized (L).

**Definition of Done (DoD) Checklist:**
* [ ] Test discovery (offline) + optional run/debug; PO accepts.

**Notes / Questions / Assumptions:**
* Static detection is the zero-runtime half; running is the optional half.

---

## US-703: Inspect-it (Structured Object Inspector)

* **ID:** US-703
* **Status:** Backlog (vision — milestone 1.6+) — #75
* **Epic:** EPIC-007
* **Priority:** Medium
* **Estimate:** L
* **Date Proposed:** 2026-06-22
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **developer**, I want **Inspect-it: a structured object inspector** backed by the optional runtime, so that **I can drill into evaluated objects.**

**Acceptance Criteria (AC):**
* AC1: Inspect evaluated objects with drill-down.
* AC2 **(DoR gate):** justify any **webview vs native tree** against Constitution I (*avoid webviews unless strictly necessary*).

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-701.
* [ ] Webview-vs-tree decision recorded (gate).

**Definition of Done (DoD) Checklist:**
* [ ] Inspector (tree preferred); PO accepts.

**Notes / Questions / Assumptions:**
* The execution soul we concede we can only have with a runtime (red-team conclusion).

---

## US-704: Playground / REPL

* **ID:** US-704
* **Status:** Backlog (vision — milestone 1.6+) — #76
* **Epic:** EPIC-007
* **Priority:** Low
* **Estimate:** M
* **Date Proposed:** 2026-06-22
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **developer**, I want **a Playground/REPL scratch surface** backed by the optional runtime, so that **I can evaluate scratch expressions interactively.**

**Acceptance Criteria (AC):**
* AC1: Evaluate scratch expressions; history; **degrades to nothing** without a runtime.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-701.
* [X] Estimated/sized (M).

**Definition of Done (DoD) Checklist:**
* [ ] Playground surface; no-runtime no-op; PO accepts.

**Notes / Questions / Assumptions:**
* Lowest-priority live feature; ships after the higher-value live items.

---

## US-705: Runtime Compile / Semantic Diagnostics (deferred from US-414)

* **ID:** US-705
* **Status:** Backlog (vision — milestone 1.6+). Deferred from US-414 (2026-06-24).
* **Epic:** EPIC-007
* **Priority:** Medium
* **Estimate:** M
* **Date Proposed:** 2026-06-24
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Smalltalk developer with a runtime present**, I want **real compile/semantic diagnostics from the Live Bridge** (undeclared variables, unknown selectors, doesNotUnderstand, arity), so that **I catch errors the static parser fundamentally cannot — without leaving the editor.**

**Why this is here and not in US-414 (0.6.0):**
> An opt-in `gst`-on-save tier was built for US-414 (server-side runner, no-zombie discipline, stderr→diagnostics, `useGst` setting + *Validate* command) and then **deferred**: GNU Smalltalk 3.2.5 is dynamic and emits only *syntax* errors, which the parser tier already catches **more precisely (column), live (on change), and fixably** — so as a syntax tier it was redundant, line-only, save-only, and the highest-risk surface (process lifecycle). Its genuine, non-redundant value is **semantic**, which requires compiling/loading into a runtime — i.e. the Live Bridge. The US-414 implementation is preserved in git history (`feature/US-414-diagnostics`, commit `a02518d`) as this story's seed.

**Acceptance Criteria (AC):**
* AC1: When a runtime/image is present (per EPIC-007), surface its **compile/semantic** diagnostics (badge e.g. `gst`/runtime), distinct from the always-on `smalltalk(parse)` tier; **degrades to nothing** without a runtime (ADR-0001).
* AC2: Bounded, no-zombie process discipline (timeout, kill-on-edit, one in-flight per uri) — reuse the US-414 runner.
* AC3: Overlap with the parser tier is handled (suppress/merge) so the user sees one actionable signal per error, not duplicate squiggles.
* AC4: Generalize beyond gst-3.2.5 to the Live Bridge backend(s); not tied to a single dialect.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on EPIC-007 Live Bridge (US-701 runtime delegation).
* [X] Seed implementation exists (US-414 `a02518d`).
* [X] Estimated/sized (M).

**Definition of Done (DoD) Checklist:**
* [ ] Runtime diagnostics provider; no-runtime no-op; overlap handling; PO accepts.

**Notes / Questions / Assumptions:**
* The overlap-handling decision (AC3) was surfaced during US-414 manual QA: parser + runtime tiers disagree on position (runtime is often line-only) and duplicate syntax errors — so the runtime tier should add value only where it differs.

---

## US-706: Live-Image Virtual FileSystem (writable `smalltalk-image://`)

* **ID:** US-706
* **Status:** Backlog (vision — milestone 1.6+) — #94
* **Epic:** EPIC-007
* **Priority:** Low *(the most ambitious live feature; ships after the higher-value live items)*
* **Estimate:** L
* **Date Proposed:** 2026-06-24
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Pharo/Squeak developer with a live image**, I want **the image's classes to appear as virtual files via a `smalltalk-image://` FileSystemProvider**, so that **I browse the living image inside VS Code and `Ctrl+S` compiles a method straight back into the image — the image-based editing loop, natively.**

**Acceptance Criteria (AC):**
* AC1: A `registerFileSystemProvider` (`smalltalk-image://`) populates a pseudo-directory of
  packages/classes from the connected live image; opening a class yields a virtual text buffer.
* AC2: **Saving compiles back** — `Ctrl+S` sends a compile command over the Live Bridge to the image
  (not a disk write); compile errors surface as diagnostics on the virtual document.
* AC3: Strictly part of the **optional** Live Bridge — absent with no runtime; never required (ADR-0001).
* AC4: Complements (does not replace) the offline System Browser (US-801) — that tree stays the
  zero-runtime experience; this VFS is the *live, writable* counterpart.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on EPIC-007 Live Bridge (US-701 runtime delegation) + a per-dialect image connection.
* [ ] Connection/transport to the live image specified (reuses the Live Bridge channel).
* [ ] Estimated/sized (L).

**Definition of Done (DoD) Checklist:**
* [ ] `smalltalk-image://` FS provider + compile-on-save + diagnostics; no-runtime no-op; PO accepts.

**Notes / Questions / Assumptions:**
* The friend's Pillar #3 (2026-06-24 review): image developers browse a living heap, not files — the
  `FileSystemProvider` API maps that experience natively. Captured as the *writable* sibling of the
  offline System Browser (US-801 AC3 records the TreeView-vs-VFS primitive decision).

---

> **EPIC-008 — Image-Grade Workbench (milestones 1.1–1.4).** The browsing soul over the offline
> Console — System Browser, search, hierarchy, refactorings — **no running image**. See
> [`epics.md`](epics.md) EPIC-008.

## US-801: System Browser Tree View

* **ID:** US-801
* **Status:** Backlog (vision — milestones 1.1–1.4) — #77
* **Epic:** EPIC-008
* **Priority:** High
* **Estimate:** L
* **Date Proposed:** 2026-06-22
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **developer**, I want **a System Browser tree** (namespaces/packages → classes → protocols → methods) over the offline Console, with senders/implementors/references panes, so that **I get the browsing soul with no running image.**

**Acceptance Criteria (AC):**
* AC1: Native VS Code tree over workspace + cartridge index; **offline**.
* AC2: Works identically across every loaded cartridge/dialect (EPIC-006).
* AC3 **(DoR gate):** decide the surface primitive — a `TreeView` vs a **Virtual FileSystem**
  (`registerFileSystemProvider`, `smalltalk-image://`) where classes appear as virtual files. The
  read-only offline browser favors a `TreeView`; the VFS is the more native primitive when paired with
  the **writable** live-image bridge (see US-706 / EPIC-007). Record the choice.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-423 (cross-reference) + US-603 (multi-cartridge).
* [ ] TreeView-vs-VFS primitive decided (gate, AC3).
* [X] Estimated/sized (L).

**Definition of Done (DoD) Checklist:**
* [ ] System Browser tree (or VFS) + panes; PO accepts.

**Notes / Questions / Assumptions:**
* The signature "feels like Smalltalk" feature, delivered offline (the navigation soul).
* The friend (2026-06-24 review) flags `registerFileSystemProvider` (`smalltalk-image://`) as arguably
  the more native primitive than a tree view. For the *offline* browser a tree is simpler; the VFS earns
  its keep once `Ctrl+S` can compile back into a live image — that writable bridge is **US-706** (EPIC-007).

---

## US-802: Full-Text Method Search

* **ID:** US-802
* **Status:** Backlog (vision — milestones 1.1–1.4) — #78
* **Epic:** EPIC-008
* **Priority:** Medium
* **Estimate:** M
* **Date Proposed:** 2026-06-22
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **developer**, I want **full-text method search across workspace + cartridge**, so that **I can find code by source, not just symbol name.**

**Acceptance Criteria (AC):**
* AC1: Extends `workspace/symbol` with method-source search; **offline**.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-430.
* [X] Estimated/sized (M).

**Definition of Done (DoD) Checklist:**
* [ ] Method-source search; PO accepts.

**Notes / Questions / Assumptions:**
* Cartridge prose/source availability is license-gated (facts-only for GST).

---

## US-803: Class-Hierarchy View

* **ID:** US-803
* **Status:** Backlog (vision — milestones 1.1–1.4) — #79
* **Epic:** EPIC-008
* **Priority:** Medium
* **Estimate:** S
* **Date Proposed:** 2026-06-22
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **developer**, I want **a class-hierarchy view** (super/subclasses including kernel chains from the cartridge), so that **I can navigate inheritance.**

**Acceptance Criteria (AC):**
* AC1: `typeHierarchy` provider over workspace + cartridge; **offline**.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-430 (superclass chains resolved over the index).
* [X] Estimated/sized (S).

**Definition of Done (DoD) Checklist:**
* [ ] Type-hierarchy provider; PO accepts.

**Notes / Questions / Assumptions:**
* Superclass links are facts in the cartridge; chains resolved at index-build time.

---

## US-804: Extract-Method / Extract-Temp Refactorings

* **ID:** US-804
* **Status:** Backlog (vision — milestones 1.1–1.4) — #80
* **Epic:** EPIC-008
* **Priority:** Medium
* **Estimate:** L
* **Date Proposed:** 2026-06-22
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **developer**, I want **AST-driven extract-method / extract-temp refactorings**, so that **I can restructure code safely.**

**Acceptance Criteria (AC):**
* AC1: Extract-method and extract-temp over the US-411 AST; idempotent; **builds on US-426** scope rename.
* AC2: Selector rename across the system treated with **extreme caution** (dynamic dispatch).

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-426 (scope rename) + US-411 (AST).
* [X] Estimated/sized (L).

**Definition of Done (DoD) Checklist:**
* [ ] Extract-method + extract-temp; idempotence tests; PO accepts.

**Notes / Questions / Assumptions:**
* The famous Smalltalk refactoring-browser capabilities, brought to a file workflow on our AST.
