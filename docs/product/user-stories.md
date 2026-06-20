# vscode-smalltalk User Stories

> **Status summary (2026-06-20).** v0.2.0, v0.3.0, and v0.4.0 are shipped. **Done:** US-101–106 & US-200–203 (declarative foundation, v0.2.0), US-301 (Run Current File, v0.3.0), US-410 (LSP scaffold, v0.3.0), **US-411** (error-tolerant parser + symbol table, internal milestone M3) and **US-412** (outline + workspace symbols + go-to-definition, v0.4.0). **Next:** US-413 (completion, 0.5.0). **Planned:** US-413–416. **Superseded by [ADR-0001](../decisions/0001-typescript-bundled-lsp-server.md):** US-401–403 (the server is TypeScript, not Smalltalk). The per-story `Status` fields below reflect this; see [`docs/ROADMAP.md`](../ROADMAP.md) for the live milestone view.

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
* **Status:** Planned
* **Epic:** EPIC-004
* **Priority:** High *(directly answers issue #1)*
* **Estimate:** L
* **Date Proposed:** 2026-06-13
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Smalltalk developer**, I want **auto-completion for selectors, class names, and local/instance variables, including standard kernel-library selectors**, so that **I can write code faster with fewer lookups.**

**Acceptance Criteria (AC):**
* AC1: A build-time generator parses `../smalltalk-3.2.5/kernel/*.st` with our own parser into `server/data/kernel-index.json` (classes, superclass chains, selectors + arity).
* AC2: Completion offers keyword selectors after a receiver (workspace > kernel ranking, prefix + camel-hump matching).
* AC3: Completion offers class names in expression-head position and temp/instance variables from the symbol-table scopes.
* AC4: Multi-part keyword selectors insert as snippets (e.g. `at:put:` → `at:${1} put:${2}`).
* AC5: A setting `smalltalk.completion.kernelLibrary` (`gst-3.2.5` | `off`) controls the kernel index.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-411 + US-412 indexes.
* [X] Licensing reviewed (kernel selector names/arities are facts; method comment prose is LGPL 2.1 — names/signatures only in v0.5.0).
* [X] Estimated/sized.

**Definition of Done (DoD) Checklist:**
* [ ] Kernel-index generator + completion provider implemented.
* [ ] **Language Server:** unit tests at cursor positions; index generator snapshot test.
* [ ] **End-to-End:** integration test asserting kernel + workspace completions.
* [ ] GitHub issue #1 closed with a demo.
* [ ] PO accepts the story.

**Notes / Questions / Assumptions:**
* Decide on shipping kernel method-comment text (with attribution) before hover (US-415).

---

## US-414: Diagnostics (Parser Live; gst Opt-In)

* **ID:** US-414
* **Status:** Planned
* **Epic:** EPIC-004
* **Priority:** Medium
* **Estimate:** L
* **Date Proposed:** 2026-06-13
* **Owner:** Leonardo Nascimento

**User Story:**
> As a **Smalltalk developer**, I want **error squiggles as I type, plus optional real compile errors from `gst`**, so that **I catch mistakes without leaving the editor.**

**Acceptance Criteria (AC):**
* AC1: Parser diagnostics (syntax errors/warnings) are published on change, debounced, with code `smalltalk(parse)`.
* AC2: An opt-in setting `smalltalk.diagnostics.useGst` (default off) runs `gst` on save, parses stderr (`file.st:LINE: error: ...`), maps to ranges, and tags source `gst`.
* AC3: gst processes time out and are killed on edit; diagnostics never block typing.
* AC4: Trivial code actions (e.g. insert missing `]`) are offered where cheap.

**Definition of Ready (DoR) Checklist:**
* [X] Depends on US-411 (parser diagnostics are free from it).
* [X] gst integration is strictly optional.
* [X] Estimated/sized.

**Definition of Done (DoD) Checklist:**
* [ ] Live parser diagnostics + opt-in gst path implemented.
* [ ] **Language Server:** unit tests for diagnostic ranges; stderr-parsing tests.
* [ ] **End-to-End:** integration test asserting squiggles on malformed input.
* [ ] No zombie gst processes under rapid edits.
* [ ] PO accepts the story.

**Notes / Questions / Assumptions:**
* Verify gst stderr format against the bundled GNU Smalltalk 3.2.5 sources.

---

## US-415: Hover

* **ID:** US-415
* **Status:** Planned
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
* [ ] Hover provider implemented.
* [ ] **Language Server:** unit tests for hover content per symbol kind.
* [ ] **End-to-End:** integration test asserting hover Markdown.
* [ ] PO accepts the story.

**Notes / Questions / Assumptions:**
* Reuses the workspace + kernel indexes from US-412/US-413.

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
