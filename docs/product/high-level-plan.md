# High-Level Plan: vscode-smalltalk Extension

**Document Version:** 1.0
**Date:** 2025-05-02
**Author:** Leonardo Nascimento

## 1. Introduction & Vision

Our vision is to create the **premier Smalltalk development experience within Visual Studio Code**. We aim to establish VS Code as a highly productive, reliable, and intuitive environment for Smalltalk developers, enabling seamless integration into modern development workflows. This plan focuses specifically on the **`vscode-smalltalk`** extension, which will provide foundational editing support and language intelligence.

## 2. Target Audience

Our primary focus is on **Smalltalk developers working with GNU Smalltalk** in a **file-based (`.st` files) workflow**. This includes students, professionals, researchers, and hobbyists who use or wish to use VS Code as their primary editor. While initial features target GNU Smalltalk specifics, foundational elements like syntax highlighting aim for broad applicability.

## 3. Core Value Proposition (for `vscode-smalltalk`)

The `vscode-smalltalk` extension will provide a seamless, reliable, and increasingly intelligent Smalltalk development environment integrated directly into VS Code. The value will be delivered incrementally:

1.  **Solid Foundation:** Excellent basic editing support (accurate syntax highlighting, useful snippets, intuitive indentation, comment handling).
2.  **Language Intelligence (LSP):** Advanced coding assistance via the Language Server Protocol (smart auto-completion, real-time diagnostics, code navigation, formatting).

*Note: Integrated debugging capabilities are planned as a future enhancement, likely via a separate extension (`vscode-smalltalk-debugger`).*

## 4. Technical Approach Summary (for `vscode-smalltalk`)

The `vscode-smalltalk` extension involves:

* A **VS Code extension client** (written in TypeScript) responsible for UI integration, command handling, and managing the LSP communication protocol.
* A backend **language server implemented in TypeScript** (`vscode-languageserver-node`), bundled with the extension. **Architecture Decision (2026-06-13):** the server is *not* implemented in Smalltalk. It ships inside the VSIX and is fully functional **without** any external Smalltalk installation, parsing `.st`/`.gst` files with a hand-written, error-tolerant Smalltalk parser.
    * **Optional `gst` integration:** GNU Smalltalk (`gst`), when available and configured, is spawned only as an *optional external tool* — for the "Run Current File" command and opt-in compile diagnostics. It is never required for language intelligence.
    * **Dialect layering:** the parser is structured as a core ANSI-Smalltalk layer plus pluggable container formats (GST chunk `!` syntax and GST brace/class syntax), leaving the door open to other dialects (e.g. Pharo/Tonel) later without a rewrite.
    * **Protocol:** Communication between the client and the bundled server adheres to the standard **Language Server Protocol (LSP)**.
* A strong initial focus on supporting **GNU Smalltalk** semantics, runtime behavior, and tooling integration for both declarative features and LSP capabilities.

> **Note:** This supersedes the earlier plan (and EPIC-004 / US-401–403) to implement the server in Smalltalk over hand-rolled JSON-RPC. GNU Smalltalk has had no stable release since 2013; a bundled TypeScript server is more portable, more maintainable, and works for every user out of the box.

## 5. Modularity Strategy Context

This plan details the development of the primary **`vscode-smalltalk`** extension. This extension will encompass:

* All **Declarative Language Features** (Syntax Highlighting, Snippets, Language Configuration).
* Integration of **Language Server Protocol (LSP)** features (client-side logic plus the bundled TypeScript language server) for code intelligence.

A separate extension, tentatively named `vscode-smalltalk-debugger`, is envisioned for future development to handle Debug Adapter Protocol (DAP) integration and debugging features. This aligns with the Single Responsibility Principle.

## 6. Phased Rollout Plan (for `vscode-smalltalk`)

Development of the `vscode-smalltalk` extension will proceed iteratively through the following high-level phases:

* **Phase 1: Solidify Foundation, DX & Technical Investigation**
    * **Focus:** Enhance and refine declarative features (syntax, snippets, config). Improve user onboarding via clear documentation (README.md). Implement basic workflow commands (e.g., running files via configured `gst`), ensuring alignment with VS Code best practices. Stand up the bundled TypeScript LSP scaffold (US-410) to de-risk Phase 2. *(The earlier plan to research a Smalltalk LSP backend was superseded by [ADR-0001](../decisions/0001-typescript-bundled-lsp-server.md).)*
    * **Extension(s):** `vscode-smalltalk`
* **Phase 2: Language Intelligence (LSP)**
    * **Focus:** Implement and integrate core LSP features. Develop the bundled TypeScript language server (hand-written Smalltalk parser + symbol table) targeting GNU Smalltalk support. Provide features like diagnostics, completion, hover, and navigation.
    * **Extension(s):** `vscode-smalltalk`

*(Phase 3, focusing on Integrated Debugging (DAP), will be planned separately and involve the future `vscode-smalltalk-debugger` extension).*

## 7. Key Principles

* **Agile & Iterative:** Deliver value incrementally through well-defined User Stories.
* **User-Centric:** Prioritize features based on the needs of GNU Smalltalk developers using file-based workflows.
* **Value-Driven:** Focus on delivering tangible benefits in each phase for the `vscode-smalltalk` extension.
* **Quality Focused:** Emphasize robust implementation, clear documentation, and a comprehensive, multi-layered testing strategy:
    * **TypeScript Client:** Unit tests (e.g., Mocha/Chai/Jest).
    * **Language Server:** Unit tests for the parser/symbol table (snapshot + mutation tests) and LSP integration tests via `@vscode/test-cli`.
    * **End-to-End:** Integration tests using `@vscode/test-cli` and `@vscode/test-electron` verifying client-server communication within VS Code.
* **Collaborative:** Engage actively (PO, Architect, Dev Team) to refine requirements and ensure alignment.
* **Architecturally Sound:** Adhere to VS Code best practices, LSP/DAP standards, and maintain clean separation of concerns.

## 8. Next Steps / Current Focus

Phase 1 (declarative foundation + onboarding docs) shipped in **v0.2.0**; the TypeScript
client/server scaffold and the *Run Current File* command shipped in **v0.3.0**. Phase 2 is
underway: the error-tolerant parser + symbol table (**M3 / US-411**) and the first navigation
features — outline, workspace symbol search, and go-to-definition (**US-412**) — shipped in
**v0.4.0**, with semantic folding + document highlight (**US-417**) in **v0.4.1**. **Completion + a
GNU Smalltalk kernel index (US-413)** is code-complete and in **0.5.0** release prep (manual-QA gate +
tag pending). Current focus next is **US-414** — diagnostics (parser live; `gst` opt-in); hover and
formatting follow.

See [`docs/ROADMAP.md`](../ROADMAP.md) for live milestone status, [`epics.md`](epics.md) and
[`user-stories.md`](user-stories.md) for the backlog, and [`docs/decisions/`](../decisions/)
for architecture decisions.

