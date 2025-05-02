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

* A **VS Code extension client** (written primarily in TypeScript) responsible for UI integration, command handling, and managing the LSP communication protocol.
* A backend **language server** implemented in Smalltalk.
    * **Implementation Flexibility:** While the server *must* provide accurate support for **GNU Smalltalk** users and workflows, the specific Smalltalk dialect used to *implement* the server (e.g., GNU Smalltalk or Pharo) may be chosen based on technical advantages (available libraries, tooling for JSON-RPC, reflection, process management).
    * **Protocol:** Communication between the client and the backend server will strictly adhere to the standard **Language Server Protocol (LSP)**, primarily via JSON-RPC over stdio.
* A strong initial focus on supporting **GNU Smalltalk** semantics, runtime behavior, and tooling integration for both declarative features and LSP capabilities.

## 5. Modularity Strategy Context

This plan details the development of the primary **`vscode-smalltalk`** extension. This extension will encompass:

* All **Declarative Language Features** (Syntax Highlighting, Snippets, Language Configuration).
* Integration of **Language Server Protocol (LSP)** features (client-side logic and management of the Smalltalk LSP server process) for code intelligence.

A separate extension, tentatively named `vscode-smalltalk-debugger`, is envisioned for future development to handle Debug Adapter Protocol (DAP) integration and debugging features. This aligns with the Single Responsibility Principle.

## 6. Phased Rollout Plan (for `vscode-smalltalk`)

Development of the `vscode-smalltalk` extension will proceed iteratively through the following high-level phases:

* **Phase 1: Solidify Foundation, DX & Technical Investigation**
    * **Focus:** Enhance and refine declarative features (syntax, snippets, config). Improve user onboarding via clear documentation (README.md). Implement basic workflow commands (e.g., running files via configured `gst`), ensuring alignment with VS Code best practices for future compatibility. Conduct technical investigation and preparatory work for the LSP backend (library research, process management, communication details) to de-risk Phase 2.
    * **Extension(s):** `vscode-smalltalk`
* **Phase 2: Language Intelligence (LSP)**
    * **Focus:** Implement and integrate core LSP features. Develop the Smalltalk LSP server backend targeting GNU Smalltalk support. Provide features like diagnostics, completion, hover, and navigation.
    * **Extension(s):** `vscode-smalltalk`

*(Phase 3, focusing on Integrated Debugging (DAP), will be planned separately and involve the future `vscode-smalltalk-debugger` extension).*

## 7. Key Principles

* **Agile & Iterative:** Deliver value incrementally through well-defined User Stories.
* **User-Centric:** Prioritize features based on the needs of GNU Smalltalk developers using file-based workflows.
* **Value-Driven:** Focus on delivering tangible benefits in each phase for the `vscode-smalltalk` extension.
* **Quality Focused:** Emphasize robust implementation, clear documentation, and a comprehensive, multi-layered testing strategy:
    * **TypeScript Client:** Unit tests (e.g., Mocha/Chai/Jest).
    * **Smalltalk Backend:** Unit and integration tests (e.g., SUnit).
    * **End-to-End:** Integration tests using `@vscode/test-cli` and `@vscode/test-electron` verifying client-server communication within VS Code.
* **Collaborative:** Engage actively (PO, Architect, Dev Team) to refine requirements and ensure alignment.
* **Architecturally Sound:** Adhere to VS Code best practices, LSP/DAP standards, and maintain clean separation of concerns.

## 8. Next Steps / Current Focus

Our immediate focus is on **executing Phase 1**. We will refine the Epics and User Stories for improving the foundational aspects, developer experience, and performing LSP technical investigation.

* Refine initial Epics and define User Stories in `docs/epics.md` and `docs/user-stories.md`.
* Prioritize work for the first development iterations.

