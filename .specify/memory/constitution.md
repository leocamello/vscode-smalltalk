# vscode-smalltalk Constitution

The definitive guide for developing the VS Code Smalltalk extension.

## Core Principles

### I. Native Look & Feel
The extension must feel like a built-in part of VS Code:
- Use standard VS Code APIs (LSP, DAP, FileSystemProvider) over custom solutions.
- Follow VS Code UI guidelines (notifications, status bar, tree views).
- Respect user themes and standard keybindings.
- Avoid custom webviews unless strictly necessary for complex visualizations.

### II. Zero Configuration
The extension should work out of the box for standard installations:
- Automatically detect GNU Smalltalk (`gst`) and other supported dialects in common paths (`PATH`, `/usr/bin`, etc.).
- Provide sensible defaults for all settings.
- Prompt users helpfully if configuration is required, rather than failing silently.

### III. Protocol First (LSP/DAP)
Strict adherence to the Language Server Protocol (LSP) and Debug Adapter Protocol (DAP):
- Implement features via LSP/DAP whenever possible, rather than client-side logic.
- Ensure compatibility with standard clients.
- Isolate dialect-specific logic behind standard protocol adapters.

### IV. Dialect Agnostic Architecture
While initially focused on GNU Smalltalk, the architecture must support multiple dialects:
- Core client logic should be generic.
- Dialect-specific strategies (parsing, execution, image interaction) must be abstracted.
- New dialects should be addable via adapters without rewriting the core.

### V. Robustness & Resilience
The extension must handle the "dirty" state of code editing:
- Parsing must tolerate syntax errors and incomplete code.
- Never crash the extension host; handle backend failures gracefully.
- Restart language servers/repls automatically upon failure where appropriate.

### VI. Test-Driven Development
High quality is non-negotiable:
- **TypeScript Client**: Unit tests (Mocha) for all logic. Integration tests (@vscode/test-electron / @vscode/test-cli) for workflows.
- **Language Server (TypeScript)**: Unit tests for the parser/symbol table (snapshot + mutation tests) and LSP behaviour.
- **Grammar**: Snapshot tests (`npm run test:grammar`).
- Tests must exist and pass — and **CI must be green on Linux, macOS, and Windows** — before feature merge.

### VII. No Telemetry
The extension collects **no telemetry** and phones home to no analytics service:
- No usage tracking, no third-party crash reporting, no network calls except the user's own opt-in `gst` invocations and standard VS Code/Marketplace mechanisms.
- Privacy is a product feature and a marketing point; any future opt-in diagnostics would require explicit, documented consent and a setting defaulting to off.

## Technical Constraints

### Language & Runtime
- **Client**: TypeScript (Strict Mode).
- **Runtime**: Node.js (VS Code bundled version).
- **Language Server**: TypeScript (`vscode-languageserver-node`), **bundled in the VSIX** and fully functional without any external Smalltalk install. See [ADR-0001](../../docs/decisions/0001-typescript-bundled-lsp-server.md).
- **`gst` (GNU Smalltalk)**: an *optional external tool* only — used for "Run Current File" and opt-in compile diagnostics, never required for language intelligence.

### State Management
- Use `vscode.Memento` for persistence.
- Minimal global state in the client.

### Performance
- **Startup**: Activation < 500ms.
- **Typing Latency**: Syntax highlighting/diagnostics must not block the UI thread.
- **Workspace index**: indexing a 1,000-file workspace completes in < 5 s; completion responds in < 100 ms (targets enforced/bug-bashed in the 0.9.0 hardening pass — US-901).
- **Memory**: Be mindful of large images or file inputs; honor cancellation tokens and file-size guards.

## Development Standards

### Code Style
- Use `eslint` and `prettier` configurations provided.
- TypeScript: Explicit types (avoid `any`).
- JSDoc/TSDoc for all public APIs.

### Testing Standards
- **Unit Tests**: Cover >80% of business logic.
- **Integration Tests**: Cover key user journeys (Quick Start, Debugging, Hover).
- **Manual QA**: Verify in a clean VS Code instance.

## Governance

### Constitution Authority
- This document supersedes other practices.
- Changes require documented rationale and team agreement.

### Amendment Log
- **1.1.0 (2026-06-13):** Principle VI and Technical Constraints updated — the language server is implemented in TypeScript and bundled (not a Smalltalk backend); `gst` is an optional tool. Added the cross-platform CI gate. See [ADR-0001](../../docs/decisions/0001-typescript-bundled-lsp-server.md).
- **1.2.0 (2026-06-21):** Added Principle VII (No Telemetry) and the workspace-index/completion performance budgets — migrated from the genesis `plan.md` into the binding hierarchy as part of the 0.5.0 plan reconciliation. See [ADR-0002](../../docs/decisions/0002-kernel-symbol-sourcing.md) and US-901.

**Version**: 1.2.0 | **Ratified**: 2026-02-08 | **Last Amended**: 2026-06-21
