# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins
**Type**: Requirements Quality Gate
**Story**: US-412 — Document/workspace symbols + go-to-definition

---

## Section 1: Constitution Gates (Mandatory)
- [x] **Native Look & Feel**: Uses standard VS Code surfaces — Outline, breadcrumbs, `Ctrl/Cmd+T`, `F12` — driven entirely by LSP `documentSymbol`/`workspace/symbol`/`definition`. No custom UI.
- [x] **Zero Config**: No settings; the workspace scan and parse cache are automatic; works on first open.
- [x] **Protocol First**: Pure LSP providers over the bundled server; the client is unchanged plumbing.
- [x] **Robustness**: Built on the never-throws front end; a malformed file yields a partial outline (manual row 6). Definition returns all candidates rather than guessing.
- [x] **Dialect Agnostic**: Consumes the dialect-neutral `buildSymbolTable`; brace + chunk formats both covered (AC1).

## Section 2: Specification Completeness
- [x] Goals and Non-Goals explicitly listed (§2/§3; completion/diagnostics/hover/formatting excluded).
- [x] User stories in standard format (§4).
- [x] Acceptance scenarios (Given/When/Then) defined (§4).
- [x] Edge cases identified: chunk vs brace outline, multi-implementor definition, live edits/debounce, malformed file, large file, no `gst`, `files.exclude`, clean-install VSIX.
- [x] Dependencies listed: US-411 (`parse`, `buildSymbolTable`) — merged; `vscode-languageserver` types; `@vscode/test-cli` for integration.

## Section 3: Technical Design
- [x] API/Command contracts defined: the three LSP requests + advertised server capabilities (§5).
- [x] Data structures defined: `SymbolNode → DocumentSymbol`/`WorkspaceSymbol` mapping; the `(uri,version)` parse cache; the workspace index.
- [x] Error handling strategy defined: never-throws front end; empty/partial results instead of failures; index guards for large trees.
- [x] Testing strategy defined: unit (mapping + resolution) + integration (`@vscode/test-cli` on a fixture workspace) + a **manual verification matrix** (§6 / `verification.md`) gating 0.4.0.

## Section 4: Validation Result
- [x] PASS - Ready for implementation

**Emphasis**: per PO direction, 0.4.0 does **not** ship on automated tests alone — the §6 manual
verification matrix (real-corpus workspace, outline, search, go-to-def, live editing, robustness,
no-`gst`, cross-platform, clean-install VSIX) is a hard gate, recorded in `verification.md`.
