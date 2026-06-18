# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins
**Type**: Requirements Quality Gate
**Story**: US-410 — LSP scaffold

---

## Section 1: Constitution Gates (Mandatory)
- [x] **Native Look & Feel**: Uses standard VS Code APIs (`vscode-languageclient`/`-server`, output channel, trace setting).
- [x] **Zero Config**: No config required; trace defaults to `off`; works with no `gst` (AC5).
- [x] **Protocol First**: This story *is* the LSP bring-up — client↔server over LSP/IPC.
- [x] **Robustness**: Server restart-on-crash defined (AC3); handshake must not error without `gst` (AC5).
- [x] **Dialect Agnostic**: Server is dialect-neutral plumbing; dialect logic arrives behind the parser (US-411).

## Section 2: Specification Completeness
- [x] Goals and Non-Goals explicitly listed (§2/§3; US-301 and real features excluded).
- [x] User stories in standard format (§4).
- [x] Acceptance scenarios (Given/When/Then) defined (§4).
- [x] Edge cases identified (no `gst`; server crash/restart; VSIX excludes source/`node_modules`).
- [x] Dependencies listed (`vscode-languageclient` 9.x, `vscode-languageserver(-textdocument)`, esbuild; engine ≥1.82).

## Section 3: Technical Design
- [x] API/Command contracts defined: LSP `initialize` capabilities; `smalltalk.trace.server` setting.
- [x] Data structures defined: N/A for a no-op server (capabilities object only; AST/symbols are US-411).
- [x] Error handling strategy defined: client restart policy; graceful no-`gst` activation.
- [x] Testing strategy defined: `@vscode/test-cli` integration test for the handshake; `vsce ls` packaging assertion; existing `npm run eval` unaffected.

## Section 4: Validation Result
- [x] PASS - Ready for implementation
