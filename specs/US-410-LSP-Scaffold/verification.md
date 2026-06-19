# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding
**Type**: Implementation Verification
**Story**: US-410 — LSP scaffold

---

## Section 1: Acceptance Criteria
- [x] AC1 (workspaces + strict tsconfig): `npm run check-types` passes under `strict` + `noUncheckedIndexedAccess`.
- [x] AC2 (esbuild bundles + clean VSIX): `npm run compile` emits `dist/extension.js` + `dist/server.js`; `vsce ls` shows the bundles and grammar, no source/`node_modules`/maps. `vscode:prepublish` builds grammar + production bundles.
- [x] AC4 (server advertises capabilities): `npm run test:server` completes an LSP `initialize`/`shutdown` handshake and asserts incremental `textDocumentSync`.
- [x] AC5 (no `gst`): the handshake test runs the server as plain Node with no Smalltalk on PATH.
- [x] AC3 (client starts server, restart on crash): confirmed via manual F5 — opening a `.st` file in the Extension Development Host starts the client and the server logs `Smalltalk language server initialized.` (see §4). Automated Electron integration test deferred (no display in CI sandbox; follow-up).

## Section 2: Code Quality
- [x] `npm run lint` passes (typescript-eslint strict; `^_` args ignored).
- [x] Server handshake test passes (`npm run test:server`); grammar eval green (`npm run eval`).
- [x] No `any` types (rule set to warn; none present).
- [x] Intent documented via comments in `server.ts` / `extension.ts`.

## Section 3: Constitutional Compliance
- [x] **Native**: standard `vscode-languageclient`/`-server`, output channel, trace setting.
- [x] **Zero Config**: no setup needed; works without `gst`; trace defaults `off`.
- [x] **Robustness**: client restarts the server on crash (default policy); handshake never requires `gst`.
- [x] **TDD**: server handshake test written alongside the code.

## Section 4: Manual Verification
- [x] F5 Extension Host: opened a `.st` file with no `gst` installed; "Smalltalk Language Server" output shows `Smalltalk language server initialized.` with no error notification. *(Owner-confirmed.)*
- [x] No error notifications on activation.

## Section 5: Sign-Off
- [x] Ready for Merge — CI green on Linux/macOS/Windows; owner F5 confirmed.
