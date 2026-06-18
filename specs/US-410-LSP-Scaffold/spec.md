# Specification: LSP scaffold (client + bundled server)

**ID**: US-410
**Feature**: TypeScript LSP scaffold
**Status**: Draft
**Owner**: Leonardo Nascimento
**Created**: 2026-06-19

## 1. Overview
Stand up the TypeScript foundation for all language-intelligence work: a VS Code **extension
client** and a **bundled language server** (`vscode-languageserver-node`), wired together with
esbuild and connected over Node IPC. This story ships *no user-facing feature* — it delivers a
no-op server that connects, advertises capabilities, and proves the toolchain (bundling,
activation, client↔server handshake) end-to-end. Per [ADR-0001](../../docs/decisions/0001-typescript-bundled-lsp-server.md),
the server is bundled in the VSIX and runs with **no external Smalltalk (`gst`) dependency**.

## 2. Goals
- Establish an npm-workspaces repo layout (`client/`, `server/`) with a shared strict `tsconfig`.
- Bundle `dist/extension.js` and `dist/server.js` with esbuild; make `vscode:prepublish` build both bundles **and** the grammar.
- Start the bundled server from the client over IPC, scoped to the `smalltalk` language, with automatic restart on crash and a configurable trace level.
- Keep the existing declarative features (grammar, snippets, language config) and the v0.2.0 VSIX hygiene intact.

## 3. Non-Goals
- No real language features (symbols, completion, diagnostics, hover, formatting) — those are US-411..416.
- No parser/AST work (US-411).
- No `gst` process integration or "Run Current File" (that is US-301).
- No change to the TextMate grammar or its snapshot harness beyond relocation if needed.

## 4. User Stories & Acceptance Criteria
**US-410**: As a developer, I want a TypeScript client + bundled language server wired together,
so that language features can be built on a server that ships in the VSIX and runs without `gst`.

- **AC1**: The repo is restructured into `client/` and `server/` npm workspaces with a shared strict `tsconfig.base.json` (`strict`, `noUncheckedIndexedAccess`, ES2022, Node16/NodeNext modules).
- **AC2**: esbuild bundles `dist/extension.js` and `dist/server.js` (CJS, `external: ['vscode']`); `vscode:prepublish` runs `build:grammar` **and** the production esbuild build. `npm run package` produces a VSIX containing the bundles and the grammar, and **no** `node_modules`, `client/src`, or `server/src`.
- **AC3**: On opening a `.st`/`.gst` file, the client starts the server (`TransportKind.ipc`) with `documentSelector: [{ language: 'smalltalk' }]`; the server restarts automatically on crash; a `smalltalk.trace.server` setting controls LSP tracing.
- **AC4**: The no-op server responds to `initialize` advertising its (currently empty/placeholder) capabilities and incremental `textDocumentSync`; the client reaches the `running` state, observable in the "Smalltalk Language Server" output/trace.
- **AC5**: With no `gst` on `PATH` and no `smalltalk.gnuSmalltalkPath` set, activation and the handshake still succeed (server has zero Smalltalk-runtime dependency).

**Acceptance scenarios**
- *Given* a fresh install with no `gst`, *when* I open a `.st` file, *then* the Smalltalk language server starts and reaches `running` with no error notification.
- *Given* the packaged VSIX, *when* I inspect it (`vsce ls`), *then* it contains `dist/extension.js`, `dist/server.js`, and `syntaxes/gnu-smalltalk.tmLanguage.json`, but no source or `node_modules`.
- *Given* the running server, *when* it crashes, *then* the client restarts it (bounded retries) without reloading the window.

## 5. Technical Design
- **Layout** (npm workspaces; modeled on `microsoft/vscode-extension-samples/lsp-sample`):
  - `package.json` (root) — `workspaces: ["client","server"]`, `main: ./dist/extension.js`, `engines.vscode ^1.82`, contributes unchanged; scripts: `compile`/`watch` (esbuild), `package`, `vscode:prepublish`, plus existing `build:grammar`/`eval`/`new-story`.
  - `tsconfig.base.json` — strict shared config; `client/tsconfig.json` and `server/tsconfig.json` extend it.
  - `esbuild.mjs` — two entry points → `dist/extension.js`, `dist/server.js`; `--production` minifies; `external: ['vscode']`.
  - `client/src/extension.ts` — `activate()` creates a `LanguageClient` pointing at `dist/server.js` via IPC; `deactivate()` stops it. Output channel "Smalltalk Language Server".
  - `server/src/server.ts` — `createConnection(ProposedFeatures.all)`, `TextDocuments`, `onInitialize` returning capabilities (incremental sync; feature providers added in later stories), `listen()`.
- **Settings**: add `smalltalk.trace.server` (`off`|`messages`|`verbose`, default `off`).
- **Grammar harness**: `src/test/` stays as-is (grammar tooling, not server code); `npm run eval` unchanged. `.vscodeignore` extended to exclude `client/**`, `server/**`, `esbuild.mjs`, `tsconfig*.json`, `node_modules`.
- **CI**: add `npm ci` + `npm run compile` + lint; keep the eval + package smoke (now asserting the two bundles are present).

## 6. Risks & Limitations
- **Bundling correctness**: esbuild must externalize `vscode` and bundle `vscode-languageserver`/`-textdocument` into `dist/server.js`; verify the VSIX runs without `node_modules`. Mitigation: `package` smoke test loads the bundles.
- **Workspaces + vsce**: `vsce package` must not pull workspace `node_modules` into the VSIX. Mitigation: `--no-dependencies` and a `vsce ls` assertion in CI.
- **Engine floor**: `vscode-languageclient` 9.x requires VS Code ≥ 1.82 (already bumped in v0.2.0).
- **Scope creep**: resist adding real features here; the connected no-op server is the whole deliverable.
