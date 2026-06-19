# Implementation Plan: LSP scaffold

**ID**: US-410 | **Date**: 2026-06-19 | **Spec**: ./spec.md | **Branch**: `feature/US-410-lsp-scaffold`

## Summary
Bring up the TypeScript client + bundled language server using npm workspaces and esbuild,
connected over IPC, with a no-op server that completes the LSP handshake. No real features.

## Approach
Mirror `microsoft/vscode-extension-samples/lsp-sample`, adapted to this repo's existing
declarative contributes and v0.2.0 packaging hygiene. Two esbuild bundles (`dist/extension.js`,
`dist/server.js`); `vscode:prepublish` builds the grammar **and** the bundles. The grammar
harness (`src/test/`) and `npm run eval` are untouched.

## Steps
1. Root `package.json`: add `workspaces: ["client","server"]`, `main: ./dist/extension.js`,
   contributes `smalltalk.trace.server`, and scripts (`compile`, `watch`, `lint`, `package`,
   updated `vscode:prepublish`). Keep `build:grammar`/`eval`/`new-story`.
2. `tsconfig.base.json` (strict, ES2022, Node16) + `client/tsconfig.json` + `server/tsconfig.json`.
3. `esbuild.mjs` — two CJS bundles, `external: ['vscode']`, `--production` minify, sourcemaps in dev.
4. `eslint.config.mjs` (typescript-eslint strict).
5. `server/` workspace: `package.json` (deps `vscode-languageserver`, `-textdocument`),
   `src/server.ts` (connection, `TextDocuments`, `onInitialize` → capabilities, `listen`).
6. `client/` workspace: `package.json` (dep `vscode-languageclient`), `src/extension.ts`
   (`activate` starts `LanguageClient` over IPC at `dist/server.js`; output channel; `deactivate`).
7. `.vscodeignore`: exclude `client/**`, `server/**`, `esbuild.mjs`, `tsconfig*.json`, `**/*.map`.
8. CI: `npm ci` + `npm run lint` + `npm run compile`; keep `npm run eval` + package smoke,
   now asserting `dist/extension.js` and `dist/server.js` are in the VSIX.
9. `@vscode/test-cli` integration test: open a `.st` fixture, assert the client reaches `running`.

## Dependencies & Risks
- New devDeps: `esbuild`, `typescript`, `eslint`/`typescript-eslint`, `@types/node`, `@types/vscode`.
- Workspace deps: `vscode-languageclient` (client), `vscode-languageserver(-textdocument)` (server).
- Risk: esbuild must externalize `vscode` and bundle the LSP libs into `dist/server.js`; VSIX must
  run with no `node_modules`. Mitigated by the `vsce ls` assertion + manual Extension Host smoke.

## Verification
`npm run compile` (both bundles emitted) → `npm run lint` → `npm run eval` (grammar still green)
→ `npx vsce ls` (bundles present, no source/`node_modules`) → F5 Extension Host: open a `.st`
file, confirm "Smalltalk Language Server" reaches `running` with no `gst` installed → CI green
on all three OSes.
