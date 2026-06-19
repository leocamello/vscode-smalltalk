# Tasks: LSP scaffold

**ID**: US-410 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Map tasks to acceptance criteria where possible.

## Phase 1 — Spec & Setup
- [x] T001 Spec reviewed; `requirements-validation.md` gate passed.
- [x] T002 ROADMAP reordered (US-410 scaffold into 0.3.0) — drift fixed.

## Phase 2 — Implementation
- [x] T010 [AC1] `tsconfig.base.json` (strict) + `client`/`server` tsconfigs.
- [x] T011 [AC1] Root `package.json`: workspaces, `main`, scripts, `smalltalk.trace.server` contribute.
- [x] T012 [AC2] `esbuild.mjs` → `dist/extension.js` + `dist/server.js`; `vscode:prepublish` builds grammar + bundles.
- [x] T013 [AC1] `eslint.config.mjs` (typescript-eslint strict; `^_` args ignored).
- [x] T014 [AC4] `server/` workspace: `package.json` + `src/server.ts` (no-op `onInitialize` capabilities).
- [x] T015 [AC3] `client/` workspace: `package.json` + `src/extension.ts` (LanguageClient over IPC, restart, output channel).
- [x] T016 [AC2] `.vscodeignore`: exclude `client/**`, `server/**`, `esbuild.mjs`, `tsconfig*.json`, `**/*.map`.
- [x] T017 CI: `npm ci` + `check-types` + `lint` + `compile` + `test:server`; package smoke.
- [x] T018 [AC4/AC5] `server/test/handshake.test.mjs` — LSP handshake test (`npm run test:server`).

## Phase 3 — Verify
- [x] T900 [AC2] `npm run compile` emits both bundles; `npm run eval` (grammar) still green; `lint` + `check-types` pass.
- [x] T901 [AC2] `vsce ls` shows `dist/extension.js` + `dist/server.js` + grammar; no source/`node_modules`.
- [x] T902 [AC4/AC5] Server handshake test passes as plain Node (no `gst`).
- [x] T903 [AC3] Manual F5 Extension Host: server reached `running` (owner-confirmed).
- [x] T904 CI green on Linux/macOS/Windows.
