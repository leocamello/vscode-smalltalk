# Tasks: LSP scaffold

**ID**: US-410 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Map tasks to acceptance criteria where possible.

## Phase 1 — Spec & Setup
- [x] T001 Spec reviewed; `requirements-validation.md` gate passed.
- [x] T002 ROADMAP reordered (US-410 scaffold into 0.3.0) — drift fixed.

## Phase 2 — Implementation
- [ ] T010 [AC1] `tsconfig.base.json` (strict) + `client`/`server` tsconfigs.
- [ ] T011 [AC1] Root `package.json`: workspaces, `main`, scripts, `smalltalk.trace.server` contribute.
- [ ] T012 [AC2] `esbuild.mjs` → `dist/extension.js` + `dist/server.js`; `vscode:prepublish` builds grammar + bundles.
- [ ] T013 [AC1] `eslint.config.mjs` (typescript-eslint strict).
- [ ] T014 [AC4] `server/` workspace: `package.json` + `src/server.ts` (no-op `onInitialize` capabilities).
- [ ] T015 [AC3] `client/` workspace: `package.json` + `src/extension.ts` (LanguageClient over IPC, restart, output channel).
- [ ] T016 [AC2] `.vscodeignore`: exclude `client/**`, `server/**`, `esbuild.mjs`, `tsconfig*.json`, `**/*.map`.
- [ ] T017 CI: `npm ci` + `lint` + `compile`; package smoke asserts both bundles.

## Phase 3 — Verify
- [ ] T900 [AC2] `npm run compile` emits both bundles; `npm run eval` (grammar) still green; `lint` passes.
- [ ] T901 [AC2] `npx vsce ls` shows `dist/extension.js` + `dist/server.js` + grammar; no source/`node_modules`.
- [ ] T902 [AC3/AC4/AC5] F5 Extension Host: open a `.st` file with no `gst`; server reaches `running`; `verification.md` gate passed.
- [ ] T903 CI green on Linux/macOS/Windows.
