# Tasks: Run Current File

**ID**: US-301 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Map tasks to acceptance criteria where possible.

## Phase 1 — Spec & Setup
- [x] T001 Spec authored from issue #13 ACs; `requirements-validation.md` gate passed.

## Phase 2 — Implementation
- [x] T010 [AC5/AC6/AC11] `client/src/gstLocator.ts`: pure `resolveGst()` + `buildRunCommand()`.
- [x] T011 [AC1/AC8/AC9/AC10] `client/src/commands/runCurrentFile.ts`: save, resolve, terminal run.
- [x] T012 [AC7] Missing-`gst` error with "Open Settings" action.
- [x] T013 [AC1] Register command in `client/src/extension.ts`.
- [x] T014 [AC1/AC2/AC3] `package.json`: command + commandPalette + editor/context menus (`editorLangId == smalltalk`).
- [x] T015 [AC5/AC6/AC11] `client/test/gstLocator.test.ts` + `npm run test:client` (6 tests).
- [x] T016 README Commands section added.

## Phase 3 — Verify
- [x] T900 `check-types` + `lint` + `compile` pass; `npm run test:client` green; `npm run eval` unaffected.
- [x] T901 [AC4] `vsce ls` still clean (only `dist/` bundles; no new source leaked).
- [ ] T902 [AC7–AC11] Manual F5: run with/without `gst`; output in terminal; spaces-in-path works (owner-confirmed).
- [ ] T903 `verification.md` gate passed; CI green on Linux/macOS/Windows.
