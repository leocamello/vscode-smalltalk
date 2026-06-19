# Implementation Plan: Run Current File

**ID**: US-301 | **Date**: 2026-06-19 | **Spec**: ./spec.md | **Branch**: `feature/US-301-run-current-file`

## Summary
Add a `smalltalk.runCurrentFile` command that resolves `gst` (setting → PATH) and runs the active
Smalltalk file in the integrated terminal, with a helpful error when `gst` is missing. Builds on
the US-410 client; keeps `gst` an optional tool.

## Approach
Split pure logic (testable in Node) from VS Code glue:
- `client/src/gstLocator.ts` — pure `resolveGst()` + a `buildRunCommand()` string builder (quoting).
- `client/src/commands/runCurrentFile.ts` — VS Code handler using the pure helpers + `fs` probe.
- Register in `client/src/extension.ts`; declare command/menus in `package.json`.

## Steps
1. `gstLocator.ts`: `resolveGst({ configuredPath, pathEnv, delimiter, exeNames, isExecutable })`
   → `{ path, source } | undefined`; `buildRunCommand(gstPath, filePath)` → double-quoted string.
2. `runCurrentFile.ts`: active-editor guard (`languageId === 'smalltalk'`); `await document.save()`;
   resolve via real `fs.existsSync`/`fs.statSync` probe + `process.env.PATH`; on miss →
   `showErrorMessage(..., 'Open Settings')` → `openSettings` `smalltalk.gnuSmalltalkPath`; on hit →
   reuse/create the "Smalltalk" terminal, `show()`, `sendText(buildRunCommand(...))`.
3. `extension.ts`: `commands.registerCommand('smalltalk.runCurrentFile', runCurrentFile)` pushed to
   `context.subscriptions`.
4. `package.json`: `contributes.commands` (+ `category: "Smalltalk"`); `menus.commandPalette` and
   `menus.editor/context` with `when: editorLangId == smalltalk`.
5. Tests: `client/test/gstLocator.test.mjs` — resolution precedence (setting > PATH), not-found,
   Windows exe names, and command-string quoting (spaces). Add `npm run test:client`.
6. README: Commands section; confirm Configuration documents `smalltalk.gnuSmalltalkPath`.
7. CHANGELOG (Unreleased → 0.3.0 later).

## Dependencies & Risks
- Reuses the US-410 client + the existing `smalltalk.gnuSmalltalkPath` setting. No new runtime deps.
- Risk: cross-shell quoting in the terminal. Mitigation: double-quote both args; unit-test the builder; document that the setting is the reliable path.

## Verification
`npm run check-types`/`lint`/`compile` → `npm run test:client` (pure logic) → `npm run eval`
(unaffected) → F5 Extension Host: with `gst` present run a `.st` file (terminal shows output);
without `gst` confirm the "Open Settings" error; test a path with spaces → CI green on 3 OSes.
