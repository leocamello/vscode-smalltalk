# Specification: Run Current File

**ID**: US-301
**Feature**: Run Current File
**Status**: Draft
**Owner**: Leonardo Nascimento
**Created**: 2026-06-19

## 1. Overview
Add a `Smalltalk: Run Current File` command that executes the active `.st`/`.gst` file with the
configured GNU Smalltalk interpreter (`gst`) in the integrated terminal. This is the first
user-facing *command* and the first place the extension shells out to `gst` — as an **optional
tool** only (the language server never needs it). It builds on the US-410 client scaffold.

## 2. Goals
- Register `smalltalk.runCurrentFile` ("Smalltalk: Run Current File"), available in the Command Palette and the editor context menu, only for Smalltalk files.
- Resolve `gst` from the `smalltalk.gnuSmalltalkPath` setting, falling back to the system `PATH`.
- Run the file in the VS Code integrated terminal, surfacing `stdout`/`stderr`, handling paths with spaces.
- Fail helpfully when `gst` is missing (actionable error with a shortcut to settings).

## 3. Non-Goals
- No language intelligence (that's the LSP server, US-411+).
- No build/test/REPL tasks, no `TaskProvider`, no debugging (later/ separate stories).
- No bundling of `gst`; the user supplies it.

## 4. User Stories & Acceptance Criteria
**US-301**: As a Smalltalk developer, I want a command to run my current `.st` file using the
configured GNU Smalltalk interpreter, so that I can quickly execute and test my code without
leaving VS Code.

- **AC1**: Command `smalltalk.runCurrentFile`, title "Smalltalk: Run Current File", is registered.
- **AC2**: It is searchable/runnable from the Command Palette.
- **AC3**: It is only enabled/visible for Smalltalk files (`when: editorLangId == smalltalk`, which covers `.st` and `.gst`).
- **AC4**: The `smalltalk.gnuSmalltalkPath` setting exists in `package.json` (string, documented). *(Already contributed in v0.2.0.)*
- **AC5**: On run, the `gst` path is resolved first from `smalltalk.gnuSmalltalkPath`.
- **AC6**: If that setting is empty, `gst` is looked up on the system `PATH`.
- **AC7**: If `gst` is found via neither, an informative `showErrorMessage` is shown with an action that opens the `smalltalk.gnuSmalltalkPath` setting.
- **AC8**: If found, the active file is executed as `<gst> <file>`.
- **AC9**: Execution happens in the VS Code integrated terminal (`createTerminal`).
- **AC10**: `stdout`/`stderr` appear in that terminal.
- **AC11**: File paths (incl. spaces) are correctly quoted when passed to `gst`.

**Acceptance scenarios**
- *Given* `gst` on PATH and an open `.st` file, *when* I run the command, *then* a "Smalltalk" terminal opens and runs `gst "<file>"`, showing its output.
- *Given* no `gst` configured or on PATH, *when* I run the command, *then* I see an error offering "Open Settings", and no terminal command runs.
- *Given* an unsaved `.st` file, *when* I run the command, *then* it is saved first so `gst` executes current content.
- *Given* a non-Smalltalk editor, *then* the command is hidden from the palette/context menu.

## 5. Technical Design
- **`client/src/gstLocator.ts`** — a **pure** `resolveGst(opts)` taking the configured path, `PATH`,
  the platform delimiter, candidate exe names (`gst`, plus `gst.exe` on Windows), and an
  `isExecutable(path)` probe; returns `{ path, source: 'setting' | 'path' } | undefined`. Pure →
  unit-testable in Node (mirrors the US-410 handshake-test approach).
- **`client/src/commands/runCurrentFile.ts`** — command handler: get active editor; bail if not
  `smalltalk`; `document.save()`; resolve `gst` (real `fs` probe + `process.env.PATH`); on failure
  `showErrorMessage('GNU Smalltalk (gst) was not found…', 'Open Settings')` →
  `workbench.action.openSettings` for `smalltalk.gnuSmalltalkPath`; on success reuse/create a named
  terminal ("Smalltalk") and `sendText(\`${quote(gst)} ${quote(file)}\`)`.
- **`client/src/extension.ts`** — register the command in `activate()` (alongside the LSP client);
  push to `context.subscriptions`. Optional shared output channel for logging.
- **`package.json` contributes** — `commands` entry + `menus.commandPalette` and
  `menus.editor/context` with `when: editorLangId == smalltalk`. Setting AC4 already present.
- **README** — add a Commands section and confirm the Configuration section documents the setting.

## 6. Risks & Limitations
- **Quoting/cross-platform**: terminal shells differ (pwsh/cmd/bash); quote both paths and prefer a form that works across default shells. Mitigation: quote with double quotes; unit-test the command string builder.
- **`gst` discovery**: we do a lightweight executable probe, not a full `which`; document that the setting is the reliable path.
- **Terminal reuse**: reuse a single "Smalltalk" terminal to avoid clutter; recreate if disposed.
