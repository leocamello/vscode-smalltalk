# US-430 — Manual QA workspace

A ready-to-open workspace for the `verification.md` §3 matrix. The cartridge convergence is
**behaviour-preserving**: completion should look exactly like 0.5.0, with **two intended changes** —
the status-bar label (**frozen reference** vs installed) and the completion-item detail
(`kernel (reference)` / `kernel (installed)`).

## Open it
1. In the Extension Development Host (`F5` from the repo): **File → Open Folder…** → this
   `manual-qa-workspace/` folder. The extension auto-activates (the folder contains `.st` files).
2. The status bar (bottom-right) shows `Smalltalk kernel: …`. Note: **reload the window** (`Ctrl+R` /
   `Cmd+R`) whenever you change the kernel setting in `.vscode/settings.json`.

## Trigger completion
Type the text, then `Ctrl+Space` if the popup doesn't auto-open. The **detail** (provenance) is the greyed
text beside the selected item; press `Ctrl+Space` again to expand the details pane.

## Row-by-row script

| Row | Where | Do | Expect |
|---|---|---|---|
| 1 | `Account.st` `deposit:` | type `amount print` | `printString`, `printNl`, `printOn:` offered |
| 2 | `Account.st` `describe` | type `ba`, then `OC` | `balance` (Field icon) above classes; `OrderedCollection` by camel-hump |
| 3 | `Account.st` `record:value:` | type `history at`, accept `at:put:` | inserts `at:${1} put:${2}`, two Tab stops |
| 4 | settings → `"bundled"`, reload | read status bar | **`reference (gst 3.2.5)`** |
| 5 | settings → `"auto"`, reload | read status bar | **`installed (gst)`** (this box has gst) |
| 6 | any kernel item | read the greyed detail | **`kernel (reference)`** on floor / **`kernel (installed)`** installed |
| 7 | settings → `"auto"` + bogus `kernelPath`, reload | — | one-time toast: bundled reference (GST 3.2.5) + *Open Settings* |
| 8 | settings → `"off"`, reload | type `amount print` | no kernel items; only workspace symbols; status bar **off** |
| 9 | `Account.st` `describe` (on floor) | type `Err`, then `Inv` | `Error`, `InvalidArgument` offered — broader base image (SystemExceptions etc.), no noise/dupes |
| 10 | `Scratch.st` `broken` | type after the incomplete `x`; add a new method | partial completions, never throws; new selector completable after ~debounce |
| 11 | — | fresh defaults, no gst | useful completions out of the box from the frozen floor |
| 12 | clean VSIX | `npm run package` → install in clean VS Code → repeat rows 1, 3, 4 | shipped artifact matches the dev build |

## What "changed vs 0.5.0" looks like
- **Before:** status `bundled (gst 3.2.5)`, detail `kernel (bundled)`.
- **After:** status `reference (gst 3.2.5)`, detail `kernel (reference)`. Installed wording is
  unchanged (`installed (gst)` / `kernel (installed)`); it gains a version (`installed (gst X.Y.Z)`)
  automatically if/when one is known.

Record results in `../verification.md` §3, fill the date/sign-off line, then tick §5.
