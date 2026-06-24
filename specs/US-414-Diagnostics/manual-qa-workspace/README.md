# US-414 — Manual QA workspace (Diagnostics)

A ready-to-open workspace for the `../verification.md` §4 matrix. Each `.st` fixture carries a
self-contained intentional defect (or none, for `Clean.st`); the rows below drive every AC. All
expected messages here were verified against the live parser **and** real `gst` 3.2.5 on the dev box.

## Open it
1. From the repo, press **`F5`** → in the **[Extension Development Host]** window, **File → Open
   Folder…** → select this `manual-qa-workspace/` folder. The extension auto-activates (the folder
   has `.st` files).
2. The opt-in gst tier is controlled by `.vscode/settings.json` (`smalltalk.diagnostics.useGst`,
   **default false**). After changing a setting, **reload the window** (`Ctrl/Cmd+R`) so the server
   re-reads it.
3. Open the **Problems** panel (`Ctrl/Cmd+Shift+M`) to read the badges: parser diagnostics show
   **`smalltalk(parse)`**, gst diagnostics show **`gst(compile)`**.

## Fixtures
| File | Defect | Parser diagnostic | gst diagnostic (on save, useGst on) |
|---|---|---|---|
| `Clean.st` | none (valid) | — (silent) | — (silent) |
| `MissingBracket.st` | class body missing `]` | `Expected "]" to close definition` (L26) | `parse error, expected ']'` (L26) |
| `MissingParen.st` | unclosed `(` in `compute` | `Expected ")"` (L16) | `parse error, expected ')'` (L17) |
| `UnterminatedString.st` | string never closed | `Unterminated string literal` (L15) | `Unterminated string, attempting recovery` (L16) |

## Row-by-row script (maps to `../verification.md` §4)

> Keep gst **off** (default) for rows 1–6, 16. Turn it **on** (then reload + **save** the file) for
> rows 7, 8, 11, 12.

| Row | File | Do | Expect |
|---|---|---|---|
| 1 ✦ | `MissingBracket.st` | open it | red squiggle near the end within ~250 ms; Problems shows it badged **`smalltalk(parse)`** |
| 2 | `Clean.st` | inside `describe`, type `total prin` then pause, then finish to `total printNl` | no flicker storm; nothing settles as an error on valid code; never blocks typing |
| 3 ✦ | `MissingBracket.st` | apply the row-13 quick fix (or type the `]`) | the squiggle clears on the debounce |
| 4 | — | (a Warning-severity parser case, if you can reproduce one) | shows as a **Warning**, not an Error (severity respected as emitted) |
| 5 ✦ | `MissingBracket.st` | close the tab **without saving** | its diagnostics disappear (cleared on close) |
| 6 | `MissingBracket.st` | gst **off** (default); save | **no** `gst(compile)` entry — only the parser one |
| 7 ✦ | `MissingBracket.st` | set `useGst: true`, reload, **save** | a 2nd diagnostic badged **`gst(compile)`** = `parse error, expected ']'` appears next to the parser one |
| 8 | `MissingParen.st` | run **Smalltalk: Validate with gst** from the palette | saves + runs gst on demand; `gst(compile)` = `parse error, expected ')'` appears (works even if `useGst` is off) |
| 9 | `MissingBracket.st` | `useGst: true` **and** uncomment the bogus `gnuSmalltalkPath` (see settings), reload, save | gst tier silently inert — **no dialog**, no `gst(compile)`; the parser squiggle is unaffected |
| 10 | `MissingBracket.st` | (on a machine with **no gst**) `useGst: true`, save | tier inert; parser tier still works |
| 11 | `MissingBracket.st` | gst on; save (gst squiggle shows), then **edit** the file | the stale `gst(compile)` squiggle clears on the edit (no error against edited text) |
| 12 | `MissingBracket.st` | gst on; rapidly save/edit several times, then in a terminal: `pgrep -a gst` | **no lingering/zombie `gst` processes** |
| 13 ✦ | `MissingBracket.st` | caret on the squiggle → `Ctrl/Cmd+.` | a **single** `Insert missing "]"` quick fix (deduped); applying it inserts `]` and clears the parser squiggle. `Ctrl/Cmd+Z` to re-run |
| 14 | `MissingParen.st` | caret on the squiggle → `Ctrl/Cmd+.` | `Insert missing ")"` offered and applies |
| 15 | `UnterminatedString.st` | caret on the squiggle → `Ctrl/Cmd+.` | **no** bracket quick fix offered (only bracket diagnostics get one) |
| 16 ✦ | `Clean.st` | open / save (with gst on or off) | no diagnostics, no quick fixes |
| 17 ✦ | — | Developer Tools console (`Help → Toggle Developer Tools`) during all of the above | no exceptions thrown by the server |

## Clean-VSIX pass
Re-verify the ✦ rows against the shipped artifact: `npm run package` → install the `.vsix` in a fresh
VS Code window (`Extensions: Install from VSIX…`) → open this folder → repeat rows 1, 7, 13, 16.

Record results in `../verification.md` §4, fill the sign-off line in §5.
