# US-414 — Manual QA workspace (Diagnostics)

A ready-to-open workspace for the `../verification.md` §4 matrix. Each `.st` fixture carries a
self-contained intentional defect (or none, for `Clean.st`); the rows below drive the shipped ACs
(AC1 live parser squiggles + AC4 quick fixes). All expected messages were verified against the live
parser. _(The opt-in `gst` tier — AC2/AC3 — was deferred to EPIC-007; see `../spec.md` §7.)_

## Open it
1. From the repo, press **`F5`** → in the **[Extension Development Host]** window, **File → Open
   Folder…** → select this `manual-qa-workspace/` folder. The extension auto-activates (the folder
   has `.st` files).
2. Open the **Problems** panel (`Ctrl/Cmd+Shift+M`); parser diagnostics are badged **`smalltalk(parse)`**.

## Fixtures
| File | Defect | Parser diagnostic | Quick fix |
|---|---|---|---|
| `Clean.st` | none (valid) | — (silent) | — |
| `MissingBracket.st` | class **and** method body missing `]` | two `Expected "]"` (L26) | one `]]` (both) |
| `MissingParen.st` | unclosed `(` before the `.` | `Expected ")"` (L18) | `)` before the `.` |
| `MissingBrace.st` | dynamic array missing `}` | `Expected "}"` (L13, after `3`) | `}` |
| `MissingAttribute.st` | method pragma missing `>` | `Expected ">" to close attribute` (L14, after `'geometry'`) | `>` |
| `UnterminatedString.st` | string never closed | `Unterminated string literal` (L15) | `'` at end of open line |
| `NoQuickFix.st` | truncated expression | `Unexpected EOF` (L15) | — (none: nothing to insert) |

## Row-by-row script (maps to `../verification.md` §4)

| Row | File | Do | Expect |
|---|---|---|---|
| 1 ✦ | `MissingBracket.st` | open it | red squiggle near the end within ~250 ms; Problems badges it **`smalltalk(parse)`** |
| 2 | `Clean.st` | inside `describe`, type `total prin`, pause, then finish to `total printNl` | no flicker storm; nothing settles as an error on valid code; never blocks typing |
| 3 ✦ | `MissingBracket.st` | apply the row-6 quick fix (or type the `]`) | the squiggle clears on the debounce |
| 4 | — | (a Warning-severity parser case, if you can reproduce one) | shows as a **Warning**, not an Error (severity as emitted) |
| 5 ✦ | `MissingBracket.st` | close the tab **without saving** | its diagnostics disappear (cleared on close) |
| 6 ✦ | `MissingBracket.st` | caret on the squiggle → `Ctrl/Cmd+.` | a **single** `Insert missing "]]"` (class **and** method both need a `]`); one apply inserts both and **fully clears**. `Ctrl/Cmd+Z` to re-run |
| 7 | `MissingParen.st` | caret on the squiggle → `Ctrl/Cmd+.` | `Insert missing ")"`; applies `)` **before** the `.` and clears (a `)` after the `.` would not fix it) |
| 8 | `MissingBrace.st` | caret on the squiggle → `Ctrl/Cmd+.` | squiggle sits on the **`{ 1. 2. 3` line** (after `3`), not the line below; `Insert missing "}"` applies there and clears |
| 9 | `MissingAttribute.st` | caret on the squiggle → `Ctrl/Cmd+.` | squiggle sits on the **`<category: …` line** (after `'geometry'`), not the body below; `Insert missing ">"` applies there and clears |
| 10 | `UnterminatedString.st` | caret on the squiggle → `Ctrl/Cmd+.` | `Insert missing "'"`; inserts `'` at the **end of the open line** and clears |
| 11 | `NoQuickFix.st` | caret on the squiggle → `Ctrl/Cmd+.` | **no** quick fix offered (a real squiggle, but nothing trivial to insert) |
| 12 ✦ | `Clean.st` | open it | no diagnostics, no quick fixes |
| 13 ✦ | — | Developer Tools console (`Help → Toggle Developer Tools`) during all of the above | no exceptions thrown by the server |

## Clean-VSIX pass
Re-verify the ✦ rows against the shipped artifact: `npm run package` → install the `.vsix` in a fresh
VS Code window (`Extensions: Install from VSIX…`) → open this folder → repeat rows 1, 6, 12.

Record results in `../verification.md` §4, fill the sign-off line in §5.
