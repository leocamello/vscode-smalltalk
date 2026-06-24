# US-422 — Manual QA workspace (Semantic Tokens)

A ready-to-open workspace for the `verification.md` §4 matrix. It exercises every role
the provider classifies (AC1), the headline **cartridge-driven class vs unknown-global**
distinction (AC2), pseudo-variables and keyword-selector parts (AC3), and the **no-cartridge
capitalization fallback** (AC4) — all offline, **no `gst` needed**.

> Semantic tokens are *invisible without the right tool*: they only re-tint what the TextMate
> grammar already colored, and only when a theme maps the token type. **Verify with the token
> inspector, not your eyes** — it reports the exact semantic type.

## Open it
1. In the Extension Development Host (`F5` from the repo): **File → Open Folder…** → this
   `manual-qa-workspace/` folder. The extension auto-activates (the folder has `.st` files).
2. Run **Developer: Inspect Editor Tokens and Scopes** (Command Palette). A hover panel
   follows the cursor; under **semantic token type** it shows the type + any modifiers
   (e.g. `class` / `defaultLibrary`). That field is the source of truth for this story.
3. The status bar (bottom-right) shows `Smalltalk kernel: …`. After editing
   `.vscode/settings.json`, **reload the window** (`Ctrl+R` / `Cmd+R`).

`Roles.st` is valid GST and must parse with **0 diagnostics** — any red squiggle is a bug.

---

## Part A — role classification (default settings: `auto`, a cartridge is loaded)

Put the cursor on each token in `Roles.st`; read **semantic token type** in the inspector.

| Row | Token (where) | Expect type | Modifier |
|---|---|---|---|
| **AC1-ivar** | `balance` / `entries` (in `init`/`deposit:`) | `property` | — |
| **AC1-classvar** | `DefaultLimit` (in `limit`/`setLimit:`) | `property` | **`static`** |
| **AC1-temp** | `receipt` (in `deposit:`) | `variable` | — |
| **AC1-param** | `amount` (in `deposit:`), `n` (in `setLimit:`) | `parameter` | — |
| **AC1-blockarg** | `each` (in `report`'s block) | `parameter` | — |
| **AC2-kernel** | `OrderedCollection` (in `init`), `Object` (header) | `class` | **`defaultLibrary`** |
| **AC2-workspace** | `Vault` (use site, last lines) | `class` | *(no `defaultLibrary`)* |
| **AC2-global** | `Transcript` (in `report`) | `variable` | — *(not a known class ⇒ global)* |
| **AC2-unknown** | `Zork` (use site) | `variable` | — *(unknown capitalized ⇒ global, not class)* |
| **AC3-keyword** | `deposit:` / `setLimit:` parts, `do:` (selectors) | `method` | — |
| **AC3-unary** | `printString`, `new`, `isNil` (selectors) | `method` | — |
| **AC3-binary** | `+`, `*`, `\|` (selectors) | `method` | — |
| **AC3-pseudo** | `self` / `super` / `nil` / `true` / `false` / `thisContext` | `keyword` | — |

> **AC1-shadowing spot-check:** there is no shadow in this fixture, but the rule (a lexical
> param/temp wins over a same-named class field) matches the hover scope rule — trust the
> unit test `AC1` rows for that edge.

---

## Part B — selectors vs everything else (AC3, a TextMate-interaction check)

This is where US-415 found a grammar bug, so look carefully:

| Row | Do | Expect |
|---|---|---|
| **B1** | Inspect `deposit:` in `Vault new deposit: 16rFF` | `method` — the keyword part is a selector, **not** colored like the `amount` parameter beside it |
| **B2** | Inspect the `16rFF` next to it | **no** semantic token (literals are left to the grammar) — and its trailing `.` stays the separator color, not the number color (radix-integer grammar regression from US-415) |
| **B3** | Inspect `each printString` in the block | `each` → `parameter`, `printString` → `method` — adjacent tokens get **distinct** types |

---

## Part C — AC2/AC4 cartridge matrix (`.vscode/settings.json` → reload)

Change `smalltalk.completion.kernelLibrary`, **reload the window**, re-inspect. The
**workspace** rows (AC2-workspace `Vault`, all AC1 roles) must be **unchanged** in every tier —
only the unknown-capitalized rows move.

| Row | Setting | Status bar | `Zork` (unknown) | `OrderedCollection` (kernel) |
|---|---|---|---|---|
| **C1** | `"auto"` | `installed (gst)` | `variable` (global) | `class` + `defaultLibrary` |
| **C2** | `"bundled"` | `reference (gst 3.2.5)` | `variable` (global) | `class` + `defaultLibrary` |
| **C3** | `"off"` | `off` | **`class`** (AC4 capitalization fallback) | **`class`** (no cartridge — fallback) |

This is the whole differentiator: **with the cartridge, the extension knows `OrderedCollection`
is a class and `Zork` is not — offline, no `gst`.** With the cartridge off, it can only guess by
capitalization (AC4).

---

## Part D — robustness & shipped artifact

| Row | Do | Expect |
|---|---|---|
| **R1** | Mid-edit: delete the closing `]` of `deposit:` so a squiggle appears, then inspect `balance`/`OrderedCollection` higher up | still classified correctly — the provider degrades, never throws (no error toast, no missing-token cascade) |
| **V1** | `npm run package` → install the VSIX in a clean VS Code → open this folder | repeat **AC2-kernel**, **AC2-unknown (C1)**, and **C3** — the shipped build matches the dev build |

---

Record results in `../verification.md` §4, fill the date/sign-off line, then tick §5.
Anything that doesn't match the expected type above is a bug — note it and stop.
