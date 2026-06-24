# US-415 — Manual QA workspace (Hover)

A ready-to-open workspace for the `verification.md` §4 matrix. It exercises every hover
acceptance criterion (AC1–AC5) **and** the headline of slice B: **comment prose gated by
provenance** — bundled reference is facts-only; the locally-built installed kernel and your
own workspace source carry comments.

> The expected outputs below were captured from the real provider against this repo's fixtures,
> so they should match **exactly** (modulo the long implementor lists, which are abbreviated here).

## Open it
1. In the Extension Development Host (`F5` from the repo): **File → Open Folder…** → this
   `manual-qa-workspace/` folder. The extension auto-activates (the folder has `.st` files).
2. **Hover** = mouse over a token and wait for the popup, or put the cursor on it and press the
   hover key (default **Ctrl+K Ctrl+I** / `editor.action.showHover`).
3. The status bar (bottom-right) shows `Smalltalk kernel: …`. After editing
   `.vscode/settings.json`, **reload the window** (`Ctrl+R` / `Cmd+R`).

`Bank.st` and `Literals.st` are valid GST and must parse with **0 diagnostics** — any red
squiggle in them is a bug. (`Scratch.st` is deliberately broken; see row R1.)

---

## Part A — content per symbol kind (default settings: `auto` → installed)

| Row | File · where | Hover | Expect |
|---|---|---|---|
| **AC1a** | `Bank.st` · bottom `Bank new rate printString` | `printString` | `**printString** — unary selector`, fenced `printString`, **Implementors:** `Integer`, `Object`, then Object's comment *"Return the base 10 representation of the receiver"* |
| **AC1b** | `Bank.st` · `ledger at: name put: entry.` | `at:put:` (on `at:` or `put:`) | `keyword selector`, fenced `at: a1 put: a2`, **Implementors:** `Dictionary`, … *(+N more)*, then a method comment |
| **AC1c** | `Bank.st` · bottom `Bank new rate …` | `rate` | `unary selector`, fenced `rate`, **Implementors:** `Bank`, then *"Answer the default interest rate."* (workspace prose) |
| **AC2a** | `Bank.st` · bottom `Bank new …` | `Bank` | `**Bank** — class`, fenced chain `Bank → Object`, then *"A tiny bank for the US-415 hover tests. …"* |
| **AC2b** | `Bank.st` · `init` → `OrderedCollection new` | `OrderedCollection` | chain `OrderedCollection → SequenceableCollection → Collection → Iterable → Object`, then the class comment |
| **AC3a** | `Bank.st` · `init` → `accounts := …` | `accounts` | `**accounts** — instance variable`, "Declared at line …" |
| **AC3b** | `Bank.st` · `deposit:into:` header | `amount` (or `name`) | `parameter`, "Declared at line …" |
| **AC3c** | `Bank.st` · `deposit:into:` → `entry := …` | `entry` | `temporary`, "Declared at line …" |
| **AC3d** | `Bank.st` · `setRate:` → `DefaultRate := r` | `DefaultRate` | `class variable`, "Declared at line …" |
| **AC4** | `Literals.st` · `samples` array | each literal | see the table below |
| **AC5** | any row above | — | content is **Markdown** with a fenced code block; multi-line comments render as prose |
| **AC5-neg** | `Literals.st` · `nonNumeric` array | `'hi'`, `#tag`, `$a` | **no hover** (strings/symbols/chars carry no extra facts — expected) |

### AC4 — numeric literal decode (`Literals.st`)
| Hover | Expect |
|---|---|
| `16rFF` | `16rFF = 255 (base 16)` |
| `2r1010` | `2r1010 = 10 (base 2)` |
| `8r17` | `8r17 = 15 (base 8)` |
| `3.14s2` | `3.14s2 = 3.14 (scale 2)` |
| `100s` | `100s = 100 (scale default)` |
| `255` | `255 = 255` |
| `3.5` | `3.5 = 3.5` (float literal) |

> **Syntax coloring check (grammar):** the statement-separating `.` *after* each radix
> integer (`16rFF.`, `2r1010.`, `8r17.`) must be the **separator color**, NOT the number
> color — a bare `16rFF.` is a radix integer + period, not a radix float. A real radix
> float still needs a fraction digit (`16rF.F`). (Regression-pinned in the grammar snapshot.)

---

## Part B — prose gated by provenance (the slice B headline)

Edit `.vscode/settings.json`, change `smalltalk.completion.kernelLibrary`, **reload the window**,
then re-check the rows. Workspace prose (AC1c, AC2a, AC3*) must be **unchanged** in every tier —
only the **kernel** hovers change.

| Row | Setting | Status bar | Re-check AC1a / AC1b / AC2b (kernel) | Re-check AC1c / AC2a (workspace) |
|---|---|---|---|---|
| **P1** | `"auto"` | `installed (gst)` | facts **+ comments** (Object/at:put:/OrderedCollection prose shown) | comments shown |
| **P2** | `"bundled"` | `reference (gst 3.2.5)` | **facts only** — signature/implementors/chain shown, **NO comment** | comments **still** shown |
| **P3** | `"off"` | `off` | `OrderedCollection` → **no hover**; `printString` → signature only, no Implementors line | comments **still** shown |

This is the whole point: **provenance, not dialect, gates prose.** The shipped reference cartridge
never carries LGPL kernel comments; comments only ever come from files already on your machine.

---

## Part C — robustness & live edit (`Scratch.st`)

`Scratch.st` is intentionally incomplete: the half-typed keyword send `total foo:`
leaves the method unable to close, so a squiggle appears near the end of the method
(2 diagnostics). This is the mid-typing state where hover must still work.

| Row | Do | Expect |
|---|---|---|
| **R1a** | hover `count` in `count := 0` | `instance variable` + declaration site — works despite the parse error |
| **R1b** | hover `total` in `total := count`, and `16rFF` | `temporary` + decl site; `16rFF = 255 (base 16)` |
| **R1c** | hover the class name `Scratch` | `class`, chain `Scratch → Object` |
| **R1d** | hover the half-typed `foo:` | `keyword selector` (no implementors yet) — and **no crash** anywhere |
| **R2** | finish the method (`total foo:` → `^total`), add a new method whose body opens with a double-quoted comment, add a send `Scratch new <sel>` and hover the selector | within ~a second (debounced re-index) the hover shows its facts + your comment |

---

## Part D — shipped artifact (clean VSIX)

| Row | Do | Expect |
|---|---|---|
| **V1** | `npm run package` → install the VSIX in a clean VS Code → open this folder | repeat **AC1a, AC2a, AC4** and **P2** — the shipped build matches the dev build; bundled reference stays facts-only |

---

Record results in `../verification.md` §4, fill the date/sign-off line, then tick §5.
Anything that doesn't match the expected output above is a bug — note it and stop.
