# US-416 — Manual QA workspace (Formatting)

A ready-to-open workspace for the `verification.md` §4 matrix. It exercises **conservative, idempotent
whitespace-only formatting** (document + range + on-type) **offline, no `gst` needed**:

- `Messy.st` — **valid** GNU Smalltalk (parses with **0 diagnostics**) but deliberately badly spaced,
  indented, blank-line-padded, and un-aligned. Formatting normalizes the layout **without changing any
  code** (only whitespace between tokens; ADR-0005).
- `Broken.st` — deliberately **malformed** (an unclosed method block → 1 parse diagnostic). Formatting must
  be **non-destructive**: it produces **no edits** and changes nothing (the data-loss safety gate, AC5).

> `Messy.st` must parse with **0 diagnostics** (no red squiggles) — any squiggle there is a bug. `Broken.st`
> *should* show a squiggle (that's the point) and must be left **byte-for-byte unchanged** by formatting.

## Open it
1. In the Extension Development Host (`F5` from the repo): **File → Open Folder…** → this
   `manual-qa-workspace/` folder. The extension auto-activates (the folder has `.st` files).
2. **Formatting is OFF by default** (AC4). First run **Part A** to confirm the no-op, then turn it on:
   open Settings (`Ctrl+,`), search **`smalltalk.format.enable`**, and check it (or add
   `"smalltalk.format.enable": true` to the workspace `.vscode/settings.json`).
3. Format commands: **Format Document** (`Shift+Alt+F`), **Format Selection** (`Ctrl+K Ctrl+F` with text
   selected), and on-type (automatic as you edit, if `editor.formatOnType` is on).

---

## Part A — Off by default (AC4)

| Row | Setting | Action | Expect |
|---|---|---|---|
| **A1** | `smalltalk.format.enable` = `false` (default) | Open `Messy.st`, run **Format Document** | **Nothing changes.** No edits. (The provider returns `[]` while disabled.) |

> After A1, set `smalltalk.format.enable` = `true` for the rest of the matrix.

## Part B — Document formatting (AC2)

Open `Messy.st`, run **Format Document** (`Shift+Alt+F`). Expect exactly this result:

```
Object subclass: Account [
    | balance owner |

    init [balance := 0]

    deposit: amount [balance := balance + amount. ^self]

    report [Transcript
            showCr: 'opening';
            showCr: 'closing';
            nl]

    transfer: amount to: other [self balance < amount ifTrue: [^self error: 'insufficient']. other deposit: amount. ^self]

    describe [^SystemExceptions.NotYetImplemented]
]
```

| Row | Check |
|---|---|
| **B1** | Assignment / binary spacing normalized: `balance:=0` → `balance := 0`, `balance+amount` → `balance + amount`. |
| **B2** | The 3 blank lines after `| balance owner |` collapse to **one**. The single blank lines between methods are **kept**. |
| **B3** | The cascade in `report` aligns one segment per line (`showCr: …; showCr: …; nl`). |
| **B4** | The leading **class comment** (`"Messy.st — …"`) is preserved verbatim. |
| **B5** | `SystemExceptions.NotYetImplemented` stays **tight** — the namespace `.` does **not** gain spaces (it must not become a statement period). |
| **B6** | Keyword colons get one space after (`ifTrue: [`, `error: 'insufficient'`, `deposit: amount`); blocks are tight (`[^self …]`, `[self balance …`). |

## Part C — Idempotence + range + on-type (AC1, AC3)

| Row | Action | Expect |
|---|---|---|
| **C1** | Run **Format Document** on `Messy.st` a **second** time | **No further changes** (idempotent — `format(format(x)) == format(x)`). |
| **C2** | Select just the `deposit: amount [...]` line, **Format Selection** (`Ctrl+K Ctrl+F`) | Only that line is reformatted; other lines are untouched. |
| **C3** | In a block, put the cursor inside `[ … ]` and type `]` on its own indented line | The `]` line dedents to its block's level (on-type). |

## Part D — Non-destructive on error (AC5)

| Row | Action | Expect |
|---|---|---|
| **D1** | Open `Broken.st`, run **Format Document** | **No edits, nothing changes** — the whole file is left byte-for-byte intact because it has a parse error. (This is the trust gate: a formatter must never corrupt code it can't fully understand.) |
| **D2** | Confirm the red squiggle on the unclosed block is still present afterward | Yes — formatting neither fixes nor hides diagnostics. |

## Part E — Expanded block style (`blockStyle: expand`)

Set **`smalltalk.format.blockStyle`** = `expand` (default is `preserve`), then **Format Document** on
`Messy.st`. The bodies now reflow one statement per line:

```
Object subclass: Account [
    | balance owner |

    init [
        balance := 0
    ]

    deposit: amount [
        balance := balance + amount.
        ^self
    ]

    report [
        Transcript
            showCr: 'opening';
            showCr: 'closing';
            nl
    ]

    transfer: amount to: other [
        self balance < amount ifTrue: [^self error: 'insufficient'].
        other deposit: amount.
        ^self
    ]

    describe [
        ^SystemExceptions.NotYetImplemented
    ]
]
```

| Row | Check |
|---|---|
| **E1** | Each method body opens on the `[` line, statements indent one level, `]` on its own line. |
| **E2** | **Single-statement argument blocks stay inline** — `ifTrue: [^self error: 'insufficient']` is *not* expanded. |
| **E3** | Blank lines between methods are **preserved** (one blank each). |
| **E4** | Format a **second** time → no further changes (still idempotent). Switch back to `preserve` → bodies collapse to the Part B layout. |

---

### Notes
- All behavior is **offline** — no `gst` is consulted by formatting.
- Spacing follows GNU Smalltalk kernel conventions (**tight brackets**: `[^x]`, `[:each | …]`).
- Knobs: `smalltalk.format.indentSize` (default 4), `smalltalk.format.cascades` (`align`|`preserve`),
  `smalltalk.format.keywordWrap` (column threshold, default 100; `0` disables wrapping).
