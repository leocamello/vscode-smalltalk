# US-426 — Manual QA workspace (Scope-aware Rename)

A ready-to-open workspace for the `verification.md` §4 matrix. It exercises **safe, scope-aware rename**
(F2 / Rename Symbol) of temporaries, arguments, and **instance variables** — **offline, no `gst`** — and the
explicit **rejections** (selectors, classes, pseudo-vars). A split class across three files proves
**workspace-wide** ivar rename and **shadowing safety**.

- `Account.st` — the primary class: declares `| balance owner |` and uses `balance` in several methods.
- `Account-Report.st` — `Account extend [...]` in a **second** file, also referencing `balance`.
- `Account-Shadow.st` — `Account extend [...]` whose method **argument is named `balance`** (shadows the
  ivar). Renaming the ivar must **not** touch this file.

> All three files parse with **0 diagnostics** — any red squiggle is a bug.

## Open it
1. In the Extension Development Host (`F5` from the repo): **File → Open Folder…** → this
   `manual-qa-workspace/` folder. The extension auto-activates (the folder has `.st` files); the server
   indexes all three so cross-file ivar rename works.
2. Rename = **F2** (or right-click → **Rename Symbol**). VS Code shows a rename box and a **refactor
   preview** (the multi-file edits). A rejected position pops an error message instead.

---

## Part A — Temporary / argument rename (AC1, AC2)

| Row | Where | Action | Expect |
|---|---|---|---|
| **A1** | `Account.st`, the `amount` arg of `deposit:` | F2 → `n` | Both `amount` uses in `deposit:` become `n`; no other method changes. |
| **A2** | `Account.st`, add `bar [ \| acc \| acc := 1. ^acc ]` then F2 on `acc` → `total` | Only that method's `acc` → `total`. | Scope-bounded; a re-rename is a no-op. |

## Part B — Workspace-wide instance-variable rename (AC3, AC6)

| Row | Where | Action | Expect |
|---|---|---|---|
| **B1** | `Account.st`, the `balance` declaration in `\| balance owner \|` | F2 → `funds`, press **Enter** | Because the rename spans 2 files, VS Code **forces the Refactor Preview** (it does *not* apply on Enter) — you see edits in `Account.st` AND `Account-Report.st` with checkboxes. Confirm/Apply → both files updated; `owner` untouched. |
| **B2** | After B1, open `Account-Shadow.st` | inspect | `scaleBy: balance` is **unchanged** — its `balance` is a local argument (shadow), correctly skipped. |
| **B3** | Rename `funds` back to `balance` | F2 | Round-trips cleanly (idempotent); the workspace is back to the original. |

## Part C — Rejections, with a reason (AC1)

| Row | Where | Action | Expect |
|---|---|---|---|
| **C1** | `Account.st`, the selector `deposit:` (the keyword part) | F2 | Rejected: *"Selector rename isn't supported…"* — no edit. |
| **C2** | `Account-Report.st`, `Transcript` (a kernel class) | F2 | Rejected (class/kernel — read-only). |
| **C3** | any `self` / a numeric literal | F2 | Rejected (pseudo-variable / literal). |
| **C4** | `Account.st`, rename `owner` → `balance` (an existing ivar) | F2 | **Refused — collision** (no cross-symbol bleed). |

## Part D — Safety (AC2, AC5)

| Row | Check |
|---|---|
| **D1** | After any accepted rename, all three files still parse with **0 diagnostics** (no broken references). |
| **D2** | No `gst` is involved at any point; kernel/cartridge files are never edited. |
| **D3** | The Developer Tools console shows no errors during the matrix. |

---

### Notes
- Renameable: **temporaries, arguments, instance variables**. Out of scope by design: **selectors**
  (dynamic dispatch), **classes**, kernel/cartridge symbols.
- Cross-file ivar rename only reaches files **under the open workspace folder** (consistent with how
  references/indexing already behave).
