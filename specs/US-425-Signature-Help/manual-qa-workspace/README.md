# US-425 — Manual QA workspace (Signature Help)

A ready-to-open workspace for the `verification.md` §4 matrix. It exercises keyword-message
signature help **offline, no `gst` needed**: `Inventory.st` defines a workspace keyword method
(`#stock:at:`) and sends kernel keyword messages (`#at:put:`, `#at:ifAbsent:`, `#do:separatedBy:`,
`#inject:into:`) so signatures come from the **workspace ∪ the bundled GST 3.2.5 cartridge**. The
popup must **track the active parameter** (the keyword you're currently filling) and present the
matching selectors as an honest prefix union.

> `Inventory.st` is valid GST and must parse with **0 diagnostics** — any red squiggle is a bug.

## Open it
1. In the Extension Development Host (`F5` from the repo): **File → Open Folder…** → this
   `manual-qa-workspace/` folder. The extension auto-activates (the folder has `.st` files).
2. Default settings load the bundled reference cartridge (`auto` → falls back to `reference
   (gst 3.2.5)` with no `gst`; the status bar bottom-right shows the source).
3. Signature help pops automatically as you type a keyword part or a space after one; you can also
   force it with **Ctrl+Shift+Space** (Trigger Parameter Hints). It shows a `selector` line with the
   **active parameter bolded** and a **N of M** counter when several selectors match the prefix.

---

## Part A — Active-parameter tracking (AC1)

Type each line fresh (or place the cursor at the `▮` and press **Ctrl+Shift+Space**).

| Row | Type / cursor | Expect |
|---|---|---|
| **A1** | `bins at: ▮` | Popup appears; **first** keyword highlighted (`at:` bold). Several signatures cycle with ↑/↓: `at:`, `at:put:`, `at:ifAbsent:` … (the `at:` prefix union). |
| **A2** | `bins at: #widget put: ▮` | The **second** parameter is active (`put:` bold); `at:put:` is the shown signature. Typing `put:` narrowed the union (no more `at:ifAbsent:`). |
| **A3** | `… inject: 0 into: ▮` (in `demo`) | `inject:into:` with the **second** parameter (`into:`) active. |
| **A4** | `… do: [ :k | … ] separatedBy: ▮` | `do:separatedBy:` with `separatedBy:` active. |

## Part B — Provenance & the two-tier union (AC1)

| Row | Type / cursor | Expect |
|---|---|---|
| **B1** | `self stock: ▮` | The **workspace** method `stock:at:` is offered; its documentation line reads **`workspace`**. Active parameter `stock:`. |
| **B2** | `self stock: #x at: ▮` | Still `stock:at:`, now the **second** parameter (`at:`) active. |
| **B3** | `bins do: ▮` | Kernel signatures from the **bundled cartridge** appear (`do:`, `do:separatedBy:`); documentation reads **`kernel (reference)`** (or `kernel (installed)` if gst is installed). |

## Part C — Null cases (no popup) (AC1)

| Row | Type / cursor | Expect |
|---|---|---|
| **C1** | `bins values ▮` (unary) | **No** signature popup — unary sends carry no parameter sequence. |
| **C2** | cursor in a receiver, before any keyword (e.g. `bi▮ns at: 1`) | **No** popup. |
| **C3** | `1 + ▮` (binary) | **No** popup. |
| **C4** | `bins frobnicate: ▮` (unknown selector) | **No** popup — nothing in workspace ∪ cartridge matches. |

## Part D — robustness, no-gst, shipped artifact (AC2)

| Row | Do | Expect |
|---|---|---|
| **D1** | Confirm the status bar reads `reference (gst 3.2.5)` (or `installed …`) and repeat **A1/B3** | Kernel signature help works **with no `gst`** (AC2). |
| **D2** | Mid-edit: delete the `]` closing `demo` so a squiggle appears, then trigger help on `bins at: ▮` higher up | Still returns signatures — the provider degrades, never throws (no error toast). |
| **D3** | `npm run package` → install the VSIX in a clean VS Code → open this folder → repeat **A1, A2, B1, C1** | The shipped build matches the dev build. |

---

Record results in `../verification.md` §4, fill the date/sign-off line, then tick §5.
Anything that doesn't match — a popup on a unary/binary send, a wrong active parameter, a missing
workspace signature, a crash on malformed input — is a bug. Note it and stop.
