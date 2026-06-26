# US-412 — Manual QA workspace (Code Navigation) — also covers US-417

A self-contained, reproducible workspace for the navigation surface, **offline, no `gst`**:
**outline, workspace symbol search, and go-to-definition** (US-412) plus **semantic folding and
scope-aware document highlight** (US-417, "navigation polish" built on the same index). Back-filled
post-0.9.0 to modernize US-412's original corpus-based matrix into the self-contained pattern.

> `Navigation.st` is valid GST and must parse with **0 diagnostics** — any red squiggle is a bug.

## Open it
1. In the Extension Development Host (`F5` from the repo): **File → Open Folder…** → this folder.
   The extension auto-activates (the folder has a `.st` file).
2. Keep the **Developer Tools console** open (Help → Toggle Developer Tools) — confirm **no errors** throughout.

---

## Part A — Outline / Document Symbols (US-412 AC1)

Open `Navigation.st`, then the **Outline** view (and the breadcrumb bar).

| Row | Expect |
|---|---|
| **A1** | Three top-level classes: **`Shape`**, **`Circle`**, **`Gallery`**. |
| **A2** | `Shape` nests: ivar **`name`**, a **class-side** `named:`, and `setName:` / `name` / `describe` / `area`. |
| **A3** | `Circle` nests ivar **`radius`** + `radius:` / `describe` / `area`; the breadcrumb shows `Circle` subclasses `Shape`. |
| **A4** | Clicking any entry **jumps the cursor** to its definition. |

## Part B — Workspace Symbols (US-412 AC2)

| Row | Do | Expect |
|---|---|---|
| **B1** | `Ctrl/Cmd+T`, type `Circle` | The class `Circle` appears; selecting it opens its definition. |
| **B2** | `Ctrl/Cmd+T`, type `describe` | **Both** implementors (`Shape>>describe`, `Circle>>describe`) appear; each opens correctly. |
| **B3** | `Ctrl/Cmd+T`, type `area` | Both `Shape>>area` and `Circle>>area`. |

## Part C — Go to Definition (US-412 AC3; plural is US-423)

| Row | Do | Expect |
|---|---|---|
| **C1** | **F12** on `Circle` in `Shape subclass: Circle` (or its use in the bottom cascade) | Jumps to the `Circle` class definition. |
| **C2** | **F12** on `describe` in `each describe` (inside `describeAll`) | A **picker with both** implementors (`Shape>>describe`, `Circle>>describe`) — never silently one. |
| **C3** | **F12** on `name` inside `Circle>>describe` | Resolves to the `name` accessor / the inherited ivar (a candidate set, not an error). |

## Part D — Live update (US-412 AC4)

| Row | Do | Expect |
|---|---|---|
| **D1** | Add a method `volume [ ^0 ]` to `Circle`, **don't save** | The Outline shows `volume` within ~250 ms (debounced, version-keyed cache — no save needed). |
| **D2** | Delete it again | The Outline drops `volume`. |

## Part E — Folding + Document Highlight (US-417)

| Row | Do | Expect |
|---|---|---|
| **E1** (AC1) | Hover the gutter — fold the `Gallery` **class body**, then the `describeAll` **method body**, then the `do:` **block** | Each folds independently; the comment block at the top folds too. |
| **E2** (AC2) | Put the cursor on **`describe`** (a selector) | Every **send/definition of `describe`** in the file highlights (scope-aware). |
| **E3** (AC2) | Put the cursor on the ivar **`radius`** inside `Circle` | Its uses **within `Circle`** highlight — not unrelated names. |
| **E4** (AC2) | Put the cursor on the temp **`aString`** in `setName:` | Only that parameter's occurrences in that method highlight. |

---

Record results in `../verification.md` §4. `Navigation.st` must stay at **0 diagnostics**; a missing
outline entry, a collapsed (single-target) go-to-def on `describe`, or a highlight that escapes its
scope is a bug — note it and stop.
