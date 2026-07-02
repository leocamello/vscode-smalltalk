# US-428 — Manual QA workspace (Class Rename)

A ready-to-open workspace for the `verification.md` §4 matrix. It exercises **safe, workspace-wide class
rename** (F2 / Rename Symbol) — **offline, no `gst`** — across **every reference form**, plus the explicit
**rejections** (kernel class, collision). One class (`Shape`) is defined in one file and referenced from three
others, proving multi-file rename, the **Refactor Preview** gate, **shadowing safety**, **namespaced/binding**
resolution, and the **kernel boundary**.

- `Shape.st` — the primary class: `Object subclass: Shape [ … ]`, declares `| name sides |`, uses the kernel
  class `OrderedCollection` (for the reject/collision rows).
- `Shape-Draw.st` — references `Shape` in a **second** file in four forms: `Shape extend`, the superclass
  position `Shape subclass: Circle`, `Shape class extend`, and the receiver send `Shape new`. `Circle` is a
  new class (never renamed when renaming `Shape`).
- `Shape-Shadow.st` — `Shape extend [ … ]` whose method `demo` has a **temporary named `Shape`** (a shadow).
  Renaming the class must rewrite the `extend` receiver but **leave the local `Shape` untouched**.
- `Shape-Namespace.st` — the **binding constant** `#{Shape}` and the default-namespace qualified reference
  `Smalltalk.Shape`; renaming rewrites the **class segment only** (`#{…}` / `Smalltalk.` preserved). `Painter`
  is a new class (never renamed).

> All four files parse with **0 diagnostics** — any red squiggle is a bug.

## Open it
1. In the Extension Development Host (`F5` from the repo): **File → Open Folder…** → this
   `manual-qa-workspace/` folder. The extension auto-activates (the folder has `.st` files); the server
   indexes all four so cross-file class rename resolves.
2. Rename = **F2** (or right-click → **Rename Symbol**). A multi-file rename shows the **Refactor Preview**;
   a rejected position pops an error message instead.

---

## Part A — Workspace-wide class rename, all forms (AC1, AC2, AC6)

| Row | Where | Action | Expect |
|---|---|---|---|
| **A1** | `Shape.st`, the `Shape` in `Object subclass: Shape [` | F2 → `Polygon`, press **Enter** | Because the rename spans 4 files, VS Code **forces the Refactor Preview** (does *not* apply on Enter). You see edits in **all four files**: the declaration, `Shape extend`/`Shape subclass:`/`Shape class extend`/`Shape new`, the `#{Shape}`→`#{Polygon}` and `Smalltalk.Shape`→`Smalltalk.Polygon` segments. Confirm/Apply → all update; `name`/`sides`/`Circle`/`Painter` untouched. |
| **A2** | `Shape-Draw.st` after A1 | inspect | `Shape subclass: Circle` became `Polygon subclass: Circle` — the **superclass** `Shape` renamed, `Circle` (the new class) unchanged. |
| **A3** | `Shape-Namespace.st` after A1 | inspect | `#{Shape}`→`#{Polygon}` and `Smalltalk.Shape`→`Smalltalk.Polygon` — only the **class segment** changed; `#{…}` and `Smalltalk.` preserved. |

## Part B — Shadowing safety (AC3)

| Row | Where | Action | Expect |
|---|---|---|---|
| **B1** | `Shape-Shadow.st` after A1 | inspect | Line 6: `Polygon extend [` — the **receiver** renamed; but inside `demo` the temporary `\| Shape \|`, `Shape := 1`, and `^Shape` are **unchanged** (a same-named local is a different symbol). |

## Part C — Kernel boundary + collisions, rejected with a reason (AC1, AC4)

| Row | Where | Action | Expect |
|---|---|---|---|
| **C1** | `Shape.st`, the `OrderedCollection` in `parts` | F2 | **Rejected** — "kernel class, read-only" (a cartridge class can't be renamed). No rename box edit is applied. |
| **C2** | `Shape.st`, the `Shape` declaration | F2 → `OrderedCollection` | **Refused** — new name collides with an existing **kernel** class. No edit. |
| **C3** | `Shape.st`, the `Shape` declaration | F2 → `Circle` | **Refused** — new name collides with an existing **workspace** class (`Circle` in `Shape-Draw.st`). No edit. |
| **C4** | `Shape.st`, the `Shape` declaration | F2 → `polygon` (lower-case) | **Refused** — not a valid class identifier (`[A-Z]…`). No edit. |

## Part D — Idempotence (AC5)

| Row | Where | Action | Expect |
|---|---|---|---|
| **D1** | After A1 (`Shape`→`Polygon`) | rename `Polygon` back to `Shape` | Round-trips cleanly (idempotent); the workspace returns to its original text, all files 0-diag. |

---

### Notes
- **Reset between runs:** `git checkout -- specs/US-428-Class-Rename/manual-qa-workspace/` restores the
  fixtures after an applied rename.
- VS Code may write `manual-qa-workspace/.vscode/settings.json` — it is **gitignored**; do not commit a stray
  `enable:true`.
