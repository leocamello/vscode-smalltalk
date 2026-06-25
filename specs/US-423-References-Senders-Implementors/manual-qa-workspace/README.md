# US-423 — Manual QA workspace (References + Senders/Implementors)

A ready-to-open workspace for the `verification.md` §4 matrix. It exercises the two-tier
cross-reference engine **offline, no `gst` needed**: the workspace half (`Theatre.st` defines
two implementors of `#greet` and several senders) joined with the bundled GST 3.2.5 **cartridge**
half (kernel selectors `#do:`, `#new`, `#printString`). Every command must present an **honest
lexical union** — ranked, never filtered, with per-row provenance.

> `Theatre.st` is valid GST and must parse with **0 diagnostics** — any red squiggle is a bug.

## Open it
1. In the Extension Development Host (`F5` from the repo): **File → Open Folder…** → this
   `manual-qa-workspace/` folder. The extension auto-activates (the folder has `.st` files).
2. Default settings load the bundled reference cartridge (`auto` → falls back to `reference
   (gst 3.2.5)` with no `gst` installed; the status bar bottom-right shows the source).
3. The **Senders / Implementors** results appear in a panel view (bottom). Commands are on the
   Command Palette (`Smalltalk: Senders of…` / `Implementors of…`) and the editor right-click menu.

---

## Part A — Implementors (workspace ∪ cartridge union, AC1/AC2)

Put the cursor on the token, run the command, read the **panel tree**.

| Row | Where (cursor) | Command | Expect |
|---|---|---|---|
| **A1** | `greet` send in `run: who [ ^who greet ]` | Implementors of… | **2 rows**: `Robot`, `Speaker`, both badged **`workspace`**. Header reads *Implementors of #greet — 2* with the union/uncertainty tooltip. |
| **A2** | `do:` in `cast do: [ … ]` | Implementors of… | **22 rows**, all badged with the kernel source identity (same as the status bar — **`reference (gst 3.2.5)`** bundled, or **`installed (gst)`**) — the offline kernel union. |
| **A3** | `new` in `OrderedCollection new` | Implementors of… | **~42 rows**; the FIRST is **`Stage class`** badged **`workspace`** (class-side, ranked above the cartridge), then the kernel `… class` rows. |

## Part B — Senders (the live workspace half, AC1/AC6)

> **Kernel senders need the bundled reference.** The senders graph (the `crossReference`
> tier) ships only in the **bundled** cartridge. The **installed** adapter emits classes
> only, so under `auto`-resolved-to-`installed` the kernel contributes **implementors but no
> senders** — B2's cartridge rows appear only with `kernelLibrary: "bundled"` (or no gst).
> This is a known limitation, not a bug; see spec §7.

| Row | Where (cursor) | Command | Expect |
|---|---|---|---|
| **B1** | `greet` (anywhere it appears) | Senders of… | **2 rows**: `Stage » run:` and `Stage » announceAll`, both **`workspace`**. (Both receivers are dynamic — no hint badge — and that is honest: we can't prove the class.) |
| **B2** | `do:` in `cast do:` | Senders of… | **~189 rows**: the FIRST is **`Stage » announceAll`** (`workspace`, ranked first), the rest cartridge. The long tail is present, not hidden. |
| **B3** | `new` (e.g. in `OrderedCollection new`) | Senders of… | Workspace senders rank first **with receiver-hint badges**: `(top level) · receiver: Stage`, `Stage class » new · receiver: super`, `Stage » init · receiver: OrderedCollection`. The hint **ranks** likely responders; nothing is filtered (AC6). |

## Part C — Plural go-to-definition + call hierarchy (AC3/AC4)

| Row | Do | Expect |
|---|---|---|
| **C1** | **F12 / Go to Definition** on the `greet` send in `run:` | A **picker with 2 targets** (`Speaker>>greet`, `Robot>>greet`) — never silently one. |
| **C2** | **Shift+F12 / Find All References** on `greet` | The peek lists the **union**: both definitions + both send sites. |
| **C3** | Right-click `greet` (its def in `Speaker`) → **Show Call Hierarchy**, expand **incoming** | `Stage » run:` and `Stage » announceAll` (the senders). |
| **C4** | On `Stage>>run:`, **Show Call Hierarchy**, switch to **outgoing** | `greet` (the send inside the method). |

## Part D — Cartridge virtual document (AC2 honest framing)

| Row | Do | Expect |
|---|---|---|
| **D1** | In an **Implementors of #do:** result, click a cartridge row (e.g. `Dictionary`) | A **read-only** editor opens (`smalltalk-cartridge:` scheme) stating the fact: *"…cartridge (facts-only). The method source body is not bundled offline…"* — **no error**, no fake source. |
| **D2** | Read the **header row tooltip** of any result | The union/uncertainty disclaimer — *lexical union; dynamic dispatch can't be narrowed to one runtime target; likely first, none hidden*. This is the contract (AC2). |

## Part E — robustness, no-gst, shipped artifact (AC7)

| Row | Do | Expect |
|---|---|---|
| **E1** | Confirm the status bar reads `reference (gst 3.2.5)` (or `installed …`) and re-run **A2/B2** | Cartridge union works **with no `gst`** (AC7). |
| **E2** | Mid-edit: delete the `]` closing `run:` so a squiggle appears, then run **Implementors of #greet** higher up | Still returns the union — the engine degrades, never throws (no error toast). |
| **E3** | `npm run package` → install the VSIX in a clean VS Code → open this folder → repeat **A1, A3, B3, D1** | The shipped build matches the dev build. |

---

Record results in `../verification.md` §4, fill the date/sign-off line, then tick §5.
Anything that doesn't match — a hidden responder, a collapsed go-to-def, a cartridge row that
errors instead of opening the virtual doc — is a bug. Note it and stop.
