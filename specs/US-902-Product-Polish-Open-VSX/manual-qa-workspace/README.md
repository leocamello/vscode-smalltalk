# US-902 — The Ultimate Manual QA Suite (v1.0.0)

One workspace that exercises **every feature the extension ships at 1.0** — declarative editing, the full
LSP language-intelligence surface (all offline, no `gst`), Run Current File, and the 1.0 polish. Use it as
the release manual-QA matrix **and** as the staging ground for the demo GIFs (`docs/media-shotlist.md`).

> **Invariant:** every `.st` file here parses with **0 diagnostics** (verified against the real parser). If
> you see a red squiggle on load, that's a bug — except in **`Diagnostics-Playground.st`**, where you
> deliberately introduce errors and then undo.

## Open it
In the Extension Development Host (`F5` from the repo): **File → Open Folder…** → this
`manual-qa-workspace/`. The extension auto-activates (the folder has `.st` files) and indexes every file so
cross-file features (workspace symbols, references, multi-file rename) resolve. For the trust checks, use
**Workspaces: Manage Workspace Trust**.

## The files (a small "Bank" domain + playgrounds)
| File | Role |
| --- | --- |
| `Account.st` | Base class — ivars (`balance`/`owner`/`history`), methods, class-side `for:`, comments, kernel use, cascade. |
| `Account-Reporting.st` | `Account extend` in a 2nd file (same ivars) — ivar rename spans both; `extend` outline. |
| `SavingsAccount.st` | `Account subclass:` — inheritance/hover chain, `super`, inherited sends. |
| `Transaction.st` | Small value class, referenced by `Account`. |
| `Bank.st` | Aggregate — collections, keyword sends, cascades, cross-file refs, namespaced `#{Account}`/`Smalltalk.Account`. |
| `Shadowing.st` | Scope-safety: an arg shadowing an ivar; a temp shadowing a class. |
| `Literals.st` | Numeric/char/symbol/array literals for hover-decode + highlighting. |
| `Diagnostics-Playground.st` | Clean; you introduce errors to test squiggles + quick fixes. |
| `Formatting-Playground.st` | Loosely-spaced (valid) input for the formatter. |
| `Legacy-Chunk.st` | The classic chunk/bang (`!`) file format — proves both container formats parse. |
| `Demo.st` | Self-contained runnable gst script for Run Current File + the trust gate. |

---

## A. Declarative editing (no LSP)
| # | Feature | Do this | Expect |
| --- | --- | --- | --- |
| A1 | **Syntax highlighting** | Open every file. | Keywords, selectors, strings, symbols, comments, and numbers are colored; both bracket- and chunk-format files highlight. |
| A2 | **Radix-literal coloring** | `Literals.st` → the `radixThenPeriod` method, look at `x := 16rFF.`. | `16rFF` colors as **one** number and the trailing `.` colors **separately** (a statement terminator) — the period is not swallowed into the literal. |
| A3 | **Snippets** | In a new line inside a method, type `ifTrue` / `whileTrue` / `on:do` / `keysAndValuesDo` and accept the snippet. | Idiomatic block-bearing templates expand with tab-stops you can `Tab` through. |
| A4 | **Bracket match / autoclose** | Type `[`, `(`, `{`. Put the caret on a `]`. | Auto-closes; the matching bracket is highlighted. |
| A5 | **Comment toggling** | Select lines, `Ctrl/Cmd+/`. | Wraps/unwraps the selection as a `"…"` comment. |
| A6 | **Folding** | Hover the gutter on a class/method body in `Account.st`. | Fold arrows collapse the class body, method bodies, and multi-line comments. |

## B. Navigation & structure (LSP, no `gst`)
| # | Feature | Do this | Expect |
| --- | --- | --- | --- |
| B1 | **Outline (bracket format)** | Open the Outline view on `Account.st`. | `Account` → its methods + ivars; `Account class` → `for:`. |
| B2 | **Outline (chunk format)** | Outline on `Legacy-Chunk.st`. | `Ledger` with `entries` and `add:` — the chunk/bang format parses too. |
| B3 | **Breadcrumbs** | Click the breadcrumb bar in `Bank.st`; navigate class → method. | Breadcrumbs list the class and its methods; clicking jumps. |
| B4 | **Workspace symbols** | `Ctrl/Cmd+T`, type `Account`, then `deposit:`. | Finds classes and method selectors across all files; jumps on select. Works even before opening a file. |
| B5 | **Go to Definition (class)** | `F12` on `Account` in `SavingsAccount.st`'s `Account subclass:`. | Resolves to the class's definition sites: the `Object subclass: Account` declaration in `Account.st` **plus** each `Account extend` (in `Account-Reporting.st` and `Shadowing.st`). Multiple results open a **peek list**, primary file first — the extension shows *all* the places that define/extend the class (same "show all" philosophy as plural go-to-def on selectors). |
| B6 | **Go to Definition (plural)** | `F12` on the `printOn:` send / selector. | Returns **all** implementors (Account, SavingsAccount, Transaction) — never collapses to one. |
| B7 | **Highlight occurrences (scope-aware)** | Caret on `balance` in `Shadowing.st`'s `scaleBy:`. | Only the **local** `balance` uses in that method highlight — not the ivar elsewhere. |

## C. Code intelligence (LSP, no `gst`)
| # | Feature | Do this | Expect |
| --- | --- | --- | --- |
| C1 | **Completion — kernel class** | In `Bank.st`, new line, type `Ordered`. | `OrderedCollection` (and other kernel classes) offered; status bar shows the kernel source. |
| C2 | **Completion — selector** | Type `accounts ` (receiver + space). | Kernel + workspace selectors offered (`do:`, `inject:into:`, `detect:ifNone:`, `add:`…). |
| C3 | **Completion — keyword snippet** | Accept `at:put:`. | Inserts the text `at: put:` **as a snippet**: the cursor sits at the first argument slot (after `at:`), and pressing **Tab** jumps to the next slot (after `put:`). The `⟨1⟩`/`⟨2⟩` are just how tab-stops are drawn — you won't see literal placeholder text; you'll see the blinking caret hop between the argument positions. That's the snippet working. |
| C4 | **Completion — in-scope vars** | Inside `totalBalance`, type `su`. | The block temp `sum` / args are offered. |
| C5 | **Hover — selector** | Hover `inject:into:` in `Bank.st`. | Signature + implementor(s), Markdown with a code fence. |
| C6 | **Hover — class + chain** | Hover `SavingsAccount` (its declaration). | Superclass chain `SavingsAccount → Account → Object`. |
| C7 | **Hover — variable** | Hover `balance` in `Account.st`. | Kind (instance variable) + declaration site. |
| C8 | **Hover — literal** | Hover `16rFF`, `2r1010`, `1.50s2` in `Literals.st`. | Decoded value (`255`, `10`, a ScaledDecimal). |
| C9 | **Hover — comment provenance** | Hover `deposit:` (has a `"…"` comment) vs. a kernel selector. | Your workspace method shows its comment prose; the bundled kernel reference stays facts-only. |
| C10 | **Diagnostics + insert-closer fix** | `Diagnostics-Playground.st`: delete the `]` ending `broken`. | A `smalltalk(parse)` squiggle on the defect line; lightbulb → **Insert missing ]** restores it and clears. Undo. |
| C11 | **Diagnostics + close-string fix** | `Diagnostics-Playground.st`: in `aString`, delete the **closing** quote of the string (the `'` just before the line break — the string is on its own line). | Unterminated-string diagnostic on that line; the **Insert missing `"'"`** quick fix appends `'` at the end of the string's line → `…quick fix'`, and the file re-parses clean. Undo. *(The string is on its own line so the fix has a clean place to close. If code follows the opener on the same line — e.g. a `]` — the string swallows it and the fix closes at end-of-line; that's inherent, not a bug.)* |
| C12 | **Semantic tokens** | **Enable it first:** open Settings (`Ctrl/Cmd+,`), search `editor.semanticHighlighting.enabled`, set it to **`true`** (it defaults to `configuredByTheme`, so some themes leave it off). Then open `Shadowing.st`. | The ivar-shadowing arg `balance` colors as a **parameter**; the temp `Account` in `demo` colors as a **variable**, not a class; kernel classes color as classes. *(To see the effect clearly, use a theme with semantic colors, e.g. Default Dark Modern.)* |
| C13 | **Signature help** | In `Bank.st`, click between `at:` and `put:` in `byOwner at: each owner put: each`. | Signature popup with the **active parameter** (the keyword you're in) highlighted. |

## D. Cross-reference — Senders / Implementors (LSP, no `gst`)
| # | Feature | Do this | Expect |
| --- | --- | --- | --- |
| D1 | **Find All References** | `Shift+F12` on `owner`. | The de-duplicated union of send-sites + the definition, each row tagged with its source. |
| D2 | **Implementors of…** | Right-click `printOn:` → **Smalltalk: Implementors of…**. | The **Smalltalk References** panel opens with a union disclaimer + per-row provenance (Account/SavingsAccount/Transaction + kernel). |
| D3 | **Senders of…** | Command Palette → **Smalltalk: Senders of…** on `balance`. | Every send of `balance` across the workspace ∪ kernel, ranked, none hidden. |
| D4 | **Call hierarchy** | `Ctrl/Cmd`-click → **Show Call Hierarchy** on `totalBalance`. | Incoming = senders; outgoing = the sends inside the method body. |

## E. Formatting (LSP, no `gst`) — **always available at 1.0**
| # | Feature | Do this | Expect |
| --- | --- | --- | --- |
| E1 | **Format Document** | `Formatting-Playground.st` → **Format Document** (`Shift+Alt/Opt+F`). | `:=`, binary-op, and keyword spacing normalized; cascade aligned; blank-line runs collapsed; comments/strings untouched. |
| E2 | **Idempotent** | Format Document again. | No further change. |
| E3 | **Format Selection** | Select the three loosely-spaced **top-level statements** at the bottom of `Formatting-Playground.st` (the `\| x y \| … x:=1+2. y:=x*3.` block); **Format Selection**. | Only those lines re-space (`x := 1 + 2.` …); the rest of the file is untouched. *(Format Selection formats the selection as a standalone unit, so a single method line inside a class — e.g. just `sum [ ^a+b ]` — yields **no edits** because a bare `^` isn't valid at top level. Use Format Document for in-class touch-ups. See the note in the file header.)* |
| E4 | **On-type** | Inside a block, press Enter / type `]`. | The line's indent adjusts. |
| E5 | **blockStyle expand** | Set `smalltalk.format.blockStyle` = `expand`; Format Document. | Method / multi-statement block bodies reflow one statement per line (single-statement arg blocks stay inline). |
| E6 | **No enable switch** | Search settings for `format.enable`. | It does **not** exist — formatting is on by default (US-902). |

## F. Rename refactorings (LSP, no `gst`)
| # | Feature | Do this | Expect |
| --- | --- | --- | --- |
| F1 | **Rename temp/arg** | `F2` on the `balance` **argument** in `Shadowing.st`'s `scaleBy:`. | Only that method's local is renamed; the Account ivar is untouched. |
| F2 | **Rename ivar (workspace-wide)** | `F2` on the `owner` **ivar** in `Account.st`. | The ivar's declaration + direct accesses rewrite in `Account.st` **and** `Account-Reporting.st`; a **Refactor Preview** shows the multi-file change. The `owner` accessor selector and its senders (`each owner` in `Bank.st`) are left alone. |
| F3 | **Ivar shadow-safety** | Same rename — check `Shadowing.st`. | The `scaleBy: balance` arg (a shadow) is **not** touched. |
| F4 | **Rename class (workspace-wide)** | `F2` on `Account` in `Account.st`. | Every resolved reference rewrites — declaration, `Account extend`, `Account subclass:`, `Account for:`, `#{Account}`, `Smalltalk.Account` — across all files, via **Refactor Preview**. |
| F5 | **Class shadow / gating** | After F4, check `Shadowing.st`'s `demo`. | The local temp `Account` is **not** renamed (it shadows the class); comments/strings untouched. |
| F6 | **Reject: selector** | `F2` on `deposit:` (the selector). | Rejected with a reason (dynamic dispatch — unsafe offline). |
| F7 | **Reject: kernel class** | `F2` on `OrderedCollection`. | Rejected — kernel/cartridge classes are read-only. |
| F8 | **Reject: collision** | Rename `Account` → `Bank` (existing) or → `OrderedCollection`. | Refused with a collision reason — a rename never merges two classes. |

## G. Run & workflow (uses `gst`)
| # | Feature | Do this | Expect |
| --- | --- | --- | --- |
| G1 | **Run Current File (trusted)** | Trust the workspace; open `Demo.st` → **Smalltalk: Run Current File**. | The Smalltalk terminal prints `Hello, Smalltalk!` and `Sum 1 to 100 = 5050` (needs `gst` installed). |
| G2 | **Run refused (untrusted)** | **Manage Workspace Trust** → *don't trust*; run `Demo.st` again. | A warning offering **Manage Workspace Trust**; `gst` is **not** spawned. |
| G3 | **Static intelligence untrusted** | Still untrusted: hover, outline, complete in `Bank.st`. | All language intelligence still works in Restricted Mode. |

## H. 1.0 product polish (US-902)
| # | Feature | Do this | Expect |
| --- | --- | --- | --- |
| H1 | **Settings order** | *Preferences: Open Settings (UI)* → filter `smalltalk`. | **Gnu Smalltalk Path** is **first**, above Completion / Trace / Format. |
| H2 | **No Preview badge** | Install the packaged `.vsix` (or the Marketplace listing pre-publish). | No **"Preview"** tag on the extension. |
| H3 | **Kernel status bar** | Look at the status bar with a `.st` file open. | Shows the active kernel source (`installed (gst …)` or `reference (gst 3.2.5)`); click opens completion settings. |
| H4 | **Kernel library setting** | Set `smalltalk.completion.kernelLibrary` = `off`, retype a receiver+space; then `bundled`. | `off` suppresses kernel completions (workspace symbols remain); `bundled` restores them. |
| H5 | **VSIX smoke** | `npm run package` → install the `.vsix` in a clean window → open this folder. | Activates, indexes, every provider responds. |

---

### Sign-off
Record any deviation as a bug. When the whole matrix passes on a clean VSIX (and the demo GIFs are
recorded per `docs/media-shotlist.md`), US-902 / the 1.0 release is manually verified.
