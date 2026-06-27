# US-427 — Manual QA workspace (Selector-surface coverage audit)

A ready-to-open workspace for the `verification.md` §4 matrix. US-427 is **doc + additive
snippets** — the feature to validate is the **static snippet surface** (the block-bearing-templates
half of the [ADR-0004](../../../docs/decisions/0004-selector-surface-division.md) division of
labour). Everything here is **offline, no `gst` needed**.

`Playground.st` is a `SnippetDemo` class whose methods each leave a receiver in place so the new
**message-fragment** snippets attach naturally. The `>>> <prefix>` markers tell you where to type.

> The new snippets attach to a receiver; on their own they are not valid top-level Smalltalk. You're
> validating the **expansion text + tab-stop order**, not that `Playground.st` parses clean as-is.

## Open it
1. In the Extension Development Host (`F5` from the repo): **File → Open Folder…** → this
   `manual-qa-workspace/` folder. The extension auto-activates (the folder has `.st` files).
2. No settings needed. The snippets are a declarative contribution (`contributes.snippets`), always
   on for `.st`/`.gst`.
3. To insert a snippet: type its **prefix**, then in the suggestion widget pick the entry whose
   detail matches the `description` (e.g. *"whileTrue: loop"*) and press **Tab** or **Enter**. Press
   **Tab** to jump between the `$1 $2 …` placeholders; the final `$0` is where the cursor lands last.
   If the widget doesn't pop, force it with **Ctrl+Space**.

---

## Part A — The 14 new block snippets expand correctly (AC3)

For each row: delete the `>>>` marker comment, type the prefix on that receiver, accept the snippet,
and Tab through. Confirm the **expansion** and **tab-stop order** match.

| Row | Method | Prefix | Expect expansion (tab stops `$1→$2→$3→$0`) |
|---|---|---|---|
| **A1** | `loopWhile` | `wt` | `whileTrue: [ ▮ ]` |
| **A2** | `loopWhile` | `wf` | `whileFalse: [ ▮ ]` |
| **A3** | `guarded` | `ond` | `on: ${Error} do: [ :${ex} | ▮ ]` — Tab: Error → ex → body |
| **A4** | `guarded` | `ens` | `ensure: [ ▮ ]` |
| **A5** | `guarded` | `ifc` | `ifCurtailed: [ ▮ ]` |
| **A6** | `nilGuards:` | `ifninn` | `ifNil: [ ▮ ] ifNotNil: [ :arg | ]` — Tab: nil-body → arg → notNil-body |
| **A7** | `nilGuards:` | `ifnnin` | `ifNotNil: [ :arg | ▮ ] ifNil: [ ]` — Tab: arg → notNil-body → nil-body |
| **A8** | `dictionaryAccess:` | `atia` | `at: ${key} ifAbsent: [ ▮ ]` — Tab: key → body |
| **A9** | `dictionaryAccess:` | `atiap` | `at: ${key} ifAbsentPut: [ ▮ ]` |
| **A10** | `dictionaryAccess:` | `kvd` | `keysAndValuesDo: [ :key :value | ▮ ]` |
| **A11** | `dictionaryAccess:` | `kd` | `keysDo: [ :key | ▮ ]` |
| **A12** | `folding` | `inj` | `inject: ${0} into: [ :acc :each | ▮ ]` — Tab: seed → acc → each → body |
| **A13** | `folding` | `dwi` | `doWithIndex: [ :each :index | ▮ ]` |
| **A14** | `folding` | `detin` | `detect: [ :each | ] ifNone: [ ▮ ]` — Tab: each → detect-body → none-body |

✅ Pass = all 14 expand with the right text and the placeholders are visited in the listed order.

## Part B — No collision with existing prefixes (AC3 "no regressions")

The nil prefixes are the danger zone (`ifn`, `ifnn` existing vs `ifninn`, `ifnnin` new). In
`regressionCheck` (and `nilGuards:`):

| Row | Type | Expect |
|---|---|---|
| **B1** | `ift` | `ifTrue: [ ▮ ]` (unchanged) |
| **B2** | `ifn` | `ifNil: [ ▮ ]` (unchanged — **not** the two-arm) |
| **B3** | `ifnn` | `ifNotNil: [ :arg | ▮ ]` (unchanged) |
| **B4** | `do` | `do: [ :each | ▮ ]` (unchanged) |
| **B5** | `to` | `to: count do: [ :i | ▮ ]` (unchanged) |
| **B6** | type `ifn` slowly → keep typing `ifninn` | the widget narrows from `ifNil:` to `ifNil:ifNotNil:` — both reachable, no shadowing |

✅ Pass = every existing prefix expands exactly as before; new prefixes never hijack an old one.

## Part C — Division of labour is intentional, not buggy (AC2)

This is the part US-425 QA flagged. With the cursor **after a receiver** (selector context):

| Row | Do | Expect |
|---|---|---|
| **C1** | In `dictionaryAccess:`, type `bins at` (no prefix accept) | You see **both** the `atia`/`atiap` **snippets** *and* completion's `at:`/`at:put:`/`at:ifAbsent:` **catalogue** selectors. This co-existence is **intended** (snippets = curated block templates; completion = full catalogue). |
| **C2** | Accept the `atia` **snippet** | Inserts the templated block form with tab-stops. |
| **C3** | Instead accept the completion `at:put:` **selector** | Inserts `at:${1} put:${2}` (completion's one-stop-per-keyword form). Different surface, different job — neither is wrong. |

✅ Pass = both surfaces appear and each does its stated job; nothing looks like a duplicate bug.

## Part D — Shipped artifact parity (AC4)

| Row | Do | Expect |
|---|---|---|
| **D1** | `npm run package` → install the VSIX in a clean VS Code → open this folder → repeat **A1, A6, A12, B2** | The shipped build offers the same snippets as the dev build. |
| **D2** | Open the Developer Tools console during A1–A14 | No errors/warnings from the extension. |

---

Record results in `../verification.md` §4, fill the date/sign-off line, then tick §5.
Anything that doesn't match — a wrong expansion, placeholders visited out of order, an existing
prefix that changed behaviour, or a new prefix shadowing an old one — is a bug. Note it and stop.
