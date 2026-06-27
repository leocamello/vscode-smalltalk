# Specification: Selector-surface coverage audit

**ID**: US-427
**Feature**: Selector-surface coverage audit (snippets ∪ completion ∪ signature help)
**Status**: Draft
**Owner**: Leonardo Nascimento
**Created**: 2026-06-27
**Epic**: EPIC-005 · **Issue**: #102 · **Estimate**: S

## 1. Overview

Three surfaces offer Smalltalk **selectors** to the developer: the **static snippets**
(`snippets/snippets.json`), **dynamic completion** (`providers/completion.ts`), and **signature
help** (`providers/signatureHelp.ts`). They grew independently and have never been audited
together. US-425 QA surfaced the confusion: as you type a keyword part, completion *and* signature
help can both pop, and a selector signature help knows isn't always offered by completion at that
cursor. This story **inventories** the three surfaces, pins their **division of labour** in an ADR
so each has one clear job, and **fills the gaps** in the static snippet set — the high-value
block-bearing idioms a developer reaches for that aren't yet templated. Doc + additive snippets
only: no provider behaviour changes.

## 2. Goals

- Inventory the three selector surfaces and benchmark the static set against the common GST idiom
  list (kernel corpus + Cartridge #01).
- Pin a **division-of-labour contract** as **[ADR-0004](../../docs/decisions/0004-selector-surface-division.md)**:
  static snippets = idiomatic **block-bearing** templates with tab-stops; dynamic completion = the
  full selector catalogue (context-switched); signature help = mid-message active parameter. No
  surface duplicates or contradicts another.
- Add the missing block idioms to `snippets.json`, each a real GST selector, with tab-stops and a
  collision-free prefix.
- Add an **eval guard** (`src/test/snippets-verification.js`, wired into `npm run eval`) that keeps
  the set honest: unique prefixes, every keyword-selector snippet resolves in Cartridge #01, and a
  prefix snapshot catches regressions to existing prefixes.

## 3. Non-Goals

- **No provider rewrite.** Completion and signature-help behaviour are unchanged (AC4).
- **Not "fixing" the completion-vs-signature-help double-popup** — it is standard VS Code
  behaviour (TypeScript does the same); the audit just makes the division of labour intentional.
- **Not changing the selector→head context switch** in completion; the inventory documents it as
  intended behaviour, not a defect.
- No new completion/signature-help eval cases (those surfaces don't change). The snippet guard is
  the only new test.

## 4. User Stories & Acceptance Criteria

**US-427**: As a Smalltalk developer, I want the three surfaces that offer selectors — static
snippets, dynamic completion, and signature help — to tell one coherent story, so that I'm not
confused by a selector appearing in one surface but missing from another, and the idiomatic
block-bearing selectors I reach for are actually templated.

- **AC1 — Inventory.** §5.1 inventories the three surfaces (snippets: prefixes, collisions,
  discoverability; completion: keyword-selector behaviour by cursor context — the selector→head
  switch; signature help: prefix union) benchmarked against the common GST idiom list.
- **AC2 — Division-of-labour doc.** ADR-0004 pins the contract (snippets = block-bearing templates;
  completion = full catalogue; signature help = mid-message active parameter); no surface
  duplicates or contradicts another.
- **AC3 — Fill the gaps.** The missing block idioms are added to `snippets.json` with tab-stops and
  collision-free prefixes; an eval guard (unique prefixes + cartridge cross-check + prefix snapshot)
  protects the set; no regressions to existing prefixes.
- **AC4 — No behaviour change.** Works with no `gst`; completion and signature-help behaviour are
  unchanged (doc-only + additive snippets); all three test layers + `npm run eval` stay green.

## 5. Technical Design

### 5.1 Inventory (AC1)

| Surface | File | Job today | Selector behaviour |
|---|---|---|---|
| **Static snippets** | `snippets/snippets.json` | Tab-completable templates | 21 curated entries; message-fragment snippets (`ift`→`ifTrue: [ … ]`) + structural ones (`sub`, `met`, …). Prefix-triggered (`if`, `do`, `col`, `to`…). |
| **Dynamic completion** | `server/src/providers/completion.ts` | Full selector catalogue | In **selector** context (after a receiver / cascade `;`) offers the merged workspace ∪ kernel selector set (keyword selectors insert as `at:${1} put:${2}` snippets); switches to **head** context (in-scope vars → classes) at expression start / after a keyword part. |
| **Signature help** | `server/src/providers/signatureHelp.ts` | Mid-message active parameter | Keyword sends only: a backward token scan reconstructs the keyword prefix typed so far, matched as an honest prefix union against workspace ∪ kernel; highlights the active keyword part. Unary/binary/head cursors → null. |

**Benchmark.** The static set covers the most common control-flow/iteration idioms (`ifTrue:`,
`do:`, `collect:`, `to:do:`, …) but is **missing** high-value **block-bearing** idioms that recur
in the kernel corpus and `docs/research/gst-syntax/`: exception handling (`on:do:`, `ensure:`,
`ifCurtailed:`), looping (`whileTrue:`, `whileFalse:`), nil-guards (`ifNil:ifNotNil:`,
`ifNotNil:ifNil:`), dictionary access (`at:ifAbsent:`, `at:ifAbsentPut:`, `keysAndValuesDo:`,
`keysDo:`), and folding/indexing (`inject:into:`, `doWithIndex:`, `detect:ifNone:`). These are the
gaps AC3 fills.

**Two backlog names corrected against Cartridge #01** (the value of the cross-check guard):
`valuesDo:` is **not** a GST kernel selector (0 hits) — dropped; `withIndexDo:` is **not** the GST
name — the real selector is **`doWithIndex:`** — templated under the correct name.

### 5.2 Division of labour (AC2)

ADR-0004 records the contract (one job per surface, no overlap):

- **Static snippets** — idiomatic **block-bearing** templates with tab-stops you *insert by mnemonic
  prefix before typing a receiver*. They are the only surface that scaffolds the `[ :each | … ]`
  block shape. They deliberately do **not** try to be the full selector catalogue.
- **Dynamic completion** — the **full, fact-sourced** selector catalogue offered *in context* after
  a receiver. Keyword selectors still insert as one-tab-stop-per-keyword snippets, but completion is
  the breadth surface, never curated.
- **Signature help** — **where-you-are** mid-arguments: the active parameter of the keyword send
  under the cursor. It never offers a catalogue; it disambiguates the message you're already typing.

The completion↔signature-help double-popup and the selector→head context switch are documented as
**intended**, not defects.

### 5.3 Snippet additions (AC3)

14 message-fragment snippets, each verified present in Cartridge #01, collision-free prefixes
(checked against the existing 21):

| Selector | Prefix | Body |
|---|---|---|
| `whileTrue:` | `wt` | `whileTrue: [ $1 ]` |
| `whileFalse:` | `wf` | `whileFalse: [ $1 ]` |
| `on:do:` | `ond` | `on: ${1:Error} do: [ :${2:ex} | $0 ]` |
| `ensure:` | `ens` | `ensure: [ $0 ]` |
| `ifCurtailed:` | `ifc` | `ifCurtailed: [ $0 ]` |
| `ifNil:ifNotNil:` | `ifninn` | `ifNil: [ $1 ] ifNotNil: [ :${2:arg} | $3 ]` |
| `ifNotNil:ifNil:` | `ifnnin` | `ifNotNil: [ :${1:arg} | $2 ] ifNil: [ $3 ]` |
| `at:ifAbsent:` | `atia` | `at: ${1:key} ifAbsent: [ $0 ]` |
| `at:ifAbsentPut:` | `atiap` | `at: ${1:key} ifAbsentPut: [ $0 ]` |
| `keysAndValuesDo:` | `kvd` | `keysAndValuesDo: [ :${1:key} :${2:value} | $0 ]` |
| `keysDo:` | `kd` | `keysDo: [ :${1:key} | $0 ]` |
| `inject:into:` | `inj` | `inject: ${1:0} into: [ :${2:acc} :${3:each} | $0 ]` |
| `doWithIndex:` | `dwi` | `doWithIndex: [ :${1:each} :${2:index} | $0 ]` |
| `detect:ifNone:` | `detin` | `detect: [ :${1:each} | $2 ] ifNone: [ $0 ]` |

Each carries `description` + the existing `scope` (`source.smalltalk.gnu, source.smalltalk`).

### 5.4 Eval guard (AC3)

`src/test/snippets-verification.js` — plain Node, no deps, mirrors `grammar-verification.js`
(snapshot via `--update`). Wired into `npm run eval` (and a `test:snippets` script). Asserts:

1. `snippets.json` is valid JSON.
2. **Unique prefixes** across all snippets (collision guard).
3. **Cartridge cross-check** — every snippet whose **key contains `:`** (a keyword selector)
   resolves in `server/data/cartridges/gst-3.2.5-cartridge.json` (union of all classes'
   `instanceMethods` + `classMethods` selectors). This catches typo/non-existent selectors like
   `valuesDo:`/`withIndexDo:`.
4. **Prefix snapshot** — `src/test/snapshots/snippets.prefixes.snap` (sorted `prefix → key`) so any
   change to an existing prefix is a visible, reviewed diff (regression guard).

## 6. Risks & Limitations

- **Prefix collisions / muscle memory.** New prefixes are checked collision-free against the
  existing 21 and snapshotted; the snapshot makes any future collision a failing diff.
- **Cartridge name drift.** The cross-check binds snippet keys to GST 3.2.5 selectors. A future
  cartridge bump that renames a selector would fail the guard — intended (forces the snippet to
  track reality). Control-flow selectors (`whileTrue:`, `ifNil:ifNotNil:`…) are all present in #01.
- **Unary/non-selector keys** (`assert`, `return`, `Block`, structural snippets) are not selectors
  and are excluded from the cross-check by the `:`-in-key rule.
- Snippets are a **declarative** VS Code contribution — not exercised by the LSP test layers; the
  Node guard + the manual-QA tab-trigger spot check are the coverage.
