# ADR-0004: Selector-surface division of labour — snippets, completion, signature help

* **Status:** Accepted
* **Date:** 2026-06-27
* **Deciders:** Leonardo Nascimento
* **Relates to:** US-427 (#102); the selector surfaces shipped in US-202 (snippets), US-413
  (completion), and US-425 (signature help). Realizes Constitution I (*Native Look & Feel*) and
  II (*Zero Configuration*).

## Context

Three surfaces offer Smalltalk **selectors** to the developer, and they grew independently:

| Surface | Source | What it is |
|---|---|---|
| **Static snippets** | `snippets/snippets.json` | A small curated set of tab-completable templates, triggered by a mnemonic **prefix**. |
| **Dynamic completion** | `server/src/providers/completion.ts` | The full, fact-sourced selector catalogue (workspace ∪ active cartridge), offered in context. |
| **Signature help** | `server/src/providers/signatureHelp.ts` | The active parameter of the keyword send under the cursor. |

US-425 QA surfaced confusion about how they relate: typing a keyword part can pop **completion
*and* signature help** at once, and a selector signature help knows about isn't always offered by
completion *at that exact cursor* — completion offers keyword selectors up front in **selector**
context, then switches to **head** context (variables → classes) once you're inside the argument.
Without a stated contract these read like bugs. They aren't — but the three surfaces had never been
audited together, and the static snippet set had drifted: it was missing high-value
**block-bearing** idioms (`on:do:`, `whileTrue:`, `ifNil:ifNotNil:`, `at:ifAbsent:`, `inject:into:`,
…) a developer reaches for daily.

## Decision

Each surface has **one job**, and they do not overlap:

1. **Static snippets = idiomatic block-bearing templates.** The snippet surface exists to scaffold
   the `[ :each | … ]` **block shape** with tab-stops, inserted by a short mnemonic prefix *before*
   you type a receiver (`wt`→`whileTrue: [ … ]`, `ond`→`on: Error do: [ :ex | … ]`). It is curated
   and deliberately **small** — it is **not** the full selector catalogue, and it only templates
   selectors that actually exist in the active dialect (enforced for GST by the Cartridge #01
   cross-check in `src/test/snippets-verification.js`). Non-block message selectors (`at:put:`,
   plain unary sends) are **out of scope** here — completion covers them.

2. **Dynamic completion = the full selector catalogue, in context.** Completion is the **breadth**
   surface: the merged workspace ∪ active-kernel selector set, ranked by provenance, offered in
   **selector** context (after a receiver / cascade `;`). Keyword selectors still insert as
   one-tab-stop-per-keyword snippets, but completion is fact-sourced and never hand-curated. In
   **head** context (expression start / after a keyword part) it offers in-scope variables then
   classes — *not* selectors, because no receiver precedes the cursor.

3. **Signature help = where-you-are, mid-arguments.** Signature help never offers a catalogue. It
   reconstructs the keyword send under the cursor and highlights the **active parameter** (the
   keyword part you're filling), as an honest prefix union. It disambiguates the message you are
   *already* typing.

**Explicitly intended, not defects:**

* The **completion ↔ signature-help double-popup** is standard VS Code behaviour (TypeScript does
  the same). We do **not** suppress either; they answer different questions ("which selector?" vs
  "which argument?").
* The completion **selector→head context switch** is correct: once the cursor is in an argument
  position there is no receiver to send a message to, so offering selectors there would be wrong.

## Consequences

* **For users:** a coherent story — reach for a snippet prefix to scaffold a block idiom; lean on
  completion for the full catalogue; read signature help to see which argument you're on. No surface
  contradicts another.
* **For maintainers:** the snippet set has a **bright line** — add a snippet only when it scaffolds a
  block idiom (and only for a real dialect selector). The guard
  (`src/test/snippets-verification.js`, run by `npm run eval`) enforces unique prefixes, the
  cartridge cross-check, and a prefix snapshot. Breadth belongs in the cartridge/completion path, not
  the curated snippet file.
* **Dialect-neutral:** the contract is independent of GST; a future dialect cartridge swaps the
  cross-check oracle and inherits the same three-way division.
* **Revisit if** we add argument-name-aware signature help (needs a cartridge that carries argument
  names — facts-only #01 does not) or a "selectors of receiver type" completion mode; either would
  refine surface 2/3 but not the snippet contract.
