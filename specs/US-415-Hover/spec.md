# Specification: Hover

**ID**: US-415
**Feature**: Hover
**Status**: Draft
**Owner**: Leonardo Nascimento
**Created**: 2026-06-24
**Branch**: feature/US-415-hover

## 1. Overview
`textDocument/hover` surfaces, at the cursor, the facts the language server already
knows from the US-411 front end and the US-412/US-413 indexes — so a developer can
understand a selector, class, variable, or literal **without navigating away**. It is
offline (no `gst` at runtime), Markdown-rendered, and reuses the workspace index +
the active kernel cartridge. Prose (method/class comments) is **gated on provenance**
so the LGPL facts-only invariant on the *shipped* cartridge is never violated.

## 2. Goals
- Hover on a **selector** → its signature + implementor list (+ a method comment when
  provenance allows it).
- Hover on a **class** → its superclass chain + class comment (when available).
- Hover on a **variable** → its kind + declaration site.
- Hover on a **numeric literal** → its decimal value, decoding radix and scaled-decimal forms.
- All hover content is **Markdown with code fences** (AC5), built with standard LSP types.
- Reuse existing data sources (workspace index, `KernelIndexService`); never throw.

## 3. Non-Goals
- No runtime/`gst` evaluation of expressions (that is EPIC-007 / US-705).
- No type inference — Smalltalk is dynamically typed; a selector lists *all* implementors,
  not "the" one.
- No prose in any **distributed or committed** artifact: the bundled/frozen reference
  cartridge stays facts-only; the `kernelIndex` facts-only test keeps enforcing it.
- No new hover for binary-operator semantics, keyword docs, or pragmas beyond what the
  indexes carry.

## 4. User Stories & Acceptance Criteria
**US-415**: As a **Smalltalk developer**, I want **hover information for selectors,
classes, variables, and literals**, so that **I can understand code without navigating away.**

- **AC1**: Hover on a selector shows its **signature** and **implementor list** (and the
  method comment **when provenance allows** — installed kernel or workspace, never the
  frozen reference).
- **AC2**: Hover on a class shows its **superclass chain** and **class comment** (comment
  when available from the workspace source or an installed-kernel cartridge).
- **AC3**: Hover on a variable shows its **kind** (temporary / parameter / instance var /
  class var / class) and its **declaration site** (line:col).
- **AC4**: Hover on a numeric literal renders its **radix / scaled-decimal value**
  (e.g. `16rFF` → `255`, `3.14s2` → `3.14s2` = value with scale 2).
- **AC5**: Hover content is **Markdown with code fences**.

## 4a. Resolved clarifications (Clarify gate)
The backlog (user-stories.md:862) deferred "ship kernel method-comment text?" to this story.
Resolution — **provenance gates prose**:

| Source (provenance) | Facts (sig / chain / kind / value) | Comment prose |
|---|---|---|
| **Bundled / frozen reference** cartridge | yes | **no** — facts-only, enforced |
| **Installed** cartridge (user's real GST, generated locally) | yes | **yes** |
| **Workspace** (user's own source) | yes | **yes** (best-effort) |

Rationale: LGPL governs *distribution*, not a tool reading the user's own local files. The
shipped VSIX must not redistribute kernel comment prose (bundled stays facts-only); reading
comments from files already on the user's disk and showing them back to the same user is
non-distributive, like any IDE showing installed-library docstrings. The cartridge
`carriesProse` flag + the `Provenance` enum are the gate.

## 5. Technical Design
**New provider** `server/src/providers/hover.ts` — a pure `hoverAt(...)` returning an LSP
`Hover | null` (Markdown `MarkupContent`). Inputs (handed in already-parsed, mirroring the
other providers): `offset`, `text`, `tokens`, `ast`, `symbols` (current doc) + resolved
lookups for selectors/classes (workspace index entries + a kernel query facade). Never throws;
returns `null` when nothing is under the cursor.

**Cursor resolution** (extends the `definition.ts` AST walk): pick the deepest node containing
`offset` and classify —
1. `Literal` (integer / float / scaledDecimal) → **AC4**.
2. `Variable` naming an in-scope temp/param/inst-var/class-var → **AC3**; else a known class
   name → **AC2**.
3. message-selector region (`offset >= receiver.end`) → **AC1**.

**Data sources**
- *Selectors (AC1)*: signature built from selector + arity (`unary` → `sel`; `binary` →
  `sel arg`; keyword → `at: a put: b` from keyword parts). Implementors = workspace `Method`
  index entries (`containerName` = class) ∪ kernel classes implementing it. Comment from the
  workspace method (best-effort, the leading `"…"` comment token in the method body) or the
  installed-kernel signature `documentation`.
- *Classes (AC2)*: superclass chain walked transitively over a combined immediate-superclass
  map = kernel cartridge classes ∪ workspace class symbols (`SymbolNode.detail` already holds
  the immediate superclass; surfaced via a new `superclass` field on `IndexEntry`). Class
  comment from the workspace `comment:` send / class body (best-effort) or installed-kernel
  `ClassFact.documentation`.
- *Variables (AC3)*: reuse the completion provider's `scopeVariablesAt` shape to find the
  variable under the cursor, its kind, and the declaration `NameRef` / symbol `selectionRange`
  for the line:col.
- *Literals (AC4)*: decode `LiteralNode.value` (raw text). Radix integer `<base>r<digits>` →
  decimal; scaled decimal `<mantissa>s<scale>` → value + scale; plain integer/float → value.

**Kernel query facade** — add to `KernelIndexService`: `superclassOf(name)`,
`implementorsOf(selector)`, `arityOf(selector)`, an `activeProvenance` getter, and (slice B)
`classComment(name)` / `selectorComment(class, selector)` returning prose only when the active
source `carriesProse`.

**Installed-cartridge prose (slice B)** — extend `indexKernelDirectoryToCartridge` to capture
the leading method comment and a class `comment:` into the cartridge `documentation` fields and
set `header.carriesProse = true` for the installed adapter; `cartridgeLoader` carries it through
to the model the service queries. Bundled cartridge unchanged (facts-only, test-enforced).

**Wiring** — `server.ts`: advertise `hoverProvider: true`; `connection.onHover` assembles the
inputs (like `onCompletion`) and calls `hoverAt`. Client needs no change (hover is built-in).

## 5a. Slices
- **Slice A — Hover provider (facts + workspace prose + all 4 kinds + Markdown):** AC1
  (sig+implementors), AC2 (chain + workspace class comment), AC3, AC4, AC5. Independently shippable.
- **Slice B — Installed-cartridge prose:** capture method/class comments in the installed
  adapter; surface installed-kernel comments; keep bundled facts-only (enforced).

## 6. Testing strategy
- **Unit** (`server/test/hover.test.ts`, `test:parser`): `hoverAt` content per symbol kind —
  selector signature/implementors, class chain, variable kind+decl, literal decode, Markdown
  fences. Pin user-observable strings (US-414 lesson: assert on content).
- **Handshake** (`test:server`): `textDocument/hover` advertised + returns Markdown over the
  bundled server.
- **e2e** (`client/test-e2e/US-415.acceptance.test.js`, `test:e2e`): `vscode.executeHoverProvider`
  asserts the rendered Markdown for each user-observable AC (RED first).
- **Eval** (`evals/datasets/hover/`, `npm run eval`): golden hover output cases mirroring
  `completion/`.
- **Invariant**: bundled cartridge stays facts-only (existing `kernelIndex` test); a new test
  asserts the installed adapter *may* carry prose.

## 7. Risks & Limitations
- Best-effort workspace comment extraction: if a method/class has no conventional comment, hover
  shows facts only (acceptable; chain/signature is the headline).
- Installed prose depends on a discovered GST install; CI stays hermetic (inject the probe list —
  do not depend on the dev box's real `/usr/local` kernel).
- Combined superclass map can have cycles/missing links in malformed input → walk with a visited
  set and a depth cap; never throw.
