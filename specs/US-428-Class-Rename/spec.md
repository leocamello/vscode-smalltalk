# Specification: Class Rename

**ID**: US-428
**Feature**: `textDocument/rename` + `textDocument/prepareRename` — safe, scope-aware **workspace-wide class rename**, **offline, no `gst`**, never touching kernel/cartridge source
**Status**: Draft
**Owner**: Leonardo Nascimento
**Created**: 2026-07-01
**Epic**: EPIC-005 (Console & Cartridges) → ~1.0
**Depends on**: US-426 (rename engine: `prepareRenameAt`/`renameAt`/`withMultiFileConfirmation`, `xref/ivarRefs.ts`), US-412 (workspace symbol index + `containerName` namespaces), US-422 (workspace ∪ cartridge known-class resolution), US-411 (parser/AST + symbol table)

## 1. Overview
Extend the US-426 rename engine to **workspace classes** — the deliberately-deferred sibling of instance-variable
rename (#109, split out of #69). A developer puts the cursor on a class name and every reference to that class
across the workspace is updated: its **declaration** (`Object subclass: #Foo`), every **receiver / superclass**
position (`Foo new`, `Foo subclass: #Bar`), `Foo class` / `Foo extend`, the class-argument **symbol** form, and
the **namespaced / binding** forms `#{Foo}`, `Foo.Bar`, `Foo::Bar`. As with ivar rename, the safety property is
**honesty about resolution**: a rewrite happens only where a reference **resolves** to the target class
(workspace index ∪ cartridge), never on a local variable that merely shares the name and never on a same-named
class in another namespace. The **kernel boundary** is the hard rule — a kernel/cartridge class is read-only and
its rename is rejected, and a new name that collides with an existing kernel **or** workspace class is refused.
Multi-file class renames inherit the US-426 **Refactor Preview** gate; selectors stay out (Live Bridge, EPIC-007).

## 2. Goals
- `textDocument/prepareRename`: accept a **workspace class** reference under the cursor and return the identifier
  range; **reject with a reason** a kernel/cartridge class, a selector, a local/ivar/global that isn't a class,
  or a pseudo-variable/literal.
- `textDocument/rename`: produce a `WorkspaceEdit` renaming **exactly** the resolved class references across the
  workspace, covering **all** of these forms:
  - **Declaration** — `Object subclass: #Foo` (rewrite the `Foo` inside the symbol arg), and the receiver
    position of `Foo extend [ … ]` / `Foo class [ … ]` / `Foo class >> sel`.
  - **Receiver / superclass** `Variable` references — `Foo new`, `Foo subclass: #Bar` (superclass `Foo`),
    `Foo>>#at:`, etc.
  - **Bare class-argument symbols** — the `#Foo` in class-defining message arguments (`subclass: #Foo`,
    `superclass: #Foo`), **not** every `#Foo` symbol literal in arbitrary data position.
  - **Namespaced / binding forms** — `#{Foo}` (`BindingConstant`), and `Foo.Bar` / `Foo::Bar`
    (`Variable` with a qualified name), rewriting **only the class segment**.
- **Resolution-gated** rewriting (namespace decision): a qualified reference (`#{NS::Foo}`, `Smalltalk.Foo`,
  `NS::Foo`) is rewritten only when the **whole path resolves** to the target class via the workspace index
  (name + `containerName` namespace) ∪ the `Smalltalk` default namespace; a same-named class in an unrelated
  namespace is left untouched.
- **Kernel boundary (the key safety rule)**: never rewrite a kernel/cartridge class; reject the rename of a
  class that resolves in the cartridge; **reject a new name** that collides with an existing kernel **or**
  workspace class (reject-with-reason, mirroring US-426 ivar collisions).
- **No false positives**: a local temp/arg (or an ivar) that merely shares the class name is **not** rewritten
  — the walk is scope-aware and only rewrites references that resolve to the *class* (reuse the workspace ∪
  cartridge known-class world from US-422 + the US-426 binding-scope walk to exclude locals).
- Never touch **comments or strings**. **Idempotent** and **no cross-symbol bleed** as property-tested invariants.
- Works with **no `gst`**; the `WorkspaceEdit` never writes kernel/cartridge source.

## 3. Non-Goals
- **No selector rename** — dynamic dispatch keeps it unsafe offline; deferred to the Live Bridge (EPIC-007).
- **No rename of kernel/cartridge classes** — read-only reference facts; prepareRename rejects them.
- **No new-namespace modeling beyond the index** — resolution uses the namespaces the workspace index already
  records (`containerName`) ∪ the `Smalltalk` default; we do **not** implement full GST import/namespace
  scoping, alias resolution, or `Smalltalk at:put:` globals analysis.
- **No rewrite of arbitrary `#Foo` symbol literals** used as data (e.g. `#Foo` in a `#( … )` array or as a
  message argument unrelated to class definition) — only class-argument symbol positions.
- **No file renames** — renaming class `Foo` does **not** rename `Foo.st`; only the in-file text of references
  changes.
- **No semantic rewrites** beyond the identifier text (no reformatting, no metaclass/category changes).
- Not a cross-language/Tonel feature; GST chunk + brace formats only.

## 4. User Stories & Acceptance Criteria
**US-428**: As a **Smalltalk developer**, I want **safe workspace-wide rename of a class I defined**, so that **I
can rename a class and have every reference — definition, senders, superclass positions, and namespaced forms —
updated together, without touching kernel classes or same-named symbols.**

- **AC1**: `prepareRename` on a **workspace class** reference returns the identifier range (for a namespaced
  reference, the range of the **class segment only**); on a **kernel/cartridge class** it **rejects** with a
  reason (e.g. "`Object` is a kernel class and can't be renamed"); on a selector / non-class identifier /
  pseudo-variable / literal it rejects as before.
- **AC2**: Renaming a class rewrites **every resolved reference across the workspace** — the declaration
  (`subclass:` symbol + `extend`/`class` receiver), receiver/superclass `Variable` references, class-argument
  symbols, and the namespaced/binding forms `#{Foo}` / `Foo.Bar` / `Foo::Bar` (class segment) — and **nothing
  else**: no local variable sharing the name, no comment/string occurrence, no same-named class in another
  namespace.
- **AC3**: **Resolution-gating** — a qualified reference is rewritten only when its whole path resolves to the
  target class (workspace `name` + `containerName` ∪ `Smalltalk` default). A `Foo` bound as a local temp/arg in
  a method is **skipped** (scope-aware, mirroring the ivar shadow rule inverted).
- **AC4**: **New-name validation / kernel boundary** — the new name must be a valid class identifier
  (`[A-Z][A-Za-z0-9_]*`) and must **not** collide with an existing **kernel or workspace** class; otherwise the
  rename is refused (no edit) with a message. The rename never merges two distinct classes.
- **AC5**: Works with **no `gst`**; the `WorkspaceEdit` never touches kernel/cartridge source. **Idempotent**
  (`rename(rename(Foo→Bar), Foo→Bar)` is a no-op) and bleed-free — property-tested.
- **AC6**: A class rename that spans **more than one file** is returned with edits marked `needsConfirmation`
  (US-426 `withMultiFileConfirmation`), so VS Code routes it through the **Refactor Preview**; a single-file
  class rename stays instant; falls back to a plain `WorkspaceEdit` when the client lacks change-annotation
  support.

### Acceptance scenarios (Given/When/Then)
- **Given** a workspace class `Account` declared in `Account.st` (`Object subclass: #Account`) and referenced in
  `Bank.st` (`Account new`, `Account subclass: #SavingsAccount`) **When** I rename `Account`→`Ledger` **Then**
  the declaration symbol, both references, and the superclass position all become `Ledger`; both files are
  edited and the Refactor Preview is shown; a re-rename is a no-op.
- **Given** `Account class >> default` in one file and `#{Account}` in another **When** I rename
  `Account`→`Ledger` **Then** the `class`-side receiver and the binding-constant class segment both update.
- **Given** a class `Foo` in namespace `App` (`App.Foo` / `App::Foo` references) and an unrelated `Lib::Foo`
  **When** I rename `App`'s `Foo`→`Bar` **Then** `App.Foo`/`App::Foo` become `App.Bar`/`App::Bar` and
  `Lib::Foo` is untouched.
- **Given** the cursor on `OrderedCollection` (a kernel class) **When** I press F2 **Then** rename is rejected
  ("kernel class, read-only").
- **Given** I rename `Account`→`OrderedCollection` (an existing kernel class) or →`Bank` (an existing workspace
  class) **When** I confirm **Then** the rename is refused with a collision message (no edit).
- **Given** a method with a local temp named `Account` (shadowing) **When** I rename the class `Account`
  **Then** that local occurrence is **not** rewritten.

### Edge cases
- Declaration symbol variants — `subclass: #Foo`, `subclass: #'Foo'` (quoted), and legacy bare `subclass: Foo`:
  rewrite the identifier inside the symbol/variable, preserving `#`/quotes.
- A class defined once and `extend`ed in N files → all N files' `extend` receivers are considered.
- `Foo class >> sel` and `Foo class extend [ … ]` → the `Foo` before `class` is the reference to rewrite.
- A qualified reference whose qualifier is `Smalltalk` (the default namespace) resolves to a top-level workspace
  class of that name.
- An unsaved/dirty editor buffer → operate on live document text (open docs win over disk).
- A capitalized name that resolves to **neither** a workspace class nor a cartridge class (an unknown global) →
  rejected (we only rename *known workspace* classes, never a guessed global).
- `#Foo` as pure data (inside `#( Foo Bar )` or a non-class-defining argument) → **not** rewritten.

### Dependencies
- US-426 rename engine — `providers/rename.ts` (`prepareRenameAt`/`renameAt`/`classify`/
  `withMultiFileConfirmation`) + `xref/ivarRefs.ts` helpers (`classNameOf`, `classNameForDefinition`).
- US-412 workspace index — `providers/workspaceIndex.ts` (`IndexEntry.name`/`containerName`/`uri`) to enumerate
  candidate files and resolve namespaces.
- US-422 known-class resolution — the workspace ∪ cartridge class world (`kernelService.hasClass`,
  `semanticContext().classOrigin`) for the kernel boundary + false-positive exclusion.
- US-411 parser/AST — `Variable` (qualified names), `Definition`/`MethodDefinition`, `BindingConstant`, symbol
  `Literal`; the token stream for segment-level ranges.

## 5. Technical Design
- **New resolver** `server/src/xref/classRefs.ts` (pure; mirrors `ivarRefs.ts`) — given a class name, a
  `ClassWorld` (is-known-class + namespace resolver), and a set of `(uri, text)` files, return per-file
  occurrence ranges (`Map<uri, Ranged[]>`) of **resolved** references to that class. Per file, parse and walk:
  - **Declaration** — a `Definition` whose `classNameForDefinition` === target: rewrite the identifier inside
    the `subclass:` symbol/variable arg (for `extend`/`class`, the receiver `Variable`). Segment range derived
    from the token stream so `#`/quotes are preserved.
  - **`Variable` references** — a `Variable` node whose (a) unqualified `name` === target, **or** (b) qualified
    `name` (`A::B`/`A.B`) whose **last segment** === target and whose path **resolves** to the target class via
    the `ClassWorld` namespace resolver. Rewrite only the class-segment token. **Skip** a `Variable` that is
    **locally bound** (temp/arg) in its enclosing method/block — reuse the US-426 binding-scope walk so a local
    named like the class isn't rewritten (AC3).
  - **`BindingConstant`** `#{…}` — same resolution-gated last-segment rule; rewrite the class-segment token.
  - **Class-argument symbols** — the `#Foo` symbol in class-defining message args (`subclass:`, `superclass:`);
    not arbitrary `#Foo` data.
  - **`MethodDefinition` target** — `Foo >> sel` / `Foo class >> sel`: the target `Variable`.
  - Reuse `classNameOf`/`classNameForDefinition` from `ivarRefs.ts` for `Foo` / `Foo class` receivers.
- **`ClassWorld`** — a small resolution context (built in `server.ts` from the index ∪ cartridge, like
  `semanticContext`): `isKnownWorkspaceClass(name)`, `isKernelClass(name)` (`kernelService.hasClass`), and
  `resolveQualified(path) → { name, namespace } | undefined` using index `name`+`containerName` ∪ `Smalltalk`.
- **Fold into `providers/rename.ts`** — extend `classify` so a capitalized identifier that **resolves to a
  workspace class** classifies as `{ kind: 'class', name }` (instead of today's blanket
  "class rename not supported" reject); a **kernel** class rejects with the kernel-boundary reason. `renameAt`
  gains a class branch: validate the new name (class identifier + **no kernel/workspace collision**), then call
  `classRefs.classOccurrences` and build the multi-file `WorkspaceEdit`. `withMultiFileConfirmation` already
  forces the preview for the (typically multi-file) result.
- **Candidate files** — `server.ts renameCandidateFiles` gains a class branch: because a class can be referenced
  **anywhere**, candidate files = **all indexed workspace files** (∪ open dirty docs), not just files
  defining/`extend`ing it (unlike ivar rename). Kernel/cartridge files are never in the workspace index (AC5).
- **Wiring** `server.ts` — `onPrepareRename`/`onRenameRequest` are unchanged in shape; they pass the class
  `ClassWorld` + candidate files through. Never throw (typed reject → `ResponseError`).

### Testing strategy
- **Unit** (`server/test/classRename.test.ts` or extend `rename.test.ts`): the prepareRename accept/reject
  matrix (workspace class ✓, kernel class ✗, selector ✗, local sharing the name ✗); occurrence sets for each
  form (decl symbol, receiver, superclass, `class`/`extend`, `#{…}`, `A.B`/`A::B`); resolution-gating (unrelated
  namespace untouched); new-name collision (kernel + workspace). Routed: internal contract.
- **Property test** (extend `server/test/rename.property.test.ts`): rename a class then rename back = identity;
  edited offsets ⊆ resolved class references (no bleed into comments/strings/locals/other-namespace classes);
  idempotence. (AC2/AC5.)
- **Output eval** (`evals/datasets/rename/`): add class-rename cases — a **multi-file** class (defined in one
  file, used/extended in others), a **namespaced** case, and a **shadowing-local** case → golden edited files.
- **Handshake** (`handshake.test.mjs`): prepareRename accepts a workspace class and rejects a kernel class;
  rename returns a multi-file `WorkspaceEdit` with change annotations.
- **e2e** (`client/test-e2e/US-428.acceptance.test.js`): F2 on a workspace class renames across two buffers
  (apply the edits, assert the buffers — memory `e2e-edit-provider-gotchas`); F2 on a kernel class is rejected;
  a colliding new name is refused.

## 6. Manual Verification
Extension Host (`specs/US-428-Class-Rename/manual-qa-workspace/`): a class defined in one file, used/extended in
others, plus a namespaced reference and a **kernel-collision** case. Rename the class (all files update via the
Refactor Preview; a shadowing local + an unrelated-namespace same-name class stay put); F2 on a kernel class is
rejected; renaming into an existing kernel/workspace class name is refused; a second rename is a no-op. Matrix
in `verification.md`.

## 7. Risks & Limitations
- **Reference-form completeness is the correctness bar**: missing any referenced form leaves a dangling
  reference. The AST already distinguishes every form (Variable/qualified/BindingConstant/symbol/Definition);
  the risk is the **token-level segment range** for qualified/symbol forms — covered by per-form unit + property
  tests (no bleed, round-trip).
- **Workspace-wide candidate scan**: class rename must consider **all** indexed files (not just def/extend
  files), so it is heavier than ivar rename; relies on the index seeing every referencing file. A reference in
  a file outside a workspace folder or excluded by `files.exclude` won't be updated — documented, consistent
  with US-423 indexing.
- **Resolution-gating vs. flat workspaces**: most GST workspaces use the default `Smalltalk` namespace; the
  qualified-path resolver is a pragmatic index-backed model, **not** full GST namespace semantics — an exotic
  import/alias could resolve differently. Documented; the safety default is to **skip** (leave untouched) when a
  path doesn't resolve, never to over-rewrite.
- **False positives** (a local/ivar sharing the class name) are excluded by the scope-aware walk + the
  known-class world; property-tested for no bleed.
- **Kernel boundary** is enforced twice — reject renaming a kernel class, and reject a new name colliding with a
  kernel/workspace class — so a class rename can never edit or shadow frozen cartridge facts.
- Front end never throws — every failure path is a typed rejection.
