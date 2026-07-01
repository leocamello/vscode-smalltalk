# Specification: Scope-aware Rename

**ID**: US-426
**Feature**: `textDocument/rename` + `textDocument/prepareRename` — safe, scope-aware rename of temporaries, block/method arguments, and instance variables, **offline, no `gst`**
**Status**: Draft
**Owner**: Leonardo Nascimento
**Created**: 2026-06-30
**Epic**: EPIC-005 (Console & Cartridges) → ~1.0
**Depends on**: US-411 (parser/AST + symbol table), US-412 (workspace symbol index), US-417 (scope-aware resolution reused from `documentHighlight`)

## 1. Overview
Let a developer rename a **temporary**, a **block/method argument**, or an **instance variable** and have
every in-scope reference updated — and *only* those — with the same scope discipline the editor already uses
for highlight/definition. The hard safety property in a dynamically-typed language is **honesty about what
can be resolved**: temporaries and arguments are fully resolvable within their method/block; instance
variables are resolvable within their class (workspace-wide, across split definitions); **message selectors
are not** (dynamic dispatch) and are **explicitly excluded**. Rename refuses anything it cannot resolve
safely, with a reason, rather than doing a blind text swap. Works with no `gst`; never edits kernel/cartridge
source.

## 2. Goals
- `textDocument/prepareRename`: accept a temp/arg/ivar that resolves in the workspace and return the
  identifier range; **reject** everything else (selector, kernel/cartridge symbol, global, `self`/`super`/
  pseudo-variables, literal) with an explanatory message.
- `textDocument/rename`: produce a `WorkspaceEdit` renaming **exactly** the resolved occurrences:
  - **temp / arg** → within its binding scope (the enclosing block/method), current file.
  - **instance variable** → **workspace-wide**: every reference in the class's definition + every method of
    that class across **all** files that define or `extend` it — skipping occurrences shadowed by a local
    temp/arg in any given method.
- **New-name validation**: reject (no edit, with a message) when the new name is not a valid Smalltalk
  identifier, or when it **collides/shadows** an existing binding in the resolved scope (the AC2
  "no cross-symbol bleed" guarantee).
- **Idempotent** and **no cross-symbol bleed** as property-tested invariants.
- Works with **no `gst`**; never writes to kernel/cartridge files.

## 3. Non-Goals
- **No selector rename** — dynamic dispatch makes the sender/implementor set unknowable; renaming a selector
  is the dangerous refactor this story deliberately excludes (a future, separately-gated story may revisit).
- **No class rename** — out of scope for v1 (globals + cross-file class references + the cartridge boundary);
  only temps/args/ivars.
- **No rename of kernel/cartridge symbols** — those are read-only reference facts; prepareRename rejects them.
- **No semantic rewrites** beyond the identifier text (no reformatting, no re-typing, no signature changes).
- Not a cross-language/Tonel feature; GST chunk + brace formats only (the layered parser the Console uses).

## 4. User Stories & Acceptance Criteria
**US-426**: As a **Smalltalk developer**, I want **safe scope-aware rename of temps/args/instance vars (not
selectors)**, so that **I can refactor within a method/class without risk in a dynamically-typed language.**

- **AC1**: `prepareRename` returns the identifier range for a temporary, block/method argument, or instance
  variable that resolves in the workspace; for a selector, kernel/cartridge symbol, global, `self`/`super`/
  pseudo-variable, or literal it **rejects** with a reason (e.g. "Selector rename isn't supported — only
  temporaries, arguments, and instance variables can be renamed").
- **AC2**: Renaming a **temporary or argument** rewrites exactly its occurrences within the enclosing
  block/method (declaration + reads + assignment-target writes) and **nothing else** — no same-name
  variable in a sibling scope, no selector, no shadowed binding is touched.
- **AC3**: Renaming an **instance variable** rewrites its references **across every workspace file** that
  defines or `extend`s the class (declaration + all method references), and **skips** any method where a
  local temp/arg shadows the name. (Workspace-wide, scope-resolved per method.)
- **AC4**: The new name is validated — if it is not a valid identifier, or it **collides/shadows** an
  existing binding in the resolved scope, `rename` is refused (no edit) with a message; the rename never
  merges two distinct symbols.
- **AC5**: Works with **no `gst`**; the resulting `WorkspaceEdit` never touches kernel/cartridge source.
  **Idempotent** (`rename(rename(x, a→b), a→b)` is a no-op) and bleed-free — property-tested.
- **AC6**: A rename that spans **more than one file** (workspace-wide ivar) is returned with edits marked
  `needsConfirmation`, so VS Code routes it through the **Refactor Preview** — the user reviews and confirms
  the multi-file changes rather than applying them blind on Enter. Single-file renames stay instant; falls
  back to a plain `WorkspaceEdit` when the client lacks change-annotation support.

### Acceptance scenarios (Given/When/Then)
- **Given** the cursor on a temporary `acc` in a method **When** I rename it to `total` **Then** only that
  method's `acc` occurrences become `total`; an `acc` in another method is unchanged; a re-rename is a no-op.
- **Given** an instance variable `balance` declared in `Account.st` and referenced in a method in
  `AccountExtra.st` (`Account extend [...]`) **When** I rename `balance` to `funds` **Then** both files are
  edited; a method with a local temp `balance` keeps its local untouched.
- **Given** the cursor on a message selector `printOn:` **When** I press F2 **Then** rename is rejected with a
  message; no edit is offered.
- **Given** the cursor on `OrderedCollection` (a kernel class) **When** I press F2 **Then** rename is rejected
  (kernel/cartridge symbols are read-only).
- **Given** a temp `x` and an existing temp `y` in the same method **When** I rename `x`→`y` **Then** the
  rename is refused with a collision message (no edit).

### Edge cases
- `self`, `super`, `thisContext`, `nil`, `true`, `false` → not renameable (rejected).
- An assignment target (`acc := …`) is a write occurrence and renamed with the rest.
- A temp/arg that **shadows** an ivar → renaming the local touches only the local; renaming the ivar skips
  that method.
- A class defined once but `extend`ed in N files → all N considered for ivar rename.
- An unsaved/dirty editor buffer → operate on the live document text (open docs win over disk).
- A name that resolves to a global/classvar/unknown (not a declared ivar) → rejected (we only rename
  *declared* ivars, never a guessed global).

### Dependencies
- US-411 symbol table (`server/src/parser/symbols.ts`) — classifies temp/arg/ivar/classvar/global.
- US-417 scope resolution (`providers/documentHighlight.ts` `bindingScope`/occurrence walk) — reused for the
  per-method/-block resolution.
- US-412 workspace index (`providers/workspaceIndex.ts`) — to find all files defining/extending a class.

## 5. Technical Design
- **Provider** `server/src/providers/rename.ts` (pure; `vscode-languageserver-types` only):
  - `prepareRenameAt(ast, tokens, symbols, offset) → { range } | { reject: reason }` — resolve the cursor
    symbol and classify it via the symbol table. Accept temp/arg/ivar (declared); reject otherwise with a
    typed reason.
  - `renameAt(uri, offset, newName, …) → WorkspaceEdit | { reject }` — validate `newName` (identifier regex
    + collision/shadow check in scope), then collect occurrence ranges and build the edit.
- **Temp/arg occurrences**: reuse the `documentHighlight` binding-scope walk (decl + reads + assignment
  writes within the enclosing block/method). Single file.
- **Instance-variable occurrences (workspace-wide)** — new resolver `server/src/xref/ivarRefs.ts`:
  1. From the cursor, find the enclosing **class name** and confirm `name` is a **declared ivar** of that
     class (an `InstanceVariables` decl in the class body) via the symbol table — else reject.
  2. Query the workspace index for every file with a definition/`extend` of that class.
  3. For each such file: parse (via `parseCache`), and for the class body + each of its methods run the
     scope resolution; **skip** a method that binds `name` locally (temp/arg shadow); otherwise collect the
     ivar's decl + reference ranges. Aggregate into per-file `TextEdit[]`.
  - Only files **under a workspace folder** are touched (mirrors the US-423 indexing rule); kernel/cartridge
    files are never in this set (AC5).
- **New-name validation (AC4)**: `newName` matches the identifier grammar; and is not already bound in the
  resolved scope — for a temp/arg, not equal to another temp/arg/ivar visible in the method; for an ivar,
  not equal to another ivar of the class (and not a local in a method that would now capture it). On failure
  → reject with a message (the server returns a `ResponseError` so VS Code shows it).
- **Wiring** `server.ts`: advertise `renameProvider: { prepareProvider: true }`; handlers
  `onPrepareRename` / `onRenameRequest` reuse `parseCache` + the workspace index; never throw (reject, don't
  crash).

### Testing strategy
- **Unit** (`server/test/rename.test.ts`): prepareRename accept/reject matrix (temp/arg/ivar vs selector/
  kernel/global/self/literal); temp/arg occurrence sets; collision/identifier validation. Routed: internal
  contract.
- **Property test** (`server/test/rename.property.test.ts`): over corpus/synthesized inputs — renaming any
  resolvable symbol then renaming back is identity; the set of edited offsets equals the highlight set;
  **no occurrence outside the resolved scope is touched** (no cross-symbol bleed); idempotence. (AC2/AC5.)
- **Output eval** (`evals/datasets/rename/`): input file(s) + cursor + newName → golden edited file(s),
  including a **multi-file ivar** case (split class) and a **shadowing** case. (AC3.)
- **Handshake** (`handshake.test.mjs`): `renameProvider.prepareProvider` advertised; prepareRename rejects a
  selector and accepts a temp; rename returns a `WorkspaceEdit`.
- **e2e** (`client/test-e2e/US-426.acceptance.test.js`): F2 on a temp renames in-buffer; F2 on a selector is
  rejected; multi-file ivar rename edits both files; collision is refused.

## 6. Manual Verification
Extension Host (`specs/US-426-Scope-Aware-Rename/manual-qa-workspace/`): rename a temp/arg (only that method
changes); rename an ivar declared in one file and used via `extend` in another (both files update; a
shadowing local stays put); F2 on a selector and on a kernel class are both rejected with a message; a
colliding new name is refused; a second rename is a no-op. Matrix in `verification.md`.

## 7. Risks & Limitations
- **Scope expansion vs the M estimate**: workspace-wide ivar rename (the owner's choice for safety) adds a
  cross-file ivar-reference resolver beyond the original M sizing — effectively L. Temps/args remain trivial;
  the ivar resolver is the bulk of the work and its main risk.
- **Cross-file correctness** hinges on the workspace index seeing every `extend` of a class; a class extended
  in a file not under a workspace folder (or excluded by `files.exclude`) won't be updated — documented, and
  consistent with how references/indexing already behave (US-423).
- **Shadowing** is the subtle case: the per-method scope check must skip a method that re-binds the ivar name
  locally — covered by the shadowing property/eval case.
- **Selector & class rename stay out** by design; prepareRename's reasons make the boundary explicit so users
  aren't surprised.
- Front end never throws — every failure path is a typed rejection, never a crash.
