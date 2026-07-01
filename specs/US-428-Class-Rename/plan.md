# Implementation Plan: Class Rename

**ID**: US-428 | **Date**: 2026-07-01 | **Spec**: ./spec.md | **Branch**: `feature/US-428-class-rename`

## Summary
Extend the US-426 rename engine to **workspace classes** (the deferred sibling of ivar rename, #109). Put the
cursor on a class name → every **resolved** reference across the workspace is renamed: the declaration
(`Object subclass: #Foo`), receiver/superclass `Variable`s (`Foo new`, `Foo subclass: #Bar`), `Foo class` /
`Foo extend`, class-argument symbols, and the namespaced/binding forms `#{Foo}` / `Foo.Bar` / `Foo::Bar` (class
segment only, **resolution-gated**). A kernel/cartridge class is **rejected** (read-only); a new name colliding
with a kernel **or** workspace class is **refused**. A local/ivar sharing the name is never rewritten
(scope-aware + known-class world). Multi-file renames inherit the US-426 Refactor Preview. All output is one
`WorkspaceEdit`. Offline, no `gst`, never edits kernel/cartridge source.

## Approach
- **New resolver** `server/src/xref/classRefs.ts` (pure; mirrors `ivarRefs.ts`, injected `(uri, text)` files so
  it unit-tests in memory):
  - `classOccurrences(className, world, files) → Map<uri, Ranged[]>` — per file, `parse` + `tokenize`, then walk
    the AST collecting **class-segment** ranges of resolved references. Reuse `classNameOf` /
    `classNameForDefinition` from `ivarRefs.ts`.
  - Per reference form:
    - **Declaration** — a `Definition` with `classNameForDefinition === className`: locate the class identifier
      **token** inside the `subclass:`/`namespace` symbol (or bare-variable) arg, or the `extend`/`class`
      receiver `Variable`. Preserve `#`/quotes by ranging the identifier token, not the whole literal.
    - **`Variable` reference** — `name === className` (unqualified) → whole token; or a **qualified** name
      (`A::B`/`A.B`) whose last segment === className and whose path `world.resolveQualified` maps to the target
      → the **last-segment token only**. **Skip** when the `Variable` is **locally bound** (temp/arg) in its
      enclosing block/method (reuse `bindsName`/`pathToOffset` from `parser/scope.ts`).
    - **`BindingConstant`** `#{…}` — same resolution-gated last-segment rule; range the class-segment token.
    - **Class-argument symbol** — the `#Foo` symbol in a class-defining message arg only.
    - **`MethodDefinition` target** — `Foo >> sel` / `Foo class >> sel`: the target `Variable`.
  - Segment ranges come from the **token stream** (tokenize each file once) — find the identifier token whose
    text === className within the node's span; robust for qualified/symbol forms (mirrors the semantic-tokens
    `findToken` technique).
- **`ClassWorld`** resolution context (built in `server.ts`, like `semanticContext`):
  `isKnownWorkspaceClass(name)`, `isKernelClass(name)` (`kernelService.hasClass`), and
  `resolveQualified(path) → boolean` — split on `::`/`.`; last segment must be the target and the qualifier must
  match the class's `containerName` in the index, or be `Smalltalk` (default namespace) for a top-level class.
- **Fold into `providers/rename.ts`**:
  - Extend `classify` — after the local/ivar checks, a capitalized identifier that **resolves to a workspace
    class** (`world.isKnownWorkspaceClass`) classifies as `{ kind: 'class', name }`; a **kernel** class rejects
    with the kernel-boundary reason; an unknown capitalized global still rejects (today's message). The current
    blanket "class rename not supported" reject is replaced by this resolution.
  - `renameAt` gains a `class` branch — validate the new name (`[A-Z][A-Za-z0-9_]*` + **no kernel/workspace
    collision** via `world`), then `classOccurrences` → multi-file `WorkspaceEdit`.
  - `classify`/`prepareRenameAt`/`renameAt` take an optional `world` (pure default: a trivial world with no
    cartridge + no workspace classes, so existing unit tests still classify a bare `Foo` as reject unless a
    workspace class is supplied).
- **Candidate-file discovery** — `server.ts renameCandidateFiles` gains a **class branch**: a class can be
  referenced anywhere, so candidate files = **all indexed workspace files** ∪ open dirty docs (not just
  def/extend files as for ivars). Decide "is the cursor on a class?" via the same `world`/classification before
  choosing the candidate set.
- **Wiring** `server.ts` — build the `ClassWorld` from `index` ∪ `kernelService` (reuse `semanticContext`
  pieces); pass it into `prepareRenameAt`/`renameAt`; keep `withMultiFileConfirmation` (multi-file → Preview);
  reject via `ResponseError`; never throw.

## Steps
1. **Acceptance Harness (RED first)**: failing unit (`classRename.test.ts` — accept/reject matrix incl. kernel
   class, per-form occurrence sets, resolution-gating, kernel+workspace collision), property additions
   (`rename.property.test.ts` — class round-trip + no-bleed + idempotence), output eval (`evals/datasets/rename/`
   — multi-file class, namespaced, shadowing-local goldens), handshake (accept class / reject kernel class),
   e2e (`US-428.acceptance.test.js`). Prove RED.
2. `server/src/xref/classRefs.ts` — the resolver + `ClassWorld` type; green the per-form unit + goldens.
3. Extend `providers/rename.ts` `classify`/`renameAt` with the `class` kind + kernel-boundary + collision.
4. Wire `server.ts` — `ClassWorld` build, class branch in `renameCandidateFiles`, pass-through in handlers.
5. Drive all layers GREEN (`test:parser`, `test:server`, `test:e2e`, `eval`); `check-types` + `lint` clean.
6. Manual-QA workspace `specs/US-428-*/manual-qa-workspace/` (create it — new-story.sh doesn't); fill
   `verification.md`.
7. Doc-rot sweep staged for release (v0.12.0): user-stories US-428 → Done, EPIC-005, **ROADMAP resequence
   0.12=Class Rename / 0.13=Hardening**, README, CLAUDE, CHANGELOG, version.

## Dependencies & Risks
- **Depends on** US-426 rename engine + `parser/scope.ts` + `xref/ivarRefs.ts` helpers; US-412 index
  (`name`/`containerName`); US-422 class world (`kernelService.hasClass`); US-411 AST + token stream.
- **Risk — reference-form completeness** (the correctness bar): a missed form = a dangling reference. Mitigated
  by per-form unit cases + the no-bleed/round-trip property + multi-form eval goldens.
- **Risk — token-level segment ranges** for qualified (`A.B`/`A::B`) + symbol (`#Foo`/`#'Foo'`) forms: the
  rewrite must hit only the class identifier token, never the qualifier/`#`/quotes. Covered by dedicated unit +
  property (round-trip parses clean).
- **Risk — false positives**: a local/ivar or an unrelated-namespace class sharing the name. Excluded by the
  scope-aware walk + `world` resolution-gating; property-tested for no bleed.
- **Risk — heavier candidate scan**: class rename considers all indexed files (vs def/extend only for ivars);
  relies on the index seeing every referencing file (documented, consistent with US-423). Watch perf on the
  1k-file target (feeds US-901).
- **Risk — kernel boundary**: enforced twice (reject renaming a kernel class; reject a new name colliding with a
  kernel/workspace class) so a class rename can never edit or shadow frozen cartridge facts.
- **Risk — dirty buffers vs disk**: open docs win over disk (`documents.get` first); the pure resolver takes
  explicit `(uri, text)` for determinism.

## Verification
- **Unit** `server/test/classRename.test.ts`: prepare accept/reject matrix (workspace class ✓, kernel class ✗,
  selector ✗, local sharing name ✗); per-form occurrence sets; resolution-gating (unrelated namespace
  untouched); new-name collision (kernel + workspace) + non-class identifier.
- **Property** `server/test/rename.property.test.ts` (extended): class rename-then-rename-back = identity;
  edited ⊆ resolved class refs (no comment/string/local/other-namespace bleed); idempotence.
- **Output eval** `evals/datasets/rename/`: multi-file class, namespaced (`A::B`/`A.B`), shadowing-local goldens.
- **Handshake**: prepareRename accepts a workspace class, rejects a kernel class; rename returns a multi-file
  change-annotated `WorkspaceEdit`.
- **Acceptance harness (TDD e2e)** `client/test-e2e/US-428.acceptance.test.js`: F2 renames a class across two
  buffers (apply edits, assert buffers — memory `e2e-edit-provider-gotchas`); F2 on a kernel class rejected;
  colliding new name refused. Routing in `requirements-validation.md` §3.5.
- **Manual QA**: `manual-qa-workspace/` matrix in the Extension Host.
