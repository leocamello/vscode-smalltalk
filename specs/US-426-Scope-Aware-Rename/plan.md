# Implementation Plan: Scope-aware Rename

**ID**: US-426 | **Date**: 2026-06-30 | **Spec**: ./spec.md | **Branch**: `feature/US-426-scope-aware-rename`

## Summary
Add `textDocument/prepareRename` + `rename` for temporaries, block/method arguments, and **instance
variables** — scope-resolved, **offline**, never a blind text swap. Temps/args reuse the US-417 binding-scope
walk (single file); ivars use a new **workspace-wide** resolver that finds every file defining/`extend`ing the
class (via the US-412 index) and collects scope-resolved references per method, **skipping methods that shadow
the name**. Selectors, classes, kernel/cartridge symbols, globals and pseudo-vars are **rejected with a
reason**. New names are validated (identifier + no collision/shadow). All output is one `WorkspaceEdit`.

## Approach
- **Shared occurrence helper** — extract the variable-occurrence walk currently inline in
  `providers/documentHighlight.ts` into a small pure helper `parser/scope.ts`:
  `bindingScopeOf(ast, offset, name)` and `variableOccurrences(scopeNode, name) → Ranged[]` (decl + reads +
  assignment-target writes). `documentHighlight` is refactored to use it (no behaviour change — its tests
  stay green). Rename reuses it for temp/arg ranges and per-method ivar ranges.
- **Provider** `server/src/providers/rename.ts` (pure; injected file access so it unit-tests in memory):
  - `prepareRenameAt(ast, tokens, symbols, offset) → { range } | { reject: reason }`:
    1. Token at offset must be an `Identifier`; a selector token / keyword / literal / pseudo-var
       (`self super nil true false thisContext`) → reject with the matching reason.
    2. If the name is **locally bound** (param/temp in an enclosing block/method/program) → accept (temp/arg).
    3. Else if it is a **declared instance/class variable of the enclosing class** (symbol table) → accept (ivar).
    4. Else (global / unknown / capitalized class / resolves only in the cartridge) → reject.
  - `renameAt(uri, offset, newName, ctx) → WorkspaceEdit | { reject }`:
    1. Re-run the prepare classification; reject if not renameable.
    2. **Validate `newName`**: identifier grammar; not already bound in the resolved scope (collision/shadow)
       → reject with a message.
    3. Collect occurrences → `WorkspaceEdit` (`changes` keyed by uri).
- **Workspace-wide ivar resolver** `server/src/xref/ivarRefs.ts` (pure):
  `ivarOccurrences({ className, ivarName }, files) → Map<uri, Ranged[]>` where `files` is the candidate set
  `(uri, text)`. For each file: parse, find the class's body (def or `extend`) → its ivar decl range(s); for
  each method of the class run `bindingScopeOf`/`variableOccurrences` — **skip the method entirely if it
  binds `ivarName` as a local temp/arg** (shadow). Aggregate.
- **Candidate-file discovery** (`ctx` supplied by the server): from `WorkspaceIndex` collect URIs that have a
  class entry `name === C` or any entry with `container === C`; text comes from the open `documents` (live,
  dirty-buffer wins) else `fs.readFileSync(uriToPath(uri))`. Only URIs **under a workspace folder** (kernel/
  cartridge never appear → AC5).
- **Wiring** `server.ts`: advertise `renameProvider: { prepareProvider: true }`; `onPrepareRename` /
  `onRenameRequest` reuse `parseCache` (ast/tokens/symbols) + the index; reject via `ResponseError`
  (`InvalidRequest`) so VS Code surfaces the message; never throw.

## Steps
1. **Acceptance Harness (RED first)**: write failing unit (`rename.test.ts` — accept/reject matrix, temp/arg
   sets, validation), property (`rename.property.test.ts` — bleed-free + idempotence), output eval
   (`evals/datasets/rename/` — split-class ivar + shadowing goldens), handshake (`prepareProvider` + reject
   selector / accept temp), and e2e (`US-426.acceptance.test.js`). Prove RED.
2. Extract `parser/scope.ts`; refactor `documentHighlight` onto it (keep its tests green).
3. `providers/rename.ts` — prepare/validate/temp-arg rename → green the prepare matrix + temp/arg + validation.
4. `xref/ivarRefs.ts` — workspace-wide ivar resolver (shadow-skip) → green the ivar unit + split-class/shadow eval.
5. Wire `server.ts` capability + handlers + candidate-file discovery (open-docs ∪ workspace files).
6. Drive all layers GREEN.
7. Manual-QA workspace `specs/US-426-*/manual-qa-workspace/` (create it — new-story.sh doesn't); fill `verification.md`.
8. Doc-rot sweep staged for release (user-stories US-426 → Done, EPIC-005, ROADMAP, README, CLAUDE, CHANGELOG, version).

## Dependencies & Risks
- **Depends on** US-411 symbols, US-417 scope walk, US-412 index, `parseCache`, VS Code rename API.
- **Risk — cross-file ivar correctness** (the bulk): the candidate set must include every `extend` of the
  class; shadowing must be skipped per method. Mitigated by the split-class + shadow eval/property cases.
- **Risk — dirty buffers vs disk**: open docs must win over disk text (use `documents.get` first). Tested via
  the server path; the pure resolver takes explicit `(uri, text)` so it's deterministic.
- **Risk — over-reach**: only declared ivars (never a guessed global/classvar-as-global); selectors/classes
  firmly rejected. prepareRename reasons make the boundary explicit.
- **Risk — refactor regression** in `documentHighlight` from the helper extraction → its existing unit tests
  are the guard.

## Verification
- **Unit** `server/test/rename.test.ts`: prepare accept/reject matrix; temp/arg occurrence sets; newName
  validation (identifier + collision/shadow).
- **Property** `server/test/rename.property.test.ts`: edited-offset set ⊆ resolved scope (no bleed);
  rename-then-rename-back is identity; idempotence.
- **Output eval** `evals/datasets/rename/`: golden edited files incl. **multi-file ivar** and **shadowing**.
- **Handshake**: `renameProvider.prepareProvider` advertised; prepareRename rejects a selector, accepts a temp;
  rename returns a `WorkspaceEdit`.
- **Acceptance harness (TDD e2e)** `client/test-e2e/US-426.acceptance.test.js`: F2 renames a temp in-buffer;
  selector rejected; multi-file ivar edits both files; collision refused. Routing in `requirements-validation.md` §3.5.
- **Manual QA**: `manual-qa-workspace/` matrix in the Extension Host.
