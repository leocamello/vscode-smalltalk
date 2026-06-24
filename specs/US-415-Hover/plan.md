# Implementation Plan: Hover

**ID**: US-415 | **Date**: 2026-06-24 | **Spec**: ./spec.md | **Branch**: `feature/US-415-hover`

## Summary
Add `textDocument/hover` as a pure provider (`server/src/providers/hover.ts`) that classifies
the node under the cursor (selector / class / variable / numeric literal) and renders Markdown
facts from the existing US-411 front end + US-412 workspace index + US-413/US-430 kernel
cartridge. Comment prose is gated on provenance (bundled facts-only; installed + workspace may
carry comments). Ship in two slices: A = facts + workspace prose + all 4 kinds; B = installed
cartridge prose.

## Approach
- **Provider, pure & never-throwing**, mirroring `completion.ts` / `definition.ts`: inputs are
  already-parsed (`offset`, `text`, `tokens`, `ast`, `symbols`) plus resolved selector/class
  lookups; returns `Hover | null`.
- **Cursor classification** extends the `definition.ts` deepest-node walk to also recognize
  `Literal` and in-scope `Variable` nodes.
- **Reuse**: `scopeVariablesAt` shape (completion) for AC3; `IndexEntry` (Method) for implementors;
  `SymbolNode.detail` (immediate superclass) surfaced as a new `IndexEntry.superclass` for AC2.
- **Kernel query facade** on `KernelIndexService`: `superclassOf`, `implementorsOf`, `arityOf`,
  `activeProvenance`, and (slice B) `classComment` / `selectorComment` (prose only when the active
  source `carriesProse`).
- **Markdown builders** (one per kind) keep AC5 (code fences) in one place.

## Steps
1. **Acceptance harness (RED first):** fill `client/test-e2e/US-415.acceptance.test.js` with one
   `executeHoverProvider` assertion per AC; add `server/test/hover.test.ts` unit asserts; extend
   the handshake test for `textDocument/hover`. Prove red.
2. **Slice A — provider + wiring:**
   - `IndexEntry.superclass` (from `SymbolNode.detail`) in `workspaceIndex.ts`.
   - `KernelIndexService`: `superclassOf` / `implementorsOf` / `arityOf` / `activeProvenance`.
   - `hover.ts`: classify + render selector (sig+implementors), class (chain+workspace comment),
     variable (kind+decl), literal (radix/scaled decode), all Markdown.
   - `server.ts`: advertise `hoverProvider: true`; add `connection.onHover` assembling inputs.
3. **Slice B — installed prose:** capture method/class comments in
   `indexKernelDirectoryToCartridge` → `documentation` fields, `carriesProse=true` for installed;
   carry through `cartridgeLoader`; add `classComment`/`selectorComment` to the service; surface
   in hover for installed provenance. Keep bundled facts-only (existing test).
4. **Eval:** add `evals/datasets/hover/{cases.json,run.ts}` mirroring `completion/`; wire into
   `npm run eval`.
5. **Verify:** all four gates green (`test:parser`, `test:server`, `test:e2e`, `eval`);
   fill `verification.md`; manual-QA spot-check in the Extension Host.

## Dependencies & Risks
- Depends on US-411 (AST/tokens/symbols), US-412 (workspace index), US-413/US-430 (kernel
  cartridge + `KernelIndexService`). No new client code, no new settings.
- Risk: installed-prose capture touches the cartridge `documentation` path — keep CI hermetic
  (inject the discovery probe list; do not depend on the dev box's real `/usr/local` kernel).
- Risk: best-effort comment extraction — render facts-only when no comment is found.
- Risk: malformed superclass links → walk with a visited set + depth cap.

## Verification
- **Acceptance harness (TDD e2e):** user-observable ACs pinned by failing tests in
  `client/test-e2e/US-415.acceptance.test.js` *before* implementation (red → green); data
  invariants → unit (`hover.test.ts`, `kernelIndex`/indexer tests); protocol shape → handshake.
  Routing recorded in `requirements-validation.md` §3.5.
- **Output eval:** `evals/datasets/hover/` golden cases via `npm run eval`.
- **Manual QA:** F5 Extension Host — hover a selector, a class, a temp/param, a `16rFF` literal;
  spot-check both bundled (facts-only) and installed (with comments) kernel tiers.
