# Implementation Plan: Diagnostics

**ID**: US-414 | **Date**: 2026-06-23 | **Spec**: ./spec.md | **Branch**: `feature/US-414-diagnostics`

## Summary
Ship 0.6.0 diagnostics in three reviewable slices. The parser tier (Slice A) is small — `parse()`
already returns `diagnostics`; the work is publishing them as `Diagnostic`s, debounced, on open/change.
The `gst` tier (Slice B) is the opt-in second opinion (save + command, no zombies). Slice C adds the
two trivial bracket/paren quick fixes. Each slice is a PR with all three test layers + `npm run eval`
green; then the 0.6.0 release ritual.

## Approach
Reuse existing patterns: the version-keyed `parseCache` (extend, don't re-parse), the
`scheduleIndex`/`INDEX_DEBOUNCE_MS` debounce shape (a sibling diagnostics timer), the `gstLocator`
resolution (factor so the server can reuse it), and the `completion/` eval harness (mirror into
`diagnostics/`). New code is small and isolated: `providers/diagnostics.ts`, `providers/codeAction.ts`,
`gst/gstRunner.ts`.

## Steps (by slice)

### Slice A — live parser diagnostics (AC1) — *this PR first*
1. `providers/diagnostics.ts`: pure `toDiagnostics(LexDiagnostic[]) → Diagnostic[]` (severity + range
   map, `source: 'smalltalk'`, `code: 'smalltalk(parse)'`). Unit-tested.
2. `parseCache.ts`: keep `diagnostics` on the cache entry; add `getDiagnostics(doc)` (no second parse).
3. `server.ts`: dedicated 250 ms diagnostics debounce (separate timer map); publish on `onDidOpen` +
   `onDidChangeContent`; clear (empty publish) + cancel timer on `onDidClose`.
4. Acceptance harness (RED first): e2e asserts squiggles with code `smalltalk(parse)` on malformed
   input; unit for the mapping; new `evals/datasets/diagnostics/` golden dataset.

### Slice B — opt-in gst diagnostics (AC2/AC3) — ⛔ DEFERRED to EPIC-007 (US-705)
> Built and verified, then removed from 0.6.0 (scope decision 2026-06-24 — spec §7). gst 3.2.5 emits
> only syntax errors the parser already catches better; the real (semantic) value needs a runtime, so
> this moves to the Live Bridge. The steps below are the original plan, preserved for the EPIC-007
> revival (implementation in git history, commit `a02518d`).

5. `gst/gstRunner.ts`: `parseGstStderr(text, uri) → Diagnostic[]` (pure, fixture-tested) + a runner
   that spawns gst with a timeout, one in-flight child per uri, kill-on-supersede.
6. Factor gst resolution so the server resolves the executable (setting → PATH), injectable for tests.
7. `server.ts`: on `onDidSave` when `smalltalk.diagnostics.useGst`, run the tier; clear stale gst
   diagnostics on the next change; union with parser diagnostics per uri.
8. `smalltalk.diagnostics.useGst` setting + `smalltalk.validateWithGst` command (client→server bridge).
9. No-zombie test (injected fake spawner, rapid edits) + opt-in wiring coverage.

### Slice C — trivial code actions (AC4)
10. Advertise `codeActionProvider`; `providers/codeAction.ts` offers insert-missing-`]`/`)` quick fixes
    from the matching parse diagnostic. Unit + e2e.

### Release — 0.6.0
11. Doc-rot audit + manual-QA matrix (`verification.md`, real malformed `.st` + clean VSIX) → bump
    version + CHANGELOG → **check the MARKETPLACE PAT** → cut `v0.6.0` Release (CI publishes).

## Dependencies & Risks
- US-411 parser diagnostics (done). US-301 gst resolution lineage (reuse). ADR-0001 (gst optional).
- Risk: gst stderr is line-only (no column/severity) — confirmed; whole-line ranges (spec §6).
- Risk: zombie processes — mitigated by timeout + kill-on-supersede + single in-flight child per uri.
- Risk: CI has gst (this dev box does) — keep gst tests hermetic via fixture strings / injected spawner
  ([[dev-box-has-gst-installed]]); the live gst run is local-only manual QA.

## Verification
- **Acceptance harness (TDD e2e):** AC1 + AC4 pinned by failing tests in
  `client/test-e2e/US-414.acceptance.test.js` before code (red → green). AC2/AC3 routed to unit +
  injected-spawner integration + local manual QA (gst optional in CI). Routing in
  `requirements-validation.md` §3.5.
- Three layers green per slice (`test:parser`, `test:server`, `test:e2e`) + `npm run eval` with the new
  `evals/datasets/diagnostics/` dataset.
