# Implementation Plan: Completion + GNU Smalltalk kernel index

**ID**: US-413 | **Date**: 2026-06-21 | **Spec**: ./spec.md | **Branch**: `feature/US-413-completion-and-kernel-index`
**Architecture**: [ADR-0002](../../docs/decisions/0002-kernel-symbol-sourcing.md)

## Summary
Add `textDocument/completion` over the US-411 symbol table + US-412 workspace index, plus a
**kernel-library tier** sourced installed-first / bundled-fallback (ADR-0002) through a dialect-neutral
index model and a reusable `.st`-directory indexer. Ships in four reviewable PR slices; each keeps all
three test layers green and is held for owner merge.

## Approach
**Reuse, don't rebuild.** The kernel index is a workspace index over a kernel directory — it reuses
`parse` (`server/src/parser/parser.ts`) + `buildSymbolTable` (`server/src/parser/symbols.ts`) exactly as
`server/test/kernel.test.ts` already does over all 122 files with 0 diagnostics. New code is the neutral
model, the directory indexer, the build-time generator, the runtime resolution service, and the LSP
completion boundary (mirroring the US-412 provider + `parseCache` + dynamic `didChangeConfiguration`
patterns).

Key new modules: `server/src/kernel/model.ts`, `server/src/kernel/indexer.ts`,
`scripts/gen-kernel-index.ts`, `server/data/kernel-index.json` (committed), then
`server/src/kernel/kernelIndexService.ts` and `server/src/providers/completion.ts`.

## Steps (sliced into PRs)
1. **Slice A — index model + bundled generator (AC1).**
   - `model.ts` neutral types; `indexer.ts` `indexKernelDirectory(dir, meta)` (walk `*.st`, parse,
     buildSymbolTable, merge by class across files/chunks, dedup + sort, never throw).
   - `scripts/gen-kernel-index.ts` + `npm run gen:kernel-index`; generate & commit
     `server/data/kernel-index.json` (deterministic, no timestamp, facts only).
   - `server/test/kernelIndex.test.ts` (wired into `run.ts`): invariants + licensing no-prose gate
     (run **without** corpus) + regeneration drift guard (when corpus present).
2. **Slice B — kernel index service (AC5/AC6).** Load the bundled JSON; discover an installed kernel dir
   (`kernelPath` → `gnuSmalltalkPath` prefix → common locations); resolve `auto|bundled|off`; expose
   provenance-tagged lookups; re-resolve on config change. Unit + discovery tests.
3. **Slice C — completion provider (AC2–AC4, AC7 items).** Context detection at cursor (receiver →
   selectors; head → classes + scope variables) over `parseCache`; merge workspace + kernel; rank
   workspace > installed > bundled with prefix/camel-hump; keyword-selector snippets; provenance in
   items. Advertise `completionProvider` in `server.ts`. Provider unit + `test:server` completion +
   e2e tests.
4. **Slice D — settings + status UX (AC5/AC7).** `smalltalk.completion.kernelLibrary` +
   `kernelPath` in `package.json`; status-bar item showing resolved identity; one-time fallback notice.
   E2e asserting kernel + workspace completions.

## Dependencies & Risks
- **Depends on** (merged): US-411 `parse`/`buildSymbolTable`; US-412 `WorkspaceIndex`/`parseCache`/the
  dynamic config-pull pattern.
- **Build-time only**: the bundled GST kernel corpus (`../smalltalk-3.2.5/kernel`, absent in CI → JSON is
  committed).
- **Risks** (see spec §7): licensing (no-prose test), version/dialect confusion (provenance + status +
  notice + `off`), selector over-offering (ranking/filtering), determinism drift (drift guard), discovery
  brittleness (override + fallback). Packaging: ensure `kernel-index.json` ships in the VSIX (Slice C/D).

## Verification
- **Automated**: `test:parser` (index snapshot/invariant + licensing gate + drift guard +
  completion-context + discovery), `test:server` (completion handshake), `test:e2e` (fixture workspace).
  Eval: extend `evals/` with a completion dataset before the story is Done.
- **Manual**: the §6 / `verification.md` matrix in the Extension Development Host — real-corpus
  workspace, selector/class/variable completion, keyword snippet, installed vs bundled sourcing +
  provenance honesty, `off`, no-`gst`, cross-platform, clean-install VSIX. Hard gate for 0.5.0.
- CI watched per PR; **merges + the release tag held for the owner**. Close issue #1 with a demo; run
  the doc-rot + manual-QA release ritual before tagging v0.5.0.
