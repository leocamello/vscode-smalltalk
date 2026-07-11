# Implementation Verification Checklist — US-902 (1.0.0)

**Purpose**: Verify implementation correctness AFTER coding
**Type**: Implementation Verification
**Date**: 2026-07-06

---

## Section 1: Acceptance Criteria
- [x] All ACs routed + implemented (AC1–AC9; see `tasks.md` T010–T022, T900–T904).
- [x] Each AC has a passing test / static check:
  - **AC1** preview removed → `manifest.test.ts` ✅ + `npm run package` shows no preview.
  - **AC2** formatting always-on → **e2e** `US-902.acceptance.test.js` (edits with no enable flag + idempotent) ✅; `manifest.test.ts` (no `format.enable` property) ✅.
  - **AC3** GST path first → `manifest.test.ts` (strictly lowest `order`) ✅.
  - **AC4** untrusted decl + Run-refused → `manifest.test.ts` (`limited` + `restrictedConfigurations`) ✅; `trustGate.test.ts` (decision) ✅; manual-QA matrix step 3 for the UX.
  - **AC5** virtual decl → `manifest.test.ts` (`limited` + description) ✅.
  - **AC6** cancellation → `cancellation.test.ts` (early bail / no full scan / pre-cancelled = 0 reads) ✅; threaded through `server.ts` handlers.
  - **AC7** Open VSX in CI → `manifest.test.ts` (`ovsx` dep + `deploy:ovsx`) ✅; `.github/workflows/main.yml` step (release-gated, secret-guarded).
  - **AC8** polish docs → README Demo section, `docs/media-shotlist.md`, `.github/ISSUE_TEMPLATE/*`, `.github/labels.yml` present.
  - **AC9** docs synced + 1.0.0 → doc-rot sweep (below) + `manifest.test.ts` (major ≥ 1) ✅.
- [x] The user-observable ACs were **red before** implementation: AC2 e2e was red (formatting off-by-default returned no edits); `manifest.test.ts` was red on `preview`; `cancellation.test.ts` / `trustGate.test.ts` were red (modules missing). Captured in the transcript.

## Section 2: Code Quality
- [x] `npm run check-types` passes.
- [x] `npm run lint` passes.
- [x] `npm run test:parser` / `test:client` / `test:server` / `eval` pass.
- [x] `npm run test:e2e` — **42 passing** (incl. US-902 AC2 + the revised US-416 suite).
- [x] `npm run package` — clean 1.0.0 VSIX, no preview.
- [x] No new `any`; new public helpers (`cancellation.ts`, `trustGate.ts`) carry JSDoc.

## Section 3: Constitutional Compliance
- [x] **Native**: standard `capabilities.*`, `workspace.isTrusted` / `manageTrust`, LSP cancellation tokens.
- [x] **Zero Config**: removing `format.enable` removes a config step; settings reorder surfaces the one that matters. No new required config.
- [x] **Robustness**: cancelled requests return the empty-result shape; virtual/untrusted paths degrade best-effort; front end never throws.
- [x] **No Telemetry (VII)**: `ovsx` is a publish-time CLI, not shipped code; no runtime network surface added (guard test still green).
- [x] **TDD**: acceptance + unit tests written before code, proven red.

## Section 4: Doc-Rot Sweep (release ritual)
- [x] `CHANGELOG.md` — `1.0.0` entry added.
- [x] `package.json` — version `1.0.0`; `package-lock.json` synced.
- [x] `CLAUDE.md` — Shipped v1.0.0 bullet; formatting/next/provider notes updated; `format.enable` references corrected.
- [x] `docs/ROADMAP.md` — 1.0 row ✅; 1.0.0-shipped delta; progress line; EPIC-004 = Complete at 1.0.
- [x] `docs/product/user-stories.md` — US-902 Done + DoD + AC1–9; status-summary "Next" → post-1.0.
- [x] `docs/product/epics.md` — EPIC-004 Complete at 1.0; EPIC-005 "next" staleness corrected.
- [x] `README.md` — Demo section; formatting always-on; Restricted-Mode + Open VSX notes; config table (removed `format.enable`, fixed stray artifact).
- [x] `specs/US-416-*/spec.md` — AC4 annotated **[SUPERSEDED by US-902]**.

## Section 5: Manual Verification (matrix in `manual-qa-workspace/README.md`)
- [ ] Settings order (GST path first) in the real Settings UI — **owner**.
- [ ] Formatting works with no enable setting; `format.enable` absent — **owner** (e2e-covered; confirm in-host).
- [ ] Restricted Mode: Run Current File refused + Manage-Trust prompt; static intelligence still works — **owner**.
- [ ] No "Preview" badge on the installed VSIX — **owner**.
- [ ] Clean VSIX smoke install — **owner**.

## Section 5b: Manual-QA bug-bash (owner pass, 2026-07-06)
Owner ran the full matrix in `manual-qa-workspace/`. Six items flagged; **none were US-902 code
regressions** — four were QA-matrix wording errors and two were QA test cases that exercised inherent
design behavior badly. All fixed in the QA workspace (source + matrix); the shipped code is unchanged.

| Ref | Finding | Resolution |
| --- | --- | --- |
| A2 | `Literals.st` had no `.` after `^…`, so the radix-swallow claim had nothing to show. | Added a `radixThenPeriod` method (`x := 16rFF.`) + corrected the matrix. |
| B5 | F12 on a class returns **multiple** definitions, not one. | **Expected** — the declaration + each `Account extend` are all definition sites (same "show all" design as plural go-to-def on selectors). Corrected the matrix wording. |
| C3 | `at:put:` inserts `at: put:` with tabbable caret stops, not literal `at:⟨1⟩ put:⟨2⟩`. | **Correct behavior** — `⟨n⟩` was shorthand for snippet tab-stops. Corrected the matrix wording. |
| C11 | Close-string fix put `'` after a same-line `]`, leaving it broken. | The fix closes at the end of the string's opening line (documented); the test case had `]` on that line. Restructured `aString` to put the string on its own line → fix now re-parses clean (verified). |
| C12 | How to enable semantic highlighting was unclear. | Added explicit steps (`editor.semanticHighlighting.enabled: true`, theme note). |
| E3 | Format Selection on a single in-class method line does nothing. | **Inherent limitation** — the formatter formats the span as a standalone unit and a bare `^` isn't valid at top level, so the fragment is returned unchanged. Changed E3 to a top-level-statement selection (formats cleanly) + documented the limitation in the file header and matrix. **Candidate post-1.0 follow-up:** context-aware range formatting (format whole doc, filter edits to the selection) — a formatter behavior change needing its own eval; out of US-902 scope. |

## Section 6: Owner Handoffs Before the `v1.0.0` Tag
- [x] Record the five demo GIFs per `docs/media-shotlist.md` → `media/demo-*.gif`, commit them. **Done (owner recorded via OBS; converted with `scripts/make-gif.sh`; committed 2026-07-06).**
- [x] Provision the `OVSX_PAT` secret (Open VSX access token), analogous to `MARKETPLACE`. **Done (owner, 2026-07-06).**
- [ ] Confirm the `MARKETPLACE` PAT (Azure DevOps) has not expired (~yearly) before tagging.
- [ ] Create the `v1.0.0` GitHub Release (draft notes provided) → CI publishes to Marketplace + Open VSX.

## Section 7: Sign-Off
- [x] All automated layers green (unit + eval + e2e + package); doc-rot swept; spec/gate/tasks complete.
- [x] Manual-QA matrix run by owner; six findings triaged (§5b) — all doc/fixture fixes, no code regressions.
- [x] Demo GIFs recorded + committed; `OVSX_PAT` provisioned.
- [ ] **PO accepts** — pending only the `v1.0.0` tag (which triggers publish).
