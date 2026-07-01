# Tasks: Scope-aware Rename

**ID**: US-426 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Map tasks to acceptance criteria where possible.

## Phase 1 — Spec & Setup
- [x] T001 Spec reviewed; `requirements-validation.md` gate passed (incl. §3.5 AC routing). No new ADR (reuses US-417 scope + US-423/ADR-0003 indexing posture).

## Phase 2 — Acceptance Harness (TDD e2e — write tests BEFORE code)
- [x] T005 AC routing reconfirmed (§3.5).
- [x] T006a RED unit `server/test/rename.test.ts` — prepare accept/reject matrix + temp/arg set + newName validation (13 cases).
- [x] T006b RED property `server/test/rename.property.test.ts` — no-bleed + round-trip + parse-clean (9 checks).
- [x] T006c RED output eval `evals/datasets/rename/` — temp/arg, multi-file ivar, shadowing, selector-reject, collision-reject (6 cases); wired `eval:rename` into `npm run eval`.
- [x] T006d RED handshake — `renameProvider.prepareProvider` advertised.
- [x] T006e RED e2e `client/test-e2e/US-426.acceptance.test.js` — temp rename in-buffer; selector rejected; collision refused.
- [x] T007 Confirmed RED for the right reason (`MODULE_NOT_FOUND` rename.ts for unit/eval/property; `renameProvider` undefined for handshake).

## Phase 3 — Implementation
- [x] T010 Extracted `server/src/parser/scope.ts` (`bindingScope`, `pathToOffset`, `isLocallyBound`, **shadow-aware** `variableOccurrences`); refactored `documentHighlight` onto it — its 24 provider tests stay green.
- [x] T011 `server/src/providers/rename.ts` — `prepareRenameAt`/`renameAt`/`enclosingClassNameAt`; classify (local/ivar/reject-with-reason) + temp/arg rename + identifier/collision validation (AC1/AC2/AC4).
- [x] T012 `server/src/xref/ivarRefs.ts` — workspace-wide ivar resolver over `(uri,text)`; per-method shadow-skip; decl + reference ranges; `isDeclaredIvar` (AC3).
- [x] T013 Wired `server.ts`: `renameProvider:{prepareProvider:true}`; `onPrepareRename`/`onRenameRequest`; candidate-file discovery (active+dirty ∪ open docs ∪ class-filtered workspace files from the index/disk); reject via `ResponseError` (AC1/AC5).

## Phase 4 — Verify
- [x] T900 All acceptance tests GREEN: `test:parser` (rename 13+9, suite incl. documentHighlight 24), `test:server`, `eval` (rename 6/6), `test:e2e` (US-426 3/3; suite 38/38). `check-types` + `lint` clean. Also fixed a pre-existing US-416 e2e config-isolation leak.
- [~] T901 Created `specs/US-426-*/manual-qa-workspace/` (README A–D + split-class `Account.st`/`Account-Report.st` + shadowing `Account-Shadow.st`, all 0-diag; ivar rename verified to edit both files + skip the shadow) and filled `verification.md`. **Extension-Host matrix run still pending** (verification.md §4).
- [ ] T902 CI green on Linux/macOS/Windows + e2e job (after PR).
- [ ] T903 Doc-rot sweep staged for release: user-stories US-426 → Done, EPIC-005, ROADMAP, README features, CLAUDE.md, CHANGELOG, version bump.
