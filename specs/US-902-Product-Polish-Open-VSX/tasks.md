# Tasks: Product Polish & Open VSX (1.0.0)

**ID**: US-902 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Tasks map to acceptance criteria (AC1–AC9).

## Phase 1 — Spec & Setup
- [x] T001 Spec reviewed; `requirements-validation.md` gate passed (incl. §3.5 AC routing). **PASS**.
- [x] T002 Manual-QA workspace scaffolded at `specs/US-902-*/manual-qa-workspace/` (new-story omits it).

## Phase 2 — Acceptance Harness (TDD e2e — write tests BEFORE code)
- [x] T005 Route each AC to its layer per `requirements-validation.md` §3.5 (done in the gate).
- [x] T006 Wrote failing tests: AC2 (formatting-on) + idempotence e2e in
  `client/test-e2e/US-902.acceptance.test.js`; manifest assertions (AC1/AC3/AC4/AC5/AC7/AC9) in
  `server/test/manifest.test.ts`; cancellation unit test (AC6) in `server/test/cancellation.test.ts`;
  trust-gate decision (AC4) in `client/test/trustGate.test.ts`.
- [x] T007 Confirmed RED for the right reason (preview present; modules missing; formatting off-by-default).

## Phase 3 — Implementation (drive to green)
- [x] T010 (AC1) Removed `"preview": true` from `package.json`.
- [x] T011 (AC2) Removed `smalltalk.format.enable` property; stripped `enable` from `FormatSettings` /
  `DEFAULT_FORMAT_SETTINGS` + the three guards; dropped it from `getFormatSettings()`.
- [x] T012 (AC3) Added `order` 1–8 to every `contributes.configuration` property (GST path = 1).
- [x] T013 (AC4/AC5) Added top-level `capabilities.untrustedWorkspaces` + `capabilities.virtualWorkspaces`.
- [x] T014 (AC4) Gated `runCurrentFile` on `workspace.isTrusted` (+ pure `trustGate.ts` decision).
- [x] T015 (AC6) Threaded the `CancellationToken` through `onWorkspaceSymbol`, `onReferences`, the
  call-hierarchy handlers, and `onRenameRequest`; `providers/cancellation.ts` `readFilesCancellable`.
- [x] T016 (AC7) Added `ovsx` dev-dep + `deploy:ovsx` script; Open VSX publish step in `main.yml`.

## Phase 4 — Polish & Docs
- [x] T020 (AC8) README Demo section + captioned `media/demo-*.gif` placeholders + config-table update.
- [x] T021 (AC8) `docs/media-shotlist.md` — deterministic capture spec per demo asset.
- [x] T022 (AC8) `.github/ISSUE_TEMPLATE/{bug_report,feature_request}.md` + `config.yml`; `.github/labels.yml`
  (`area:parser`, `area:lsp`, `good-first-issue`); CONTRIBUTING label section updated.

## Phase 5 — Verify & Release
- [x] T900 Acceptance tests GREEN; all layers pass (`check-types`, `lint`, `test:parser`, `test:server`,
  `test:client`, `eval`, `test:e2e` (42 passing), `package`).
- [x] T901 (AC9) Doc-rot sweep: `docs/product/*`, ROADMAP, README, CLAUDE, CHANGELOG synced to 1.0 reality.
- [x] T902 (AC9) Bumped `package.json` to `1.0.0` (+ lockfile) + added the `1.0.0` CHANGELOG entry.
- [x] T903 `verification.md` gate authored (automated portion signed; manual-QA + owner handoffs pending).
- [ ] T904 Draft `v1.0.0` release notes; prompt owner for the handoffs (record media, provision
  `OVSX_PAT`) and the tag. **Owner tags — I never publish.**
