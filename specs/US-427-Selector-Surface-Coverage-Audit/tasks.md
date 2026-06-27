# Tasks: Selector-surface coverage audit

**ID**: US-427 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Map tasks to acceptance criteria where possible.

## Phase 1 — Spec & Setup
- [x] T001 Spec reviewed; `requirements-validation.md` gate passed (incl. §3.5 AC routing). **AC1** inventory written in spec §5.1.

## Phase 2 — Acceptance Harness (TDD — write tests BEFORE code)
- [x] T005 Route each AC: AC1/AC2 → reviewed docs; **AC3** → unit guard; AC4 → regression (all layers). No user-observable LSP surface → e2e stub removed (§3.5).
- [x] T006 Write the failing guard `src/test/snippets-verification.js` (unique prefixes + cartridge cross-check + prefix snapshot); add `test:snippets` script + wire into `npm run eval`.
- [x] T007 Confirm RED for the right reason (missing snapshot / pre-add prefix set) before adding snippets.

## Phase 3 — Implementation
- [ ] T010 **AC3** Add the 14 block snippets to `snippets/snippets.json`; generate prefix snapshot (`--update`); guard GREEN.
- [ ] T011 **AC2** Write `docs/decisions/0004-selector-surface-division.md`; link from CLAUDE.md code-map + spec.

## Phase 4 — Verify
- [ ] T900 **AC4** `test:parser` + `test:server` + `npm run eval` all green (guard included); completion/signature-help untouched.
- [ ] T901 `verification.md` gate passed (manual-QA tab-trigger matrix for each new prefix).
- [ ] T902 Doc-rot sweep (CLAUDE/user-stories/ROADMAP); CI green on Linux/macOS/Windows.
