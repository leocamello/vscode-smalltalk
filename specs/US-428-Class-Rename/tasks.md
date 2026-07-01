# Tasks: Class Rename

**ID**: US-428 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Map tasks to acceptance criteria where possible.

## Phase 1 — Spec & Setup
- [x] T001 Spec reviewed; `requirements-validation.md` gate passed (incl. §3.5 AC routing). No new ADR — extends the US-426 engine + reuses US-422 class world / US-412 namespace posture; forms/resolution/kernel-boundary are spec-level decisions from Clarify.
- [x] T002 Created `specs/US-428-*/manual-qa-workspace/` (Shape.st + Shape-Draw.st + Shape-Shadow.st + Shape-Namespace.st + README matrix) — multi-file class, namespaced (`#{…}`/`Smalltalk.`), shadowing-local, kernel-collision; all parse **0-diag** (verified).

## Phase 2 — Acceptance Harness (TDD e2e — write tests BEFORE code)
- [x] T005 AC routing confirmed (§3.5): AC1→e2e+unit+handshake; AC2→e2e+eval+property; AC3→unit+property; AC4→e2e+unit; AC5→property+unit; AC6→e2e+unit.
- [x] T006a RED unit `server/test/classRename.test.ts` — prepare accept/reject matrix + per-form occurrence sets (decl, receiver, superclass, `class`/`extend`, `#{…}`, `A.B`/`A::B`) + resolution-gating + collision (kernel + workspace) + non-class identifier (16 tests).
- [x] T006b RED property additions `server/test/rename.property.test.ts` — class round-trip, no-bleed (class segment only), safe re-parse (6 checks; 9→15).
- [x] T006c RED output eval `evals/datasets/rename/` — multi-file class, namespaced, shadowing-local, kernel-collision goldens (6→10 cases; world built via `buildClassWorldFromFiles`).
- [x] T006d RED handshake — prepareRename accepts workspace class `Greeter`, rejects kernel `Object`; rename returns a `WorkspaceEdit`.
- [x] T006e RED e2e `client/test-e2e/US-428.acceptance.test.js` — class rename updates decl/receiver/superclass/`#{…}` in-buffer (shadow local untouched); kernel-class rejected; colliding name refused (3 tests).
- [x] T007 Confirmed RED for the right reason (`Cannot find module '../src/xref/classRefs.ts'`).

## Phase 3 — Implementation
- [x] T010 `server/src/xref/classRefs.ts` — `classOccurrences(className, world, files)` + `ClassWorld` + `buildClassWorldFromFiles`; token-level class-segment ranges (qualified/binding/symbol); scope-stack local-skip; resolution-gated qualified/binding forms (AC2/AC3).
- [x] T011 Extended `server/src/providers/rename.ts` — `classify` `class` kind (workspace ✓ / kernel ✗ / unknown global ✗); `renameAt` class branch + new-name validation (`[A-Z]…` + kernel/workspace collision); `renameKindAt` export (AC1/AC4).
- [x] T012 Wired `server.ts` — `buildClassWorld` (index ∪ active-doc defs ∪ cartridge), `allWorkspaceFiles` (whole-workspace scan for class rename), `renameContext` picks breadth by kind; `world` threaded through both handlers (AC1/AC5/AC6).

## Phase 4 — Verify
- [x] T900 All acceptance tests GREEN: `test:parser` (classRename 16, rename 16, property 15), `test:server` (handshake clean), `eval` (rename 10/10; full eval green), `test:e2e` (US-428 3/3; suite 41/41). `check-types` + `lint` clean.
- [ ] T901 Manual-QA matrix Parts run in the Extension Host (`manual-qa-workspace/`); `verification.md` filled + **owner sign-off** (pending).
- [ ] T902 CI green on Linux/macOS/Windows + e2e job (PR closing #109).
- [ ] T903 Doc-rot sweep (v0.12.0): version + CHANGELOG; README (Class Rename); user-stories US-428 → Done + EPIC-005; **ROADMAP resequence (0.12=Class Rename, 0.13=Hardening/US-901) + dated delta**; CLAUDE.md (date, Shipped v0.12.0, Next, code map + classRefs bullet).
