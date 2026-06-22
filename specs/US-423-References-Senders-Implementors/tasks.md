# Tasks: References + Senders/Implementors (Two-Tier Engine)

**ID**: US-423 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Map tasks to acceptance criteria where possible.

## Phase 1 — Spec & Setup
- [ ] T001 Spec reviewed; `requirements-validation.md` gate passed.
- [ ] T002 Confirm US-430 Slice B merged (cartridge loader + `crossReference` views available).

## Phase 2 — Implementation
### Slice A — workspace cross-reference index
- [ ] T010 `server/src/xref/workspaceXref.ts`: `selector → SendSite[]` from message-send nodes (US-411 AST).
- [ ] T011 `selector → ImplementorRef[]` from method defs (incl. class-side).
- [ ] T012 Incremental patch on `didChange` via `parseCache`; lazy build + file-size cap.
- [ ] T013 Unit tests: build + incremental patch correctness.

### Slice B — merge engine + references
- [ ] T020 `server/src/xref/resolve.ts`: normalize → dedup (key `(uri,line,selector)`, dev-box overlap) → precedence `Workspace ≻ Cartridge` → stable multi-key sort. (AC1/AC5/AC6)
- [ ] T021 `providers/references.ts` + advertise `referencesProvider`; plural go-to-definition. (AC3)
- [ ] T022 Unit (merge/dedup/ranking) + `test:server` references + e2e.

### Slice C — Senders/Implementors commands + virtual docs
- [ ] T030 Commands `smalltalk.sendersOf` / `smalltalk.implementorsOf`: tree + header node (union contract) + per-row provenance. (AC2)
- [ ] T031 Client `TextDocumentContentProvider` for `smalltalk-cartridge:` (read-only peek/jump targets).
- [ ] T032 E2e on the command tree + cartridge jump.

### Slice D — call hierarchy
- [ ] T040 `providers/callHierarchy.ts` (incoming = senders, outgoing = sends) reusing the engine. (AC4)
- [ ] T041 Unit + e2e.

## Phase 3 — Verify
- [ ] T900 `evals/datasets/references/` added + green (`npm run eval`).
- [ ] T901 `verification.md` gate passed (no `gst`; provenance + union header; dev-box de-dup).
- [ ] T902 CI green on Linux/macOS/Windows.
