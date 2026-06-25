# Tasks: References + Senders/Implementors (Two-Tier Engine)

**ID**: US-423 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Map tasks to acceptance criteria where possible.

## Phase 1 — Spec & Setup
- [x] T001 Spec reviewed; `requirements-validation.md` gate passed.
- [x] T002 Confirm US-430 Slice B merged (cartridge loader + `crossReference` views available — committed cartridge ships 1585 senders / 2459 implementors; `cartridgeLoader.sendersOf`/`implementorsOf` live).

## Phase 2 — Implementation
### Slice A — workspace cross-reference index
- [x] T010 `server/src/xref/workspaceXref.ts`: `selector → WorkspaceSendSite[]` from message-send nodes (US-411 AST), with enclosing-method context + `receiverHint`.
- [x] T011 Workspace implementors are **not** duplicated here — they already live in `WorkspaceIndex` as method symbols (name/containerName/classSide/selectionRange); the resolve layer reads them from there (DRY). `sendsFrom()` added for call-hierarchy outgoing.
- [x] T012 Incremental patch via per-URI slices + global selector map (`setFile`/`removeFile`); O(1) `sendersOf`. Wiring into `server.ts` `didChange` lands with Slice B.
- [x] T013 Unit tests (`server/test/workspaceXref.test.ts`, 8): build, context, ranges, hints, keyword/binary, top-level, incremental patch, cross-file union, `sendsFrom`.

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
