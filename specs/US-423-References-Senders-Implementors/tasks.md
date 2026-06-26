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
- [x] T020 `server/src/xref/resolve.ts`: normalize both tiers → dedup (dev-box overlap by logical class+side / enclosing-method key) → precedence `Workspace ≻ Cartridge` → stable multi-key sort (provenance, hint confidence, class, line). Synthetic `smalltalk-cartridge:` URIs. (AC1/AC5/AC6)
- [x] T021 `providers/references.ts` (pure converters) + advertise `referencesProvider`; `onReferences`; plural go-to-definition (`Location[]`, or `LocationLink[]` when the client declares `linkSupport`). Cartridge cross-ref exposed via `KernelIndexService.crossReferenceSenders/Implementors`/`cartridgeId`. Single shared `walkStFiles` feeds both indexes; xref kept in lockstep on open/change/close/rebuild. (AC3)
- [x] T022 Unit (`server/test/resolve.test.ts`, 7: union/dev-box dedup/ranking/no-cartridge) + `test:server` references + plural definition; e2e stub covers AC1/AC3.

### Slice C — Senders/Implementors commands + virtual docs
- [x] T030 Commands `smalltalk.sendersOf` / `smalltalk.implementorsOf`: a TreeView (new "Smalltalk References" panel) whose header carries title/count + the union/uncertainty disclaimer (tooltip) and whose rows carry per-row provenance + a receiver-hint badge. Server `smalltalk/crossReference` request builds the result (`providers/crossReference.ts`, pure + 5 unit tests); palette + editor-context menus. (AC2)
- [x] T031 Client `TextDocumentContentProvider` for `smalltalk-cartridge:` — a read-only virtual doc that states the cartridge fact (facts-only, no bundled body) so cartridge rows peek/open honestly instead of erroring.
- [x] T032 E2e (`US-423.acceptance.test.js`) asserts the command's structured result (title + disclaimer + per-row provenance); full e2e suite green (27 passing) incl. AC1/AC3/AC4. Updated US-412 nav e2e for the evolved plural `LocationLink[]` definition shape.

### Slice D — call hierarchy
- [x] T040 `providers/callHierarchy.ts` (incoming = senders via the merge engine, outgoing = the method's body sends via `workspaceXref.sendsFrom`) reusing the engine; `resolveCallTarget` does method-definition + send position resolution (incl. class-side) for `prepareCallHierarchy`; identity round-trips on `CallHierarchyItem.data`. Capability advertised + 3 handlers wired in `server.ts`. (AC4)
- [x] T041 Unit (`server/test/callHierarchy.test.ts`, 8: resolve method/send/class-side, prepare+data, incoming grouping incl. top-level, outgoing grouping) + `test:server` prepare→incoming/outgoing over the real server; e2e stub covers AC4.

## Phase 3 — Verify
- [x] T900 `evals/datasets/references/` added + green — 5/5 cases (union, provenance, ranking, AC6 never-filter), wired into `npm run eval` (now 5 datasets, all green).
- [~] T901 `verification.md` filled; AC→test mapping + code-quality + constitutional all checked. Manual-QA workspace prepared + offline pre-verified (0 diagnostics; engine outputs match the matrix). **Hands-on Extension Host pass + PO sign-off pending** (release gate).
- [ ] T902 CI green on Linux/macOS/Windows (on push/PR).
