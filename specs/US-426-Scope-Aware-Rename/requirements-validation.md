# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins
**Type**: Requirements Quality Gate
**Story**: US-426 — Scope-aware Rename

---

## Section 1: Constitution Gates (Mandatory)
- [x] **Native Look & Feel**: standard `textDocument/rename` + `prepareRename` (F2, the built-in Rename
  Symbol flow + refactor preview). No bespoke UI.
- [x] **Zero Config**: no settings — rename is always available; safety is structural (scope resolution),
  not a knob. Works out of the box, no `gst`.
- [x] **Protocol First**: pure LSP — `prepareRename` returns a range or a rejection; `rename` returns a
  `WorkspaceEdit`.
- [x] **Robustness**: front end never throws; every unsafe/unresolvable case is a typed **rejection** with a
  reason; collisions/invalid names are refused (no partial edit); never edits kernel/cartridge source.
- [x] **Dialect Agnostic**: resolution is over the layered US-411 symbol table + scope model (temp/arg/ivar
  classification), not GST-lexeme-specific; new dialects reuse it via the same symbol/scope contract.

## Section 2: Specification Completeness
- [x] Goals and Non-Goals explicit (§2/§3 — incl. the hard "no selector rename / no class rename / no kernel
  edit" non-goals).
- [x] User story in standard format (§4).
- [x] Acceptance scenarios (Given/When/Then) defined (§4 — five scenarios).
- [x] Edge cases identified (§4 — pseudo-vars, assignment writes, shadowing, split-class `extend`, dirty
  buffers, global/classvar non-ivars).
- [x] Dependencies listed (§4 — US-411 symbols, US-417 scope resolution, US-412 workspace index).

## Section 3: Technical Design
- [x] API/Command contracts defined (§5 — `prepareRenameAt`, `renameAt`, capability `renameProvider.
  prepareProvider`, `WorkspaceEdit` output).
- [x] Data structures defined (§5 — reuse `documentHighlight` binding-scope walk for temp/arg; new
  `xref/ivarRefs.ts` workspace-wide ivar resolver keyed by (class, ivar), per-method shadow-skipping).
- [x] Error-handling strategy defined (§5/§7 — typed rejections; never throw; collision/identifier
  validation refuses without editing).
- [x] Testing strategy (unit vs integration) defined (§5 + §3.5 routing below).

## Section 3.5: Acceptance Harness (TDD e2e plan)
**AC routing** (user-observable → e2e; internal contract/invariant → unit or `evals/` golden; LSP protocol
shape → handshake):

| AC  | Nature | Routed to |
|-----|--------|-----------|
| AC1 (prepareRename accept/reject matrix) | user-observable + contract | **e2e** (F2 on temp accepts; on selector/kernel rejects) **+ unit** `server/test/rename.test.ts` (full matrix) **+ handshake** (`prepareProvider` advertised) |
| AC2 (temp/arg rename — exact scope, no bleed) | user-observable + invariant | **e2e** (in-buffer rename) **+ property test** `server/test/rename.property.test.ts` (edited set == highlight set; nothing outside scope) |
| AC3 (ivar rename — workspace-wide, shadow-skip) | user-observable + golden | **e2e** (multi-file edit) **+ output eval** `evals/datasets/rename/` (split-class + shadowing goldens) |
| AC4 (new-name validation: collision/identifier) | user-observable + contract | **e2e** (colliding rename refused) **+ unit** (validation matrix) |
| AC5 (no `gst`; no kernel edit; idempotent) | internal invariant | **property test + unit** (idempotence; WorkspaceEdit never references kernel/cartridge URIs) |

- [x] Each AC routed to a layer (table above).
- [x] User-observable ACs (AC1–AC4) pinned by acceptance tests written **before** implementation (red → green)
  in `client/test-e2e/US-426.acceptance.test.js`; the AC5 invariants pinned by a red property test first.
- [x] The story **has** a user-observable surface (F2 rename) — the scaffolded e2e stub is kept and filled.

## Section 4: Validation Result
- [x] **PASS** — Ready for implementation (Plan phase next).

**Notes**: No new ADR required — rename reuses the existing scope model (US-417) and indexing posture
(US-423/ADR-0003); the "no selector/class rename, reject-with-reason" stance is a spec-level decision, not an
architecture change. **Scope/estimate flag:** the owner chose **workspace-wide** ivar rename — this enlarges
the groomed **M** to effectively **L** (the cross-file `ivarRefs` resolver is the bulk); recorded in spec §7.
