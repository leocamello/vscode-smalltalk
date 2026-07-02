# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins
**Type**: Requirements Quality Gate
**Story**: US-428 — Class Rename

---

## Section 1: Constitution Gates (Mandatory)
- [x] **Native Look & Feel**: standard `textDocument/rename` + `prepareRename` (F2 Rename Symbol + Refactor
  Preview). No bespoke UI; class rename reuses the exact flow US-426 established.
- [x] **Zero Config**: no settings — class rename is always available; safety is structural (resolution-gating +
  kernel boundary), not a knob. Works out of the box, no `gst`.
- [x] **Protocol First**: pure LSP — `prepareRename` returns a range (class segment) or a rejection; `rename`
  returns a `WorkspaceEdit` (multi-file, change-annotated).
- [x] **Robustness**: front end never throws; a kernel class / selector / non-class / collision is a typed
  **rejection** with a reason; a qualified path that doesn't resolve is **skipped** (never over-rewritten);
  never edits kernel/cartridge source.
- [x] **Dialect Agnostic**: resolution is over the US-411 AST + the US-412 index namespaces + the US-422
  workspace ∪ cartridge class world — not a GST-lexeme hack; a new dialect reuses the same class-world/index
  contract.

## Section 2: Specification Completeness
- [x] Goals and Non-Goals explicit (§2/§3 — incl. "no selector rename / no kernel-class rename / no file
  rename / no arbitrary-symbol rewrite / no full namespace modeling").
- [x] User story in standard format (§4).
- [x] Acceptance scenarios (Given/When/Then) defined (§4 — six scenarios spanning every reference form + the
  kernel/collision/namespace cases).
- [x] Edge cases identified (§4 — symbol variants `#'Foo'`/bare, split-class `extend`, `class`-side receiver,
  `Smalltalk` default namespace, dirty buffers, unknown-global reject, `#Foo`-as-data).
- [x] Dependencies listed (§4 — US-426 rename engine, US-412 index+namespaces, US-422 class world, US-411 AST).

## Section 3: Technical Design
- [x] API/Command contracts defined (§5 — extend `prepareRenameAt`/`renameAt` `classify` with a `class` kind;
  `WorkspaceEdit` output; `withMultiFileConfirmation` reused).
- [x] Data structures defined (§5 — new `xref/classRefs.ts` resolver `Map<uri, Ranged[]>`; a `ClassWorld`
  resolution context (`isKnownWorkspaceClass`/`isKernelClass`/`resolveQualified`) built in `server.ts` from
  index ∪ cartridge; token-level segment ranges for qualified/symbol forms).
- [x] Error-handling strategy defined (§5/§7 — typed rejections; never throw; kernel/collision refused without
  editing; unresolved qualified path skipped, not rewritten).
- [x] Testing strategy (unit vs integration) defined (§5 + §3.5 routing below).

## Section 3.5: Acceptance Harness (TDD e2e plan)
**AC routing** (user-observable → e2e; internal contract/invariant → unit or `evals/` golden; LSP protocol
shape → handshake):

| AC  | Nature | Routed to |
|-----|--------|-----------|
| AC1 (prepareRename: workspace class ✓, kernel class ✗, non-class ✗; class-segment range) | user-observable + contract | **e2e** (F2 on workspace class accepts; on kernel class rejects) **+ unit** `server/test/classRename.test.ts` (accept/reject matrix) **+ handshake** (accepts a class, rejects a kernel class) |
| AC2 (rewrite every resolved form, nothing else) | user-observable + invariant | **e2e** (multi-file, multi-form in-buffer rename) **+ output eval** `evals/datasets/rename/` (per-form goldens: decl symbol, receiver, superclass, `class`/`extend`, `#{…}`, `A.B`/`A::B`) **+ property test** (edited ⊆ resolved refs; no comment/string bleed) |
| AC3 (resolution-gating; skip locals + unrelated namespace) | contract + invariant | **unit** (unrelated-namespace untouched; local sharing the name skipped) **+ property test** (no bleed into locals/other-namespace) |
| AC4 (new-name validation: kernel + workspace collision) | user-observable + contract | **e2e** (colliding rename refused) **+ unit** (validation matrix: kernel collision, workspace collision, non-class identifier) |
| AC5 (no `gst`; no kernel edit; idempotent) | internal invariant | **property test + unit** (idempotence; round-trip; `WorkspaceEdit` never references kernel/cartridge URIs) |
| AC6 (multi-file → Refactor Preview) | user-observable + contract | **e2e** (multi-file edit gets change annotations) **+ unit** (`withMultiFileConfirmation` marks >1-file class edits) |

- [x] Each AC routed to a layer (table above).
- [x] User-observable ACs (AC1/AC2/AC4/AC6) pinned by acceptance tests written **before** implementation
  (red → green) in `client/test-e2e/US-428.acceptance.test.js`; the AC3/AC5 invariants pinned by a red
  property/unit test first.
- [x] The story **has** a user-observable surface (F2 class rename) — the scaffolded e2e stub is kept and filled.

## Section 4: Validation Result
- [x] **PASS** — Ready for implementation (Plan phase next).

**Notes**: No new ADR required — class rename extends the US-426 engine and reuses the US-422 class-world +
US-412 namespace posture; the "all forms incl. namespaced, resolution-gated, reject-with-reason on the kernel
boundary" stances are spec-level decisions (from the Clarify phase), not an architecture change. **Scope note:**
the workspace-wide **all-forms** coverage (owner's choice) makes this an **M→L** like US-426 — the new
`classRefs` resolver + token-level segment ranges for qualified/symbol forms are the bulk; recorded in spec §7.
**Roadmap resequence to record in the doc sweep:** 0.12 = Class Rename (this), 0.13 = Hardening/Perf (US-901) —
the ROADMAP currently pencils 0.12 = Hardening.
