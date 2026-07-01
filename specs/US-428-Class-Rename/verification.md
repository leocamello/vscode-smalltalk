# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding
**Type**: Implementation Verification
**Story**: US-428 — Class Rename

---

## Section 1: Acceptance Criteria
- [x] All ACs in `tasks.md` are checked (T010–T012 implemented; T900 green).
- [x] Each AC has a passing test:
  - **AC1** (accept workspace class / reject kernel class / reject non-class) — `classRename.test.ts` prepare matrix + handshake (`Greeter` ✓ / `Object` ✗) + e2e AC1.
  - **AC2** (rewrite every resolved form, nothing else) — `classRename.test.ts` per-form + eval `rename` multi-file/binding/qualified goldens + property no-bleed + e2e AC2.
  - **AC3** (resolution-gating; skip locals + unrelated namespace) — `classRename.test.ts` (unrelated-namespace untouched, shadow-skip) + eval shadow case.
  - **AC4** (kernel + workspace collision; non-class identifier) — `classRename.test.ts` + eval kernel-collision + e2e AC4.
  - **AC5** (no gst; no kernel edit; idempotent) — property round-trip/idempotence + `classRename.test.ts` round-trip; edits are pure `WorkspaceEdit`, never a cartridge URI.
  - **AC6** (multi-file → Refactor Preview) — reused US-426 `withMultiFileConfirmation`; handshake returns a `WorkspaceEdit`.
- [x] Each user-observable AC's acceptance test was **red before** implementation — confirmed RED via `Cannot find module '../src/xref/classRefs.ts'` (T007).

## Section 2: Code Quality
- [x] `npm run lint` passes.
- [x] `npm run test:parser` (classRename 16, rename 16, property 15), `npm run test:server` (handshake clean), `npm run eval` (rename 10/10; all evals green), `npm run test:e2e` (41/41, US-428 3/3) pass.
- [x] `npm run check-types` clean (strict). No unjustified `any` (one narrow `as Token` cast for a symbol sub-range, commented).
- [x] JSDoc on the public resolver/provider APIs (`classOccurrences`, `ClassWorld`, `buildClassWorldFromFiles`, `renameKindAt`).

## Section 3: Constitutional Compliance
- [x] **Native**: standard `textDocument/rename` + `prepareRename` (F2 + Refactor Preview); no bespoke UI.
- [x] **Zero Config**: no settings; class rename is always available; safety is structural (resolution-gating + kernel boundary). Offline, no `gst`.
- [x] **Robustness**: front end never throws; kernel/selector/non-class/collision are typed rejections; an unresolved qualified path is skipped, not over-rewritten; never edits kernel/cartridge source.
- [x] **TDD**: RED acceptance harness written before code across all five layers (unit/property/eval/handshake/e2e).

## Section 4: Manual Verification
- [ ] Feature works in the Extension Host — run `specs/US-428-Class-Rename/manual-qa-workspace/` Parts A–D (open the folder; rename `Shape`→`Polygon` across all four files via the forced Refactor Preview; confirm the shadow local + unrelated namespace stay put; F2 on `OrderedCollection` rejected; rename into `OrderedCollection`/`Circle`/`polygon` refused; round-trip back to `Shape` clean). **(Owner-run — pending.)**
- [ ] No errors in the Developer Tools console during the matrix. **(Owner-run — pending.)**

## Section 5: Sign-Off
- [ ] Ready for Merge — **pending owner manual-QA sign-off** (Section 4) + CI green on Linux/macOS/Windows (PR closing #109). Automated layers (unit/property/eval/handshake/e2e) + lint + types are all green locally.

### Manual-QA matrix
See `manual-qa-workspace/README.md` — Part A (all reference forms + forced multi-file preview), Part B
(shadowing safety), Part C (kernel boundary + collisions rejected with a reason), Part D (idempotence).
