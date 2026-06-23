# Tasks: Diagnostics

**ID**: US-414 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Tasks map to acceptance criteria. Decisions locked (Clarify):
parser debounce **250 ms** (dedicated timer), parser severity **as emitted**, gst trigger **save + a
Validate command**, code actions **insert missing `]` and `)`**.

## Phase 1 — Spec & Setup
- [x] T001 Spec reviewed; `requirements-validation.md` gate passed (incl. §3.5 AC routing). PASS.

## Slice A — live parser diagnostics (AC1) ✅
### Acceptance Harness (write RED first)
- [x] T005 Route ACs to layers (done in §3.5): AC1 → e2e + unit + eval.
- [x] T006 RED: e2e in `client/test-e2e/US-414.acceptance.test.js` (2 tests); unit
  `server/test/diagnostics.test.ts`; new `evals/datasets/diagnostics/{cases.json,run.ts}`; handshake
  `publishDiagnostics` assertion (CI coverage — e2e also runs in CI/xvfb per commit 0fd2d18).
- [x] T007 Confirmed RED for the right reason (provider module missing → MODULE_NOT_FOUND).
### Implementation (to GREEN)
- [x] T010 `server/src/providers/diagnostics.ts` — pure `toDiagnostics(LexDiagnostic[])`; `source:
  'smalltalk'`, `code: 'parse'` (renders `smalltalk(parse)`), severity as emitted. (AC1)
- [x] T011 `parseCache.ts` — store `diagnostics` on the entry; add `getDiagnostics(doc)` (no re-parse).
- [x] T012 `server.ts` — dedicated 250 ms diagnostics debounce (own timer map); publish on
  open + change (`onDidChangeContent`); clear (empty publish) + cancel timer on close. (AC1)
- [x] T013 Wired `eval:diagnostics` into `eval`; all four layers green + check-types + lint.

## Slice B — opt-in gst diagnostics (AC2/AC3)
### Acceptance Harness
- [ ] T020 RED: unit `parseGstStderr` fixtures (verified gst-3.2.5 format); injected-spawner
  no-zombie/rapid-edit test; opt-in wiring coverage in the server test where feasible.
### Implementation
- [ ] T021 `server/src/gst/gstRunner.ts` — `parseGstStderr(text,uri)` (pure) + timeout-bounded runner,
  one in-flight child per uri, kill-on-supersede. (AC2/AC3)
- [ ] T022 Factor gst executable resolution for server reuse (setting → PATH), injectable. (AC2)
- [ ] T023 `server.ts` — run gst tier on `onDidSave` when `smalltalk.diagnostics.useGst`; clear stale
  gst diagnostics on next change; union with parser diagnostics per uri. (AC2/AC3)
- [ ] T024 `package.json` — `smalltalk.diagnostics.useGst` (default false) + `smalltalk.validateWithGst`
  command + client→server bridge. (AC2)
- [ ] T025 All three layers + eval green; gst tests hermetic (no gst required in CI).

## Slice C — trivial code actions (AC4)
- [ ] T030 RED: e2e asserts insert-missing-`]`/`)` quick fix; unit for the pure mapping.
- [ ] T031 Advertise `codeActionProvider`; `server/src/providers/codeAction.ts` — quick fixes from the
  matching parse diagnostic. (AC4)
- [ ] T032 All three layers + eval green.

## Phase 4 — Verify & Release (0.6.0)
- [ ] T900 Acceptance tests GREEN; `npm run eval`, `npm run test:e2e`, `test:parser`, `test:server` pass.
- [ ] T901 `verification.md` gate passed (manual-QA matrix: real malformed `.st` + clean VSIX).
- [ ] T902 CI green on Linux/macOS/Windows + e2e.
- [ ] T903 Doc-rot audit; bump version + CHANGELOG; check MARKETPLACE PAT; cut `v0.6.0` Release.
