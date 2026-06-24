# Tasks: Diagnostics

**ID**: US-414 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Tasks map to acceptance criteria. Decisions locked (Clarify):
parser debounce **250 ms** (dedicated timer), parser severity **as emitted**, gst trigger **save + a
Validate command**, code actions **insert a missing closer (`]`/`)`/`}`/`>`) or close an unterminated
string** (expanded from `]`/`)` after manual QA).

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

## Slice B — opt-in gst diagnostics (AC2/AC3) ✅
### Acceptance Harness
- [x] T020 Unit `server/test/gstDiagnostics.test.ts` (11 checks): `parseGstStderr` fixtures (verified
  gst-3.2.5 format), injected-spawner no-zombie/kill-on-supersede + spawn-error-inert + cancel,
  `resolveGst` cases. All CI-safe (no real gst). AC2/AC3 routed here + local manual QA (§3.5).
### Implementation
- [x] T021 `server/src/gst/gstRunner.ts` — `parseGstStderr(stderr, sourceText)` (pure, whole-line
  ranges, source `gst`/code `compile`) + `GstDiagnosticsRunner`: timeout-bounded, one in-flight child
  per uri, kill-on-supersede, inert on spawn error. (AC2/AC3)
- [x] T022 `server/src/gst/resolveGst.ts` — server-side gst resolution (setting → PATH), injectable.
- [x] T023 `server.ts` — gst tier on `onDidSave` when `useGst`; kill-on-edit + clear stale gst diags on
  change; union with parser diagnostics per uri; clear all gst diags when the setting goes off. (AC2/AC3)
- [x] T024 `package.json` — `smalltalk.diagnostics.useGst` (default false) + `smalltalk.validateWithGst`
  command + palette entry; client bridge (save active doc → `smalltalk/validateWithGst` notification).
  `textDocumentSync` → object form with `save` so the client sends `didSave`. (AC2)
- [x] T025 All four layers green (parser/server/e2e/eval) + check-types + lint; gst tests hermetic.
  Local smoke against real gst (resolve→spawn→parse + rapid-supersede no-zombie) verified.

## Slice C — trivial code actions (AC4) ✅
- [x] T030 e2e asserts the `]` quick fix applies **and the squiggle clears** (`client/test-e2e/US-414`);
  unit `server/test/codeAction.test.ts` (8 checks): each closer `]`/`)`/`}`/`>`, the close-string fix,
  "applying re-parses clean", a non-fixable parse error, and non-parser filtering.
- [x] T031 `codeActionProvider: { codeActionKinds: [QuickFix] }`; `server/src/providers/codeAction.ts` —
  `toCodeActions(uri, diagnostics, text)` inserts a missing closer (anchored `^Expected "([\])}>])"`)
  at the diagnostic range **start** (before the offending token — manual-QA fix), grouping multiple
  same-spot closers into one action (`]]`); and inserts a closing `'` for an unterminated string at the
  end of its opening line. Wired `connection.onCodeAction` (passes the doc text). (AC4)
- [x] T032 All four layers + eval green; handshake asserts `codeActionProvider`. Manual-QA workspace
  extended: `MissingBrace`/`MissingAttribute`/`NoQuickFix` fixtures + README/verification rows.

## Phase 4 — Verify & Release (0.6.0)
- [ ] T900 Acceptance tests GREEN; `npm run eval`, `npm run test:e2e`, `test:parser`, `test:server` pass.
- [ ] T901 `verification.md` gate passed (manual-QA matrix: real malformed `.st` + clean VSIX).
- [ ] T902 CI green on Linux/macOS/Windows + e2e.
- [ ] T903 Doc-rot audit; bump version + CHANGELOG; check MARKETPLACE PAT; cut `v0.6.0` Release.
