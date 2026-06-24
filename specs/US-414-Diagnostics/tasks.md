# Tasks: Diagnostics

**ID**: US-414 | **Spec**: ./spec.md | **Plan**: ./plan.md

Mark each task `[x]` as it lands. Tasks map to acceptance criteria. Decisions locked (Clarify):
parser debounce **250 ms** (dedicated timer), parser severity **as emitted**, code actions **insert a
missing closer (`]`/`)`/`}`/`>`) or close an unterminated string** (expanded from `]`/`)` after manual
QA). **Shipped scope = AC1 + AC4 (parser-only); the opt-in `gst` tier (AC2/AC3) is deferred to
EPIC-007** (scope decision 2026-06-24 — see spec §7).

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

## Slice B — opt-in gst diagnostics (AC2/AC3) — ⛔ DEFERRED to EPIC-007
Built and verified (gstRunner + resolveGst + server wiring + setting/command, no-zombie proven against
real gst), then **removed from 0.6.0** as a scope decision (2026-06-24): gst 3.2.5 emits only syntax
errors the parser already catches better; its real value (semantic compile errors) needs a runtime and
belongs with the **Live Bridge (EPIC-007)**. See spec §7. The implementation lives in git history on
`feature/US-414-diagnostics` (commit `a02518d`) as the seed for the EPIC-007 runtime-diagnostics
provider. Doc trail: ROADMAP / user-stories EPIC-007 backlog.

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
