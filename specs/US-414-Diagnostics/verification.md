# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding
**Type**: Implementation Verification
**Story**: US-414 — Diagnostics (parser live; `gst` opt-in) · **Release**: v0.6.0

---

## Section 1: Acceptance Criteria
- [x] All ACs in `tasks.md` are checked (Slices A–C)?
- [x] Each AC has a passing test? AC1 → unit `diagnostics.test.ts` + e2e + eval + handshake; AC2/AC3 →
  unit `gstDiagnostics.test.ts` (stderr fixtures + injected-spawner no-zombie) + local real-gst smoke;
  AC4 → unit `codeAction.test.ts` + e2e.
- [x] Each user-observable AC's acceptance test was **red before** implementation (AC1: provider module
  missing → MODULE_NOT_FOUND; AC4 written alongside the e2e). TDD honored.

## Section 2: Code Quality
- [x] `npm run lint` passes (eslint client/src + server/src).
- [x] `npm run test:parser` (provider 4 + gst 11 + codeAction 5) · `npm run test:server` (handshake
  incl. diagnostics + save sync + codeAction capability) · `npm run test:e2e` (15 passing) ·
  `npm run eval` (completion 8/8, diagnostics 6/6) — all green.
- [x] No unjustified `any`; strict `check-types` clean (incl. `noUncheckedIndexedAccess`).
- [x] JSDoc on the public provider/runner APIs.

## Section 3: Constitutional Compliance
- [x] **Native**: standard LSP `publishDiagnostics` + `CodeAction` quick fixes + a palette command.
- [x] **Zero Config**: parser tier is always-on, no `gst`; gst tier opt-in + auto-resolved; inert when
  `gst` is absent.
- [x] **Robustness**: front end never throws (empty publish on failure); gst bounded by timeout, killed
  on edit/supersede, no zombies; tier silently inert without `gst` (ADR-0001).
- [x] **TDD**: acceptance tests precede/accompany code; output eval added.

## Section 4: Manual Verification — QA Matrix (run in the Extension Host, then a clean VSIX)
> Per [[manual-qa-before-release]]: automated layers missed real bugs in 0.4.0. A **ready-to-open
> companion workspace** with the fixtures for every row lives in `./manual-qa-workspace/` — open that
> folder in the **F5 Extension Development Host** and follow its `README.md` (rows map 1:1 to the table
> below). Then repeat the ✦-marked rows against the **clean VSIX** (`npm run package` → install the
> `.vsix` in a fresh window). The machine has `gst` 3.2.5 at `/usr/local/bin/gst` for the gst-tier rows.
> Expected messages in the table were verified against the live parser **and** real gst. Record results.

| # | AC | Scenario | Expected | Result |
|---|----|----------|----------|--------|
| 1 ✦ | AC1 | Open a `.st` with a missing `]` (e.g. `Object subclass: Foo [\n  bar [ ^1 \n`) | Red squiggle appears within ~250 ms; Problems panel shows it badged **`smalltalk(parse)`** | ☐ |
| 2 | AC1 | Type into a valid file, pausing mid-token (`Transcript sho`) | No flicker storm; squiggle settles ~250 ms after you stop; never blocks typing | ☐ |
| 3 ✦ | AC1 | Fix the malformed file (add the `]`) | Squiggle clears on the debounce | ☐ |
| 4 | AC1 | A Warning-severity parser diagnostic (if reproducible) | Shows as a Warning, not an Error (severity respected as emitted) | ☐ |
| 5 ✦ | AC1 | Close an unsaved malformed file | Its diagnostics disappear (cleared on close) | ☐ |
| 6 | AC2 | With `smalltalk.diagnostics.useGst` **off** (default), save a malformed file | **No** `gst(compile)` diagnostics; only the parser tier | ☐ |
| 7 ✦ | AC2 | Turn `smalltalk.diagnostics.useGst` **on**, save a malformed file | A second diagnostic badged **`gst(compile)`** appears (whole-line) alongside the parser one | ☐ |
| 8 | AC2 | Run **Smalltalk: Validate with gst** from the palette on a malformed file | Saves + runs gst on demand; `gst(compile)` diagnostics appear even if the setting is off | ☐ |
| 9 | AC2 | Point `smalltalk.gnuSmalltalkPath` at a bogus path with the setting on, save | gst tier silently inert (no error dialog); parser tier unaffected | ☐ |
| 10 | AC2 | Setting on but no gst on PATH / not installed | Tier inert; parser tier still works | ☐ |
| 11 | AC3 | With gst on, save then immediately edit the file | Stale `gst(compile)` squiggles clear on the edit; no error against edited text | ☐ |
| 12 | AC3 | Rapid save/edit loop with gst on; then `pgrep -a gst` | No lingering/zombie `gst` processes | ☐ |
| 13 ✦ | AC4 | `MissingBracket.st`: Quick Fix (Ctrl/Cmd+.) | **Insert missing "]]"** (class + method both need a `]`); one apply inserts both and **clears** | ☐ |
| 14 | AC4 | `MissingParen.st` (`x := (1 + 2 * 3.`) | **Insert missing ")"**; inserts `)` **before** the `.` and the squiggle clears | ☐ |
| 15 | AC4 | `MissingBrace.st` (`{ 1. 2. 3`) | squiggle on the `{ … }` line (after `3`), **not** the line below; **Insert missing "}"** applies there and clears | ☐ |
| 16 | AC4 | `MissingAttribute.st` (`<category: …`) | squiggle on the pragma line (after `'geometry'`), **not** the body below; **Insert missing ">"** applies there and clears | ☐ |
| 17 | AC4 | `UnterminatedString.st` | **Insert missing "'"**; inserts `'` at end of the open line and clears | ☐ |
| 18 | AC4 | `NoQuickFix.st` (`x := 1 +`) | No quick fix offered (real squiggle, nothing trivial to insert) | ☐ |
| 19 ✦ | — | Clean valid file across all of the above | No diagnostics, no quick fixes, no console errors | ☐ |
| 20 ✦ | — | Developer Tools console during the session | No exceptions thrown from the server | ☐ |

- [ ] Matrix executed in the Extension Host (live code).
- [ ] ✦ rows re-verified against the **clean VSIX** in a fresh window.
- [ ] No errors in Developer Tools console.

## Section 5: Sign-Off
- [ ] PO accepts the story (manual-QA matrix passed; eval + CI green on Linux/macOS/Windows + e2e).
- [ ] Ready for Merge / release `v0.6.0`.
