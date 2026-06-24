# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding
**Type**: Implementation Verification
**Story**: US-414 — Diagnostics (live parser tier) · **Release**: v0.6.0 (parser-only; gst tier deferred — spec §7)

---

## Section 1: Acceptance Criteria
- [x] Shipped ACs (AC1, AC4) in `tasks.md` checked? (AC2/AC3 gst tier deferred to EPIC-007.)
- [x] Each AC has a passing test? AC1 → unit `diagnostics.test.ts` + e2e + eval + handshake
  `publishDiagnostics`; AC4 → unit `codeAction.test.ts` (10 checks) + e2e (fix applies **and squiggle
  clears**).
- [x] Each user-observable AC's acceptance test was **red before** implementation (AC1: provider module
  missing → MODULE_NOT_FOUND; AC4 written alongside the e2e). TDD honored.

## Section 2: Code Quality
- [x] `npm run lint` passes (eslint client/src + server/src).
- [x] `npm run test:parser` (provider 4 + codeAction 10) · `npm run test:server` (handshake incl.
  diagnostics + codeAction capability) · `npm run test:e2e` · `npm run eval` (completion 8/8,
  diagnostics 6/6) — all green.
- [x] No unjustified `any`; strict `check-types` clean (incl. `noUncheckedIndexedAccess`).
- [x] JSDoc on the public provider APIs.

## Section 3: Constitutional Compliance
- [x] **Native**: standard LSP `publishDiagnostics` + `CodeAction` quick fixes.
- [x] **Zero Config**: parser tier is always-on, no `gst`, no setup.
- [x] **Robustness**: front end never throws (empty publish on failure).
- [x] **TDD**: acceptance tests precede/accompany code; output eval added.

## Section 4: Manual Verification — QA Matrix (run in the Extension Host, then a clean VSIX)
> Per [[manual-qa-before-release]]: automated layers missed real bugs in 0.4.0. A **ready-to-open
> companion workspace** with a fixture for every row lives in `./manual-qa-workspace/` — open that
> folder in the **F5 Extension Development Host** and follow its `README.md` (rows map 1:1 to the table
> below). Then repeat the ✦-marked rows against the **clean VSIX** (`npm run package` → install the
> `.vsix` in a fresh window). Expected messages were verified against the live parser. Record results.

| # | AC | Scenario | Expected | Result |
|---|----|----------|----------|--------|
| 1 ✦ | AC1 | Open `MissingBracket.st` (missing `]`) | Red squiggle within ~250 ms; Problems panel badges it **`smalltalk(parse)`** | ☐ |
| 2 | AC1 | Type into `Clean.st`, pausing mid-token (`total prin`) | No flicker storm; settles ~250 ms after you stop; never blocks typing | ☐ |
| 3 ✦ | AC1 | Apply the row-6 quick fix (or type the `]`) | Squiggle clears on the debounce | ☐ |
| 4 | AC1 | A Warning-severity parser diagnostic (if reproducible) | Shows as a Warning, not an Error (severity as emitted) | ☐ |
| 5 ✦ | AC1 | Close an unsaved malformed file | Its diagnostics disappear (cleared on close) | ☐ |
| 6 ✦ | AC4 | `MissingBracket.st`: Quick Fix (Ctrl/Cmd+.) | **Insert missing "]]"** (class + method both need a `]`); one apply inserts both and **clears** | ☐ |
| 7 | AC4 | `MissingParen.st` (`x := (1 + 2 * 3.`) | **Insert missing ")"**; inserts `)` **before** the `.` and the squiggle clears | ☐ |
| 8 | AC4 | `MissingBrace.st` (`{ 1. 2. 3`) | squiggle on the `{ … }` line (after `3`), **not** the line below; **Insert missing "}"** applies there and clears | ☐ |
| 9 | AC4 | `MissingAttribute.st` (`<category: …`) | squiggle on the pragma line (after `'geometry'`), **not** the body below; **Insert missing ">"** applies there and clears | ☐ |
| 10 | AC4 | `UnterminatedString.st` | **Insert missing "'"**; inserts `'` at end of the open line and clears | ☐ |
| 11 | AC4 | `NoQuickFix.st` (`x := 1 +`) | No quick fix offered (real squiggle, nothing trivial to insert) | ☐ |
| 12 ✦ | — | `Clean.st` | No diagnostics, no quick fixes, no console errors | ☐ |
| 13 ✦ | — | Developer Tools console during the session | No exceptions thrown from the server | ☐ |

- [ ] Matrix executed in the Extension Host (live code).
- [ ] ✦ rows re-verified against the **clean VSIX** in a fresh window.
- [ ] No errors in Developer Tools console.

## Section 5: Sign-Off
- [ ] PO accepts the story (manual-QA matrix passed; eval + CI green on Linux/macOS/Windows + e2e).
- [ ] Ready for Merge / release `v0.6.0`.
