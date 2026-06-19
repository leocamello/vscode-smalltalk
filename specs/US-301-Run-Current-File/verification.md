# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding
**Type**: Implementation Verification
**Story**: US-301 — Run Current File

---

## Section 1: Acceptance Criteria
- [x] AC1–AC6, AC11 covered by code + the 6 `gstLocator` unit tests (resolution precedence, not-found, Windows names, quoting) and the package.json contributions.
- [~] AC7–AC10 (error toast + "Open Settings"; terminal run + stdout/stderr): implemented; **owner-confirmed via F5** (see §4).

## Section 2: Code Quality
- [x] `npm run lint` passes.
- [x] `npm run test:client` (6) + `npm run test:server` + `npm run eval` pass; `check-types` clean.
- [x] No `any` types.
- [x] Public functions documented (TSDoc on `resolveGst`/`buildRunCommand`/`runCurrentFile`).

## Section 3: Constitutional Compliance
- [x] **Native**: command + palette + context menu + integrated terminal.
- [x] **Zero Config**: runs if `gst` is on PATH; setting overrides; helpful prompt when missing.
- [x] **Robustness**: missing-`gst`, unsaved file, spaces-in-path, non-Smalltalk editor handled.
- [x] **TDD**: pure logic unit-tested before wiring the VS Code glue.

## Section 4: Manual Verification
- [ ] F5: with `gst` available, run a `.st` file → "Smalltalk" terminal shows `gst "<file>"` output.
- [ ] F5: with no `gst` → error toast with "Open Settings" opens `smalltalk.gnuSmalltalkPath`.
- [ ] F5: a path containing spaces runs correctly.
- [ ] No errors in Developer Tools console.

## Section 5: Sign-Off
- [ ] Ready for Merge? *(pending CI green on 3 OSes + owner F5 confirmation)*
