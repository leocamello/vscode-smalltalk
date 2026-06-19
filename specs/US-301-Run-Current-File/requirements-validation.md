# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins
**Type**: Requirements Quality Gate
**Story**: US-301 — Run Current File

---

## Section 1: Constitution Gates (Mandatory)
- [x] **Native Look & Feel**: command + Command Palette + editor context menu + integrated terminal (standard VS Code APIs).
- [x] **Zero Config**: works if `gst` is on PATH; setting overrides; helpful prompt when missing.
- [x] **Protocol First**: N/A — this is a workflow command, not language intelligence; `gst` stays an optional tool.
- [x] **Robustness**: missing-`gst` path, unsaved file, and paths-with-spaces all handled.
- [x] **Dialect Agnostic**: `when: editorLangId == smalltalk` covers `.st`/`.gst`; interpreter path is user-configurable.

## Section 2: Specification Completeness
- [x] Goals and Non-Goals explicitly listed (§2/§3).
- [x] User story in standard format (§4) with AC1–AC11 from issue #13.
- [x] Acceptance scenarios (Given/When/Then) defined (§4).
- [x] Edge cases identified (no gst, unsaved file, spaces in path, non-Smalltalk editor, terminal reuse).
- [x] Dependencies listed (US-410 client scaffold; `smalltalk.gnuSmalltalkPath` already contributed).

## Section 3: Technical Design
- [x] Command contract defined (`smalltalk.runCurrentFile`, title, `when` clauses, menus).
- [x] Data structures defined (`resolveGst` input/return shape).
- [x] Error handling strategy defined (`showErrorMessage` + "Open Settings").
- [x] Testing strategy defined: pure `gstLocator` + command-string builder unit-tested in Node; terminal/command behaviour via manual F5 (Electron integration deferred, as in US-410).

## Section 4: Validation Result
- [x] PASS - Ready for implementation
