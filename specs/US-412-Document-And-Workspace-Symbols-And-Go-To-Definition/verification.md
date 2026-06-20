# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding
**Type**: Implementation Verification
**Story**: US-412 — Document/workspace symbols + go-to-definition
**Tester**: Leonardo Nascimento  **Date**: 2026-06-20  **OS / VS Code ver**: Linux (Alienware) / VS Code 1.125.1

> US-412 is the **first user-visible feature**, so this gate is weighted toward **manual QA in the
> Extension Development Host on real code** (Section 4). Automated tests are necessary but not
> sufficient to ship 0.4.0.

---

## Section 1: Acceptance Criteria
- [x] AC1 documentSymbol (brace + chunk) — unit (`providers.test.ts`) + real-server + Electron e2e passing.
- [x] AC2 workspace/symbol (scan, `files.exclude`) — unit + real-server + e2e (incl. runtime `files.exclude`) passing.
- [x] AC3 definition (class ref + all selector implementors) — unit + real-server + e2e passing.
- [x] AC4 debounced version-keyed parse cache — implemented; outline immediate, index debounced 250 ms.

## Section 2: Code Quality
- [x] `npm run check-types` clean (strict).
- [x] `npm run lint` passes.
- [x] `npm run test:parser` + provider unit tests + `npm run test:server` + `npm run test:e2e` (5/5) pass.
- [x] No unjustified `any`; TSDoc on the provider/cache public surface.

## Section 3: Constitutional Compliance
- [x] **Native**: standard Outline/breadcrumbs/`Ctrl+T`/`F12`; no bespoke UI.
- [x] **Zero Config**: works on first open (activates from `workspaceContains`); no settings.
- [x] **Robustness**: malformed file → partial outline, never throws (M10).
- [x] **TDD**: provider unit + real-server LSP + Electron e2e tests authored with the change.

## Section 4: Manual Verification — Extension Host matrix (the 0.4.0 gate)
Launch with **F5** (Extension Development Host). Record **Pass/Fail + a note** per row; attach a
screenshot for the headline rows (M1, M2, M4). Keep the Developer Tools console open (Help → Toggle
Developer Tools) and confirm **no errors/exceptions** throughout.

| ID | Area | Steps | Expected | Result | Notes |
|----|------|-------|----------|--------|-------|
| M1 | **Real-corpus outline** | Open `../smalltalk-3.2.5/kernel/` as the workspace; open `Array.st`, then open the Outline view. | Class `Array` with its methods nested (selectors + arity); instance variables shown; entries clickable → cursor jumps. | ✅ Pass | |
| M2 | **Chunk-format outline** | Open a chunk file (`test-cases/05_*` or `09_*`); open Outline. | Methods grouped under their class; no errors. | ✅ Pass | |
| M3 | **Brace-format outline detail** | Open `test-cases/11`/`12`/`13`; check Outline. | Namespaces → classes → instance/class methods + ivars/cvars; class-side methods marked. | ✅ Pass | |
| M4 | **Workspace symbol — class** | `Ctrl/Cmd+T`, type a class name (e.g. `Array`). | Class appears (possibly several files); selecting opens the definition site. | ✅ Pass | |
| M5 | **Workspace symbol — selector** | `Ctrl/Cmd+T`, type a common selector (e.g. `printOn:`). | Multiple implementors across files; each opens correctly. | ✅ Pass | |
| M6 | **`files.exclude` honored** | Add a pattern (e.g. `**/Array.st`) to `files.exclude`; repeat M4/M5. | Excluded files do not appear in results. | ✅ Pass (after #39) | **FAIL on first pass** — exclude applied only at startup; runtime change had no effect. **Fixed** (#39: server registers for `didChangeConfiguration` and re-indexes; regression e2e added). Re-tested: excluding `**/Array.st` removes it from `Ctrl+T`, re-including restores it. |
| M7 | **Go-to-def — class ref** | In a kernel file, put the cursor on a superclass/class name; `F12` and `Ctrl/Cmd+Click`. | Jumps to the class definition; if multiple, a peek list. | ✅ Pass | |
| M8 | **Go-to-def — message send** | Cursor on a unary and a keyword message send; `F12`. | All implementor candidates listed (peek when >1); same-file first. | ✅ Pass | |
| M9 | **Live editing / debounce** | Add a new method to an open class; wait ~½s; re-open Outline / re-query `Ctrl+T`. | New symbol appears within ~300 ms; rapid typing does not spike CPU or flash errors. | ✅ Pass | |
| M10 | **Robustness — malformed** | Delete a `]`/`!`, leave the file half-typed. | Outline still shows a usable partial result; no crash; console clean. | ✅ Pass | |
| M11 | **Large file responsiveness** | Open the largest kernel file; scroll, query, go-to-def. | Outline/results appear promptly (sub-second); editor stays responsive. | ✅ Pass | |
| M12 | **No `gst` / zero-config** | Ensure `gst` is not on `PATH` and no settings set; repeat M1, M4, M7. | Everything works unchanged; no error notifications. | ✅ Pass | Verified by construction: navigation has **zero** `gst`/`child_process` dependency (grep); only Run Current File (US-301) uses `gst`. |
| M13 | **Cross-platform sanity** | On the dev OS, confirm navigation uses correct file URIs (no `\\`/`/` glitches). | Locations open the right file:line on this OS. | ✅ Pass | Covered by M7/M8 on Linux (jumps land on the right file:line); URIs built with `pathToFileURL`; CI green on macOS/Windows. |
| M14 | **Clean-install VSIX smoke** | `npm run package`; install the `.vsix` in a clean VS Code (no dev host); repeat M1 + M7. | Shipped artifact behaves like the dev build. | ✅ Pass | Installed `vscode-smalltalk-*.vsix` into normal VS Code, opened the kernel folder, outline + go-to-def worked; clean install/uninstall. (The `DEP0169 url.parse` warning is from the VS Code CLI, not the extension.) |

- [x] Developer Tools console shows **no errors/exceptions** across the matrix.
- [x] All headline rows (M1, M2, M4, M5, M7, M8) **Pass**.

**Waivers / follow-ups:**
- **Two bugs were found by this gate and fixed** before sign-off:
  - **#39** — `files.exclude` was applied only at startup (M6). Fixed: re-index on `didChangeConfiguration`.
  - **#40** — workspace symbol search returned nothing on a fresh window until a `.st` file was opened (the extension only activated `onLanguage`). Fixed: `activationEvents: workspaceContains:**/*.{st,gst}`.
  Both have regression e2e tests.
- Both findings are the value of the manual gate — the automated layers missed them because a file was always open.

## Section 5: Sign-Off
- [x] Automated gates green (Sections 1–3) **and** the manual matrix (Section 4) complete.
- [x] Ready for Merge — all slices (#36–#40) merged to `master`.
- [x] **0.4.0 release readiness:** US-412 merged, manual matrix passed on a clean VSIX, `MARKETPLACE`
  PAT current (0.3.0 published 2026-06-19). Remaining: bump version + CHANGELOG, then cut the `v0.4.0` tag.
