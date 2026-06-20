# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding
**Type**: Implementation Verification
**Story**: US-412 — Document/workspace symbols + go-to-definition
**Tester**: _________________  **Date**: __________  **OS / VS Code ver**: __________

> US-412 is the **first user-visible feature**, so this gate is weighted toward **manual QA in the
> Extension Development Host on real code** (Section 4). Automated tests are necessary but not
> sufficient to ship 0.4.0.

---

## Section 1: Acceptance Criteria
- [ ] AC1 documentSymbol (brace + chunk) — automated unit + integration test passing.
- [ ] AC2 workspace/symbol (scan, `files.exclude`) — automated test passing.
- [ ] AC3 definition (class ref + all selector implementors) — automated test passing.
- [ ] AC4 debounced version-keyed parse cache — automated test passing.

## Section 2: Code Quality
- [ ] `npm run check-types` clean (strict).
- [ ] `npm run lint` passes.
- [ ] `npm run test:parser` + new provider unit tests + `npm test` (integration) pass.
- [ ] No unjustified `any`; TSDoc on the provider/cache public surface.

## Section 3: Constitutional Compliance
- [ ] **Native**: standard Outline/breadcrumbs/`Ctrl+T`/`F12`; no bespoke UI.
- [ ] **Zero Config**: works on first open, no settings.
- [ ] **Robustness**: malformed file → partial outline, never throws.
- [ ] **TDD**: provider unit + integration tests authored with the change.

## Section 4: Manual Verification — Extension Host matrix (the 0.4.0 gate)
Launch with **F5** (Extension Development Host). Record **Pass/Fail + a note** per row; attach a
screenshot for the headline rows (M1, M2, M4). Keep the Developer Tools console open (Help → Toggle
Developer Tools) and confirm **no errors/exceptions** throughout.

| ID | Area | Steps | Expected | Result | Notes |
|----|------|-------|----------|--------|-------|
| M1 | **Real-corpus outline** | Open `../smalltalk-3.2.5/kernel/` as the workspace; open `Array.st`, then open the Outline view. | Class `Array` with its methods nested (selectors + arity); instance variables shown; entries clickable → cursor jumps. | ☐ | |
| M2 | **Chunk-format outline** | Open a chunk file (`test-cases/05_*` or `09_*`); open Outline. | Methods grouped under their class; no errors. | ☐ | |
| M3 | **Brace-format outline detail** | Open `test-cases/11`/`12`/`13`; check Outline. | Namespaces → classes → instance/class methods + ivars/cvars; class-side methods marked. | ☐ | |
| M4 | **Workspace symbol — class** | `Ctrl/Cmd+T`, type a class name (e.g. `Array`). | Class appears (possibly several files); selecting opens the definition site. | ☐ | |
| M5 | **Workspace symbol — selector** | `Ctrl/Cmd+T`, type a common selector (e.g. `printOn:`). | Multiple implementors across files; each opens correctly. | ☐ | |
| M6 | **`files.exclude` honored** | Add a folder to `files.exclude`; repeat M4/M5. | Excluded files do not appear in results. | ☐ | |
| M7 | **Go-to-def — class ref** | In a kernel file, put the cursor on a superclass/class name; `F12` and `Ctrl/Cmd+Click`. | Jumps to the class definition; if multiple, a peek list. | ☐ | |
| M8 | **Go-to-def — message send** | Cursor on a unary and a keyword message send; `F12`. | All implementor candidates listed (peek when >1); same-file first. | ☐ | |
| M9 | **Live editing / debounce** | Add a new method to an open class; wait ~½s; re-open Outline / re-query `Ctrl+T`. | New symbol appears within ~300 ms; rapid typing does not spike CPU or flash errors. | ☐ | |
| M10 | **Robustness — malformed** | Delete a `]`/`!`, leave the file half-typed. | Outline still shows a usable partial result; no crash; console clean. | ☐ | |
| M11 | **Large file responsiveness** | Open the largest kernel file; scroll, query, go-to-def. | Outline/results appear promptly (sub-second); editor stays responsive. | ☐ | |
| M12 | **No `gst` / zero-config** | Ensure `gst` is not on `PATH` and no settings set; repeat M1, M4, M7. | Everything works unchanged; no error notifications. | ☐ | |
| M13 | **Cross-platform sanity** | On the dev OS, confirm navigation uses correct file URIs (no `\\`/`/` glitches). | Locations open the right file:line on this OS. | ☐ | |
| M14 | **Clean-install VSIX smoke** | `npm run package`; install the `.vsix` in a clean VS Code (no dev host); repeat M1 + M7. | Shipped artifact behaves like the dev build. | ☐ | |

- [ ] Developer Tools console shows **no errors/exceptions** across the matrix.
- [ ] All headline rows (M1, M2, M4, M5, M7, M8) **Pass**; any failing row is fixed or explicitly waived with a reason below.

**Waivers / follow-ups:**
- …

## Section 5: Sign-Off
- [ ] Automated gates green (Sections 1–3) **and** the manual matrix (Section 4) complete.
- [ ] Ready for Merge.
- [ ] **0.4.0 release readiness:** with US-412 merged, the manual matrix passed on a clean VSIX, and
  the `MARKETPLACE` PAT confirmed valid, the release can be cut (version + CHANGELOG → `v0.4.0` tag).
