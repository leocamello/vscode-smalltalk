# Implementation Verification — US-430: Dialect Cartridge + Console loader + convergence

**Purpose**: Verify the implementation AFTER coding, and gate the merge of the cartridge convergence.
**Type**: Implementation Verification + Manual-QA gate
**Story**: US-430 · **Spec**: ./spec.md · **ADRs**: [0002](../../docs/decisions/0002-kernel-symbol-sourcing.md), [0003](../../docs/decisions/0003-cartridge-resolution.md)

> This is a **behaviour-preserving foundation** (EPIC-005), not a user-facing feature. The hard gate is
> that completion is **unchanged** after the cartridge swap, with two intended surface changes: the
> status-bar label (**reference** vs installed) and the completion-item provenance detail
> (`kernel (reference)` / `kernel (installed)`). Per PO direction (memory *manual-qa-before-release*),
> execute the §3 matrix in the Extension Development Host — favouring **real code** and a **clean-install
> VSIX** smoke — and sign off below before merging #64.

---

## Section 1: Acceptance Criteria → evidence (automated)

| AC | What | Evidence | Status |
|---|---|---|---|
| AC1 | `knowledge-base.ts` defines `DialectCartridge` (pure JSON, facts only, class/instance split, structural resolution vs `taxonomy`) | schema in `server/src/types/knowledge-base.ts`; `cartridgeLoader.test.ts` structural validation | ✅ |
| AC2 | Cartridge round-trips through `JSON.parse` (no functions/cycles) + structural validation | `cartridgeLoader.test.ts` (round-trip; inlined copy ≡ committed file) | ✅ |
| AC3 | `export-gst-cartridge.st` runs headlessly → cartridge (classes + `crossReference`) | committed `gst-3.2.5-cartridge.json` (249 classes / 4746 signatures) | ✅ |
| AC4 | Facts-only (`carriesProse: false`); a test asserts no prose fields | `cartridgeLoader.test.ts` no-prose licensing gate (class- + method-level) | ✅ |
| AC5 | Deterministic output (sorted keys); `contentHash` stamped by the TS build step | `cartridgeHash.ts` + `stamp-cartridge.ts`; `sha256-2480f898…`; determinism guard test | ✅ |
| AC6 | Exporter is the documented template for image-export adapters | `scripts/export-gst-cartridge.st` (heavily commented) | ✅ |
| AC7 | Convergence: loader + projection drive completion; installed adapter emits cartridge shape; Tier-2 floor; `kernel-index.json` + `gen-kernel-index.ts` retired | `cartridgeLoader.ts`, `indexer.ts` (`indexKernelDirectoryToCartridge`), `kernelIndexService.ts`; eval 8/8 over the cartridge; old index deleted | ✅ (status/label: manual §3 rows 4/5) |

## Section 2: Code Quality & Automated Gates

- [x] `npm run check-types` (strict) passes.
- [x] `npm run lint` passes.
- [x] `npm run test:parser` passes (cartridgeLoader **11** + kernelService **12** + providers 23 + rest).
- [x] `npm run test:server` passes (real `textDocument/completion`; detail still carries `/kernel/`).
- [x] `npm run test:e2e` passes — **12/12** (on esbuild 0.25.12).
- [x] `npm run eval` passes — grammar + **completion eval 8/8** (now sourced from the 249-class cartridge floor).
- [x] `npm run package` smoke OK — VSIX `vscode-smalltalk-0.5.0.vsix` ships `dist/server.js` (1.31 MB) with the **inlined, contentHash-stamped** cartridge.
- [x] No unjustified `any`; public APIs carry JSDoc (loader/hash/indexer/service).
- [x] CI green on **Linux / macOS / Windows** (PR #82, run 28054294202 — all three `build` jobs pass; includes the Dependabot deps bump).

## Section 3: Manual Verification Matrix (Extension Dev Host)

Run with **F5** (dev build) and again from a **clean-install VSIX**. This box has `gst` at
`/usr/local/bin/gst` (kernel `/usr/local/share/smalltalk/kernel`), so the **installed** path is reachable;
force the **floor** with `kernelLibrary=bundled` or a bogus `kernelPath`. Record result + a short note.

| # | Area | Steps | Expected | Result |
|---|---|---|---|---|
| 1 | Completion unchanged — selectors | Open a `.st` file; type `x print` and trigger completion | Kernel selectors (`printString`, `printNl`, …) appear after the receiver — same as 0.5.0 | ✅ Pass |
| 2 | Completion unchanged — class + variable | Inside a method with a temp + instance var, complete in head position | In-scope variables first, then class names (`OrderedCollection` via `OC`); correct icons | ✅ Pass |
| 3 | Keyword snippet | Accept `at:put:` | Inserts `at:${1} put:${2}`; Tab jumps between the two argument stops | ✅ Pass |
| 4 | **Status label — floor (changed)** | `kernelLibrary=bundled` (or `auto` with no `gst`), reload | Status bar reads **reference (gst 3.2.5)** (was "bundled (gst 3.2.5)") | ✅ Pass |
| 5 | **Status label — installed** | `kernelLibrary=auto` with `gst` discoverable (this box), reload | Status bar reads **installed (gst)**; completions match the installed kernel | ✅ Pass |
| 6 | **Provenance detail (changed)** | Inspect a floor completion's detail, then an installed one | Detail reads **`kernel (reference)`** for the floor / **`kernel (installed)`** for installed; status bar agrees | ✅ Pass |
| 7 | Fallback notice | `kernelLibrary=auto`, no discoverable `gst` (bogus `kernelPath`) | One-time notice: kernel completions use a bundled reference (GST 3.2.5), with *Open Settings* | ✅ Pass |
| 8 | `off` | `smalltalk.completion.kernelLibrary=off` | Kernel completions disappear; only workspace symbols remain; status bar reads **off** | ✅ Pass |
| 9 | Floor breadth sanity | On the floor, complete a class name in head position | Full base image offered (incl. `SystemExceptions`-area classes) with no obvious noise/dupes — broader than the old kernel-dir index, as designed | ✅ Pass |
| 10 | Robustness / live edit | Type into a half-written/malformed method; add a new method then complete | Useful (partial) completions, never a thrown error; new symbols appear within ~debounce | ✅ Pass |
| 11 | No-`gst` & zero-config | Fresh defaults (no settings; simulate no `gst`) | Useful kernel completions out of the box from the frozen floor | ✅ Pass |
| 12 | Clean-install VSIX | `npm run package` → install in clean VS Code → repeat #1, #3, #4 | Shipped artifact behaves like the dev build; status label is **reference (gst 3.2.5)** | ✅ Pass |

**Developer Tools console:** ✅ no errors emitted by *this* extension during the above.

_Manual matrix executed 2026-06-23 (dev host on the manual-qa-workspace + clean-install VSIX); all 12 rows pass, including the renamed floor label `reference (gst 3.2.5)` and the `kernel (reference)` provenance detail. Signed off by the PO (Leonardo Nascimento)._

## Section 4: Constitutional Compliance

- [x] **Native**: standard completion widget, snippets, status-bar item — no custom UI.
- [x] **Zero Config**: `auto` default gives useful completion from the frozen floor with no settings and no `gst`.
- [x] **Robustness**: never-throws front end; loader/projection pure + total; service degrades installed → floor → empty.
- [x] **Dialect Agnostic**: dialect-neutral cartridge schema + source-adapter seam (reflective export template + static GST parse) — ADR-0002/0003.

## Section 5: Sign-Off (merge gate)

- [x] §3 manual matrix executed (dev host **and** clean VSIX) and notes recorded.
- [x] CI green on Linux / macOS / Windows for the final commit (PR #82).
- [x] Doc-rot audit done (CLAUDE.md, user-stories, spec/tasks) — landed in Slice D.
- [x] PO accepts US-430 (DoD met) → merge `feature/US-430-console-loader` → `master`, closing #64.
