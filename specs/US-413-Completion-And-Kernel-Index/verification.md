# Implementation Verification — US-413: Completion + Kernel Index

**Purpose**: Verify the implementation AFTER coding, and gate the **0.5.0** release.
**Type**: Implementation Verification + Manual-QA release gate
**Story**: US-413 · **Spec**: ./spec.md · **ADR**: [0002](../../docs/decisions/0002-kernel-symbol-sourcing.md)

> Per PO direction (see memory *manual-qa-before-release*), 0.5.0 does **not** ship on green CI alone:
> the §3 manual matrix must be executed in the Extension Development Host — favouring **real code**
> (open `../smalltalk-3.2.5/kernel/`) and a **clean-install VSIX** smoke — and signed off below.

---

## Section 1: Acceptance Criteria → evidence (automated)

| AC | What | Evidence | Status |
|---|---|---|---|
| AC1 | Build-time generator → `server/data/kernel-index.json` (facts only) | `kernelIndex.test.ts` (invariants + **licensing no-prose gate** + drift guard); 250 classes / 4723 selectors | ✅ |
| AC2 | Selector completion after a receiver; ranking + prefix/camel-hump | `providers.test.ts`, `eval` cases, `handshake`/e2e | ✅ |
| AC3 | Class names in head position + in-scope variables | `providers.test.ts`, `eval`, e2e | ✅ |
| AC4 | Multi-part keyword selectors insert as snippets | `providers.test.ts`, `eval` (`at:put:` → `at:${1} put:${2}`), e2e | ✅ |
| AC5 | `kernelLibrary` (`auto\|bundled\|off`) + `kernelPath` | `kernelService.test.ts`, e2e (`off` suppresses / `bundled` restores) | ✅ |
| AC6 | Live-index an installed kernel when discoverable, else bundle | `kernelService.test.ts` (discovery + auto-installed + fallback) | ✅ |
| AC7 | Item provenance + status-bar identity + one-time fallback notice | item `detail`/`sortText` (slice C); status bar + notice (slice D, manual §3 rows 6/8) | ✅ (status: manual) |

## Section 2: Code Quality & Automated Gates

- [x] `npm run check-types` (strict) passes.
- [x] `npm run lint` passes.
- [x] `npm run test:parser` passes (kernel smoke + kernelIndex 6 + kernelService 10 + providers 23).
- [x] `npm run test:server` passes (real `textDocument/completion`).
- [x] `npm run test:e2e` passes — **12/12** (3 completion + 2 setting rows added).
- [x] `npm run eval` passes — grammar + **completion eval 8/8** (`evals/datasets/completion/`).
- [x] `npm run package` smoke OK — VSIX ships `dist/server.js` with the **inlined** kernel index.
- [x] No unjustified `any`; public APIs carry JSDoc (model/indexer/service/provider).
- [ ] CI green on **Linux / macOS / Windows** for the final merge commit (confirm post-merge).

## Section 3: Manual Verification Matrix (Extension Dev Host — 0.5.0 release gate)

Run with **F5** (dev build) and again from a **clean-install VSIX**. Record result + a short note.

| # | Area | Steps | Expected | Result |
|---|---|---|---|---|
| 1 | Real-corpus workspace | Open `../smalltalk-3.2.5/kernel/`; in a `.st` file type `x print` and trigger completion | Kernel selectors (`printString`, `printNl`, …) appear after the receiver | ☐ |
| 2 | Selector ranking | In a workspace defining its own selectors, complete after a receiver | Workspace selectors rank above kernel; prefix **and** camel-hump both filter | ☐ |
| 3 | Class & variable | Inside a method with a temp + instance var, complete in head position | In-scope variables appear first, then class names; correct icons (Field/Variable/Class) | ☐ |
| 4 | Keyword snippet | Accept `at:put:` | Inserts `at:${1} put:${2}`; Tab jumps between the two argument stops | ☐ |
| 5 | Sourcing — installed | With `gst` installed (or `kernelPath` set), reload | Status bar reads **installed (gst)**; completions match that kernel | ☐ |
| 6 | Sourcing — bundled fallback | With no `gst` and `kernelLibrary=auto` | Status bar reads **bundled (gst 3.2.5)**; one-time fallback notice appears with *Open Settings* | ☐ |
| 7 | `off` | Set `smalltalk.completion.kernelLibrary=off` | Kernel completions disappear; only workspace symbols remain; status bar reads **off** | ☐ |
| 8 | Provenance honesty | Inspect a kernel completion's detail | Detail shows `kernel (bundled)` / `kernel (installed)`; status bar agrees | ☐ |
| 9 | Robustness / live edit | Type into a half-written/malformed method; add a new method then complete | Useful (partial) completions, never a thrown error; new symbols appear within ~debounce | ☐ |
| 10 | No-`gst` & zero-config | Fresh machine defaults (no settings, no gst) | Useful kernel completions out of the box (bundled) | ☐ |
| 11 | Cross-platform | Exercise on the dev OS; confirm discovery paths don't glitch | No path/URI issues; CI covers all three OS builds | ☐ |
| 12 | Clean-install VSIX | `npm run package` → install in clean VS Code → repeat #1, #4, #6 | Shipped artifact behaves like the dev build | ☐ |

**Developer Tools console:** ☐ no errors emitted by *this* extension during the above.

## Section 4: Constitutional Compliance

- [x] **Native**: standard completion widget, snippets, status-bar item — no custom UI.
- [x] **Zero Config**: `auto` default gives useful completion with no settings and no `gst`.
- [x] **Robustness**: never-throws front end; indexer/service/provider degrade to empty/bundled.
- [x] **Dialect Agnostic**: neutral index model + per-dialect source adapter seam (ADR-0002).

## Section 5: Sign-Off (release gate)

- [ ] §3 manual matrix executed (dev host **and** clean VSIX) and notes recorded.
- [ ] Issue #1 closed with a demo.
- [ ] Doc-rot audit done (CLAUDE.md, ROADMAP, user-stories, CHANGELOG, README).
- [ ] PO accepts US-413 (DoD met) → cut **v0.5.0** (verify `MARKETPLACE` PAT first).
