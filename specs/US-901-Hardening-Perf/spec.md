# Specification: Hardening & Perf

**ID**: US-901
**Feature**: Hardening & Perf
**Status**: Draft
**Owner**: Leonardo Nascimento
**Created**: 2026-07-02
**Issue**: #112 | **Milestone**: 0.13.0 | **Epic**: EPIC-004 (Language Intelligence — TypeScript LSP)

## 1. Overview
The offline language-intelligence surface is complete through class rename (v0.12.0). Before the 1.0
polish (US-902), the engine gets a **hardening & performance** pass so the beta is trustworthy on a
real, large workspace: prove the perf budgets on a 1,000-file workspace, harden the parser/front-end
against malformed and pathological input, verify we ship **zero telemetry**, and run a triaged
bug-bash over the existing providers. This story adds **no new user-facing feature** — it makes the
existing surface fast, robust, and honest.

## 2. Goals
- A committed, repeatable **perf benchmark** (`npm run bench`) over a **generated synthetic corpus**,
  reporting index / completion / class-rename timings against documented budgets.
- Meet the budgets: **1,000 files index < 5 s** (cold), **completion p95 < 100 ms** on that workspace.
- Investigate and, if warranted, optimize the class-rename whole-workspace scan
  (`allWorkspaceFiles`, `server.ts`) — **measure before optimizing**.
- **Robustness:** the lexer/parser survive fuzzed & pathological input without throwing (the
  "front end never throws" invariant), and pathological single files are **bounded** — no hang.
- **No telemetry:** an audit + guard proves the extension performs no network/telemetry I/O.
- **Bug-bash:** a triaged pass over completion/hover/rename/references/format; file defects, fix the
  triaged batch, each fix pinned by a regression test.

## 3. Non-Goals
- New features or new providers (deferred to US-902 / later milestones).
- Removing the `format.enable` / preview flag, Open VSX publishing (US-902, 1.0).
- Any runtime/`gst` work (EPIC-007, Live Bridge).
- A **hard CI perf gate** — budgets are enforced locally + at release, not as a blocking CI job
  (runner variance across the Linux/macOS/Windows matrix → flaky). CI stays green on the existing
  unit/eval/e2e layers.
- Multi-threading / worker offload of the server (out of scope; revisit only if budgets can't be met
  by algorithmic fixes).

## 4. User Stories & Acceptance Criteria
**US-901**: As a Smalltalk developer working in a large workspace, I want the extension to stay fast
and never hang or crash, so that indexing, completion and refactoring feel instant and the beta is
trustworthy.

- **AC1 — Index budget:** `npm run bench` indexes a **1,000-file** generated workspace (cold, from
  disk, feeding both the symbol index and the cross-reference index) in **< 5 s** on the dev box, and
  prints the measured time against the budget.
- **AC2 — Completion budget:** on that 1k-file indexed workspace, completion (`completionsAt`)
  responds with **p95 < 100 ms** over a representative request sample, printed against the budget.
- **AC3 — No telemetry:** a guard test asserts the built server + client contain **no** network/
  telemetry surface (no `http(s)`/`fetch`/`net`/`TelemetryReporter`/analytics imports or calls), and
  the no-telemetry stance is documented (README/CHANGELOG).
- **AC4 — Robustness (never-throw):** a fuzz test drives `tokenize`/`parse` with malformed, truncated,
  and adversarial input (unterminated strings/comments, unbalanced brackets, huge tokens, random
  bytes) and asserts **no throw** — a `LexResult`/`ParseResult` is always returned (diagnostics, not
  exceptions).
- **AC5 — Stress limits:** a pathological single file (very large and/or deeply nested) is **bounded**
  — `tokenize`+`parse` complete under a documented ceiling with no hang; the bench reports the timing.
- **AC6 — Bug-bash:** a documented triaged pass over the existing providers; defects filed as issues,
  the triaged batch fixed, and **each fix pinned by a regression test** at its appropriate layer
  (unit / eval / e2e).

## 5. Technical Design
**Perf harness — `scripts/bench.ts` (`npm run bench`), a `tsx` script (no Electron/LSP).** It drives
the pure server functions directly:
- **Corpus generator — `scripts/gen-corpus.ts`:** parametric (`--files N --out DIR`), emits N `.st`
  files with realistic shapes — class definitions with superclass chains, instance variables, methods
  with keyword/unary/binary sends, and **cross-file class references** (so the class-rename scan and
  the xref index are exercised, not just isolated files). Deterministic (seeded). Committed as a
  **script**, not as corpus files.
- **Index measurement (AC1):** walk the generated dir via `walkStFiles` feeding `index.setFile` +
  `workspaceXref.setFile` (mirrors `scanWorkspace` in `server.ts`), timed cold.
- **Completion measurement (AC2):** build the index once, then call `completionsAt` over a sample of
  positions/prefixes; report p50/p95/max.
- **Rename-scan measurement (AC5/info):** time `allWorkspaceFiles` + `classOccurrences` for a class
  used across many files — the flagged suspect.
- Output: a table of `metric … measured (budget) PASS/FAIL`; non-zero exit if a budget FAILs (so it's
  usable as a release-ritual check without being wired into CI).

**Perf suspect — `allWorkspaceFiles()` (`server.ts:294`).** It re-reads every indexed file from disk
on each class rename (`byUri` seeded from open docs, then `fs.readFileSync` per indexed uri). Measure
first; if it dominates, the fix is to **reuse text the index already holds** (the `WorkspaceIndex`
already stores per-file content via `setFile`; expose a `getText(uri)` / `entriesWithText()` accessor)
rather than re-reading disk — keeping open-doc-wins semantics. No behavior change, only fewer disk
reads. Optimize **only if measured**.

**Robustness (AC4/AC5).** A fuzz/property test in `server/test/` generates adversarial inputs and
asserts `tokenize`/`parse` return normally. Any input that throws is a defect to fix (bound the loop /
guard the index). Pathological-size inputs get an explicit ceiling assertion.

**No telemetry (AC3).** A static guard test greps the built `dist/` (or the `server/src`+`client/src`
sources) for a denylist of network/telemetry identifiers and fails on any hit — a durable regression
guard, not a one-time audit. Document the stance.

**Bug-bash (AC6).** A structured matrix (provider × edge case) run in the Extension Host + against the
corpus; findings triaged into fix-now vs backlog; each fix-now defect gets a failing regression test
first, then the fix.

## 6. Risks & Limitations
- **Bench variance:** absolute timings are machine-dependent — budgets are asserted on the dev box and
  treated as a **local + release signal**, not a cross-runner CI gate (see Non-Goals).
- **Synthetic corpus fidelity:** generated files may not match real-world reference density; the
  generator aims for representative subclassing/send/xref shapes, documented in the script.
- **Bug-bash open-endedness:** scope is bounded by triage — not every found defect is fixed in 0.13.0;
  the backlog captures the rest.
- **Optimization risk:** changing `allWorkspaceFiles` must preserve open-doc-wins + best-effort
  skip-unreadable semantics; guarded by the existing `classRename` unit tests staying green.
