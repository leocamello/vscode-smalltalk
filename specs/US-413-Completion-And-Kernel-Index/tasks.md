# Tasks: Completion + GNU Smalltalk kernel index

**ID**: US-413 | **Spec**: ./spec.md | **Plan**: ./plan.md | **ADR**: [0002](../../docs/decisions/0002-kernel-symbol-sourcing.md)

Mark each task `[x]` as it lands. Tasks map to acceptance criteria and PR slices (A–D).

## Phase 0 — Decisions & Setup
- [x] T000 ADR-0002 recorded; US-413 ACs/DoD amended in `docs/product/user-stories.md`.
- [x] T001 Spec authored; `requirements-validation.md` gate passed (PASS).

## Phase 1 — Slice A: index model + bundled generator (AC1)
- [x] T100 `server/src/kernel/model.ts` — neutral types (`KernelSelector`, `KernelClass`,
  `KernelIndexData` header + classes, `Provenance`).
- [x] T101 `server/src/kernel/indexer.ts` — `indexKernelDirectory(dir, meta)`: walk `*.st`, parse +
  `buildSymbolTable`, merge classes across files/chunks, dedup + sort, never throw; **facts only**.
- [x] T102 `scripts/gen-kernel-index.ts` + `gen:kernel-index` npm script.
- [x] T103 Generate & commit `server/data/kernel-index.json` (deterministic; `dialect`/`library`/
  `version`/`source` header; no timestamp) — 250 classes, 4723 selectors.
- [x] T104 `server/test/kernelIndex.test.ts` wired into `run.ts`: invariants (known classes, sorted,
  arity ≥ 0) + **licensing no-prose gate** (runs without corpus) + regeneration **drift guard** (when
  corpus present).
- [~] T105 `npm run test:parser`, `check-types`, `lint`, `test:server` green locally; PR (A) opened,
  CI watched, merge held. *(awaiting owner go-ahead to push/open PR)*

## Phase 2 — Slice B: kernel index service (AC5/AC6)
- [x] T200 `server/src/kernel/kernelIndexService.ts` — load bundle (via `bundledIndex.ts`, JSON
  imported so esbuild inlines it); `discovery.ts` discovers install (`kernelPath` →
  `gnuSmalltalkPath` prefix → common locations); resolve `auto|bundled|off`; provenance-tagged
  `selectors()`/`classes()`; re-resolve on config change (`configure`). `resolveJsonModule` enabled.
- [x] T201 Unit + discovery tests (`kernelService.test.ts`, wired into `run.ts`) — temp fixture
  kernel dirs; gst-prefix derivation; no-install bundled fallback; reconfigure. Hermetic (common
  locations injectable).
- [~] T202 `check-types`/`lint`/`test:parser` (10 new)/`test:server`/`compile` green locally;
  PR (B) opened, CI watched, merge held. *(VSIX bundling of the JSON verified in slice C / T301.)*

## Phase 3 — Slice C: completion provider (AC2–AC4, AC7 items)
- [ ] T300 `server/src/providers/completion.ts` — cursor-context detection over `parseCache`
  (receiver → selectors; head → classes + scope vars); merge workspace + kernel; rank
  workspace > installed > bundled (prefix + camel-hump); keyword-selector snippets; provenance in items.
- [ ] T301 Advertise `completionProvider` + wire `onCompletion` in `server.ts`; ensure
  `kernel-index.json` is bundled into the VSIX.
- [ ] T302 Provider unit tests at cursor positions; extend `test:server` for completion; e2e fixture
  asserting kernel + workspace completions.
- [ ] T303 Layers green; PR (C) opened, CI watched, merge held.

## Phase 4 — Slice D: settings + status UX (AC5/AC7)
- [ ] T400 `smalltalk.completion.kernelLibrary` (`auto|bundled|off`) + `smalltalk.completion.kernelPath`
  in `package.json` (descriptions name the bundled dialect/version).
- [ ] T401 Status-bar item showing resolved kernel identity (click → setting); one-time bundle-fallback
  notice.
- [ ] T402 e2e for the setting/status paths; layers green; PR (D) opened, CI watched, merge held.

## Phase 5 — Verify & Release
- [ ] T900 Output eval extended (`evals/` completion dataset) + `npm run eval` green.
- [ ] T901 `verification.md` manual-QA matrix executed in the Extension Dev Host (incl. real-corpus +
  clean-install VSIX) and signed off.
- [ ] T902 CI green on Linux/macOS/Windows for all slices.
- [ ] T903 Issue #1 closed with a demo; doc-rot audit; bump version + CHANGELOG; **owner** cuts v0.5.0.
