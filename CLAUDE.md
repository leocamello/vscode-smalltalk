# CLAUDE.md

Project brief for AI agents working in this repo. Read [`CONTRIBUTING.md`](CONTRIBUTING.md)
for the full process and [`docs/ROADMAP.md`](docs/ROADMAP.md) for where we're headed.

## What this is
`vscode-smalltalk` — a VS Code extension for **GNU Smalltalk** (`.st`/`.gst`). Published on the
Marketplace as `leocamello.vscode-smalltalk`.

## Current status (2026-06-19)
- **Shipped:** v0.3.0 — rewritten TextMate grammar, snippets, language config; **Run Current
  File** command (US-301); and a **bundled TypeScript language server** scaffold (US-410, no-op
  so far). Runs without `gst`.
- **Next:** **M3 / US-411** (GitHub #23) — the error-tolerant Smalltalk **lexer + parser +
  symbol table**. Internal milestone behind 0.4.0 navigation. Blueprint:
  `docs/research/gst-syntax/01-gst-lexer-token-analysis.md` + `02-gst-parser-syntactic-structures.md`;
  fixtures in `docs/research/gst-syntax/test-cases/*.st`; kernel corpus at `../smalltalk-3.2.5/kernel/`.

## How we work (spec-driven)
Per user story (`US-XXX`): **Clarify → Spec → Plan → Task → Implement → Verify**.
- Scaffold the spec package: `npm run new-story -- US-XXX "Title" --branch`
  (creates `specs/US-XXX-Title/` + branch `feature/US-XXX-slug`).
- Source of truth: `docs/product/user-stories.md` is the backlog; once a story starts, its
  `specs/US-XXX-*/spec.md` is the detailed source of truth. Architecture: `docs/decisions/` (ADRs);
  principles: `.specify/memory/constitution.md`.
- Quality harness (`evals/`): every change gets **Output Eval** (`npm run eval` + per-feature
  datasets) and **Trajectory Eval** (process rubric). The PR template gates merge on both + human
  sign-off. A feature isn't Done until its eval passes **and CI is green on Linux/macOS/Windows**.

## Conventions
- **Branches:** `feature/US-XXX-slug` (new) / `fix/US-XXX-slug` (fixes); PR to `master`, close the issue on merge.
- **Commits:** Conventional + scoped — `feat(US-XXX):`, `fix:`, `docs:`, `chore:`, `ci:`, `test:`. Use `Closes #N` (no markdown around it) to auto-close. **Never add a `Co-Authored-By` trailer.**
- **Outward/irreversible actions** (Marketplace publish, merges, issue/PR mutations): confirm with the owner first.

## Commands
```bash
npm install            # workspaces: root + client/ + server/
npm run compile        # esbuild → dist/extension.js + dist/server.js
npm run check-types    # tsc --noEmit (strict)
npm run lint           # eslint (client/src, server/src)
npm run test:client    # tsx unit tests (pure logic)
npm run test:server    # LSP handshake test against the bundled server
npm run eval           # grammar snapshot tests (Output eval; grows per milestone)
npm run package        # vsce package --no-dependencies (clean VSIX smoke)
npm run new-story -- US-XXX "Title" --branch
```
Manual QA: **F5** → Extension Development Host. The built grammar JSON and `dist/` are gitignored
(produced by `vscode:prepublish`).

## Architecture (ADR-0001)
Language server is **TypeScript, bundled** in the VSIX — fully functional **without** `gst`.
`gst` is an *optional* tool (Run Current File; opt-in compile diagnostics later). Parser is
layered (core ANSI + pluggable GST chunk/brace container formats) so other dialects can plug in.

## Releasing
Create a `vX.Y.Z` GitHub Release → CI publishes via `vsce` (auth = `MARKETPLACE` secret, an Azure
DevOps PAT that **expires ~yearly** — check it before tagging). Bump `version` + CHANGELOG first.
