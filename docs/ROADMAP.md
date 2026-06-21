# Roadmap

A snapshot of the milestone plan. Detail lives in `docs/product/` (epics, user stories) and
`docs/decisions/` (ADRs). Architecture baseline:
[ADR-0001 — TypeScript bundled LSP server](decisions/0001-typescript-bundled-lsp-server.md).

| Status | Version | Theme | Exit gate |
|---|---|---|---|
| ✅ Shipped | **0.2.0** | Housekeeping + release the rewritten grammar, snippets, language config; fix bug #2; modernize packaging/CI | Marketplace release; CI green on all OSes |
| ✅ Shipped | **0.3.0** | TypeScript + LSP scaffold (`client/` + `server/`, esbuild, bundled no-op server) + "Run Current File" (US-410, US-301) | Client↔server handshake; first command shipped |
| ✅ Done | **M3** *(internal)* | Error-tolerant Smalltalk parser + symbol table (US-411, #23) | Kernel smoke test passes — 122 files, 0 crashes/diagnostics |
| ✅ Shipped | **0.4.0** | Outline/workspace symbols + go-to-definition (US-412) | Navigation works without `gst` |
| ✅ Shipped | **0.5.0** | Completion + GNU Smalltalk kernel index (US-413) | Closes issue #1 |
| ⏭️ **Next** | **0.6.0** | Diagnostics — parser live; `gst` opt-in (US-414, #26) | Squiggles on syntax errors; no zombie `gst` procs |
| ⬜ Planned | **0.7.0** | Hover (US-415, #27) | Markdown docs on hover (selectors/classes/vars/literals) |
| ⬜ Planned | **0.8.0** | Formatting (US-416, #28) — *droppable to 1.1 if it slips* | Idempotent (`f(f(x))==f(x)`), opt-in/default-off |
| ⬜ Planned | **0.9.0** | Hardening, performance, beta polish (US-901) | Perf budgets met (1k files < 5 s, completion < 100 ms); no-telemetry verified; no P1 bugs 2+ weeks |
| ⬜ Planned | **1.0.0** | Product polish; remove `preview`; publish to Open VSX (US-902) | Marketing-grade README + demos; ovsx publish (needs `OVSX_PAT`) |

**✅ 0.4.1 (point release):** navigation polish — **US-417** (semantic `foldingRange` +
`documentHighlight`), a near-free follow-up to 0.4.0 reusing the US-411 AST.

## Next up

1. **0.6.0 / US-414 — Diagnostics** is the next milestone. Slice A is small (the US-411 parser already
   emits `parse().diagnostics`): publish them on change, debounced, code `smalltalk(parse)`. Then the
   opt-in `gst`-on-save path (process lifecycle: timeout/kill-on-edit, no zombies), then trivial code
   actions.
2. **Optional v0.5.1** — fold in the two near-term reconciliation fixes: **US-420** (#60, completion
   pseudo-variables) + **US-421** (#61, CI kernel fixtures). Both `size:S`; can also ride the front of
   the US-414 branch.

## Backlog & sequencing (0.5.0 plan reconciliation)

Small follow-ups where the implementation diverged from the (now-archived) genesis `plan.md`; full
stories in [`docs/product/user-stories.md`](product/user-stories.md).

| Story | What | When to do it | Issue |
|---|---|---|---|
| US-418 | Container-format seam (dialect door) | **Deferred** — trigger: the first additional dialect (YAGNI) | #58 |
| US-419 | Kernel-index method categories | Ride with **US-415 hover** (needs the grouping) | #59 |
| US-420 | Completion pseudo-variables | Near-term — v0.5.1 or front of US-414 | #60 |
| US-421 | CI vendored kernel smoke fixtures | Near-term — v0.5.1 or front of US-414 | #61 |

## How each milestone ships (the repeatable ritual)

Per story: **Clarify → Spec → Plan → Task → Implement → Verify** (`npm run new-story -- US-XXX "…" --branch`).
Each slice keeps the **three test layers** green (`test:parser`, `test:server`, `test:e2e`) **plus an
output-eval dataset** in `evals/datasets/<feature>/` (use `completion/` as the template). Then the
**release ritual**: doc-rot audit + the **manual-QA matrix** (`specs/US-XXX-*/verification.md`, real
corpus + clean VSIX) → bump version + CHANGELOG → **check the `MARKETPLACE` PAT** → cut the `vX.Y.Z`
Release (CI publishes). Full detail in [`CLAUDE.md`](../CLAUDE.md) and [`CONTRIBUTING.md`](../CONTRIBUTING.md).

_Last updated: 2026-06-21 — **0.5.0 shipped & published** (US-413; #25/#1 closed). Plan reconciliation
done (#62): backlog US-418–421 (#58–#61) filed, US-901/902 + Constitution v1.2.0 capture migrated
plan-only scope. Next focus: 0.6.0 / US-414 (diagnostics)._

**Dialect scope:** GNU Smalltalk now. The parser handles both GST container formats (brace + chunk);
the *pluggable* `ContainerFormat` seam for other dialects (Pharo/Tonel) is **deferred to US-418**
(built when a second dialect lands) — see [ADR-0001 §Update](decisions/0001-typescript-bundled-lsp-server.md).

**Sustainability:** every milestone is independently shippable; work-in-progress is kept to
one milestone at a time; formatting (0.8.0) is explicitly droppable from 1.0.
