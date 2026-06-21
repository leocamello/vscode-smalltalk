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
| ⏭️ **Next** | **0.5.0** | Completion + GNU Smalltalk kernel index (US-413) | Closes issue #1 |
| ⬜ Planned | **0.6.0** | Diagnostics — parser live; `gst` opt-in (US-414) | Squiggles on syntax errors |
| ⬜ Planned | **0.7.0** | Hover (US-415) | Docs on hover |
| ⬜ Planned | **0.8.0** | Formatting (US-416) — *droppable to 1.1 if it slips* | Idempotent, opt-in |
| ⬜ Planned | **0.9.0** | Hardening, performance, beta polish | No P1 bugs for 2+ weeks |
| ⬜ Planned | **1.0.0** | Product polish; remove `preview`; publish to Open VSX | Marketing-grade README + demos |

**✅ 0.4.1 (point release):** navigation polish — **US-417** (semantic `foldingRange` +
`documentHighlight`), a near-free follow-up to 0.4.0 reusing the US-411 AST.

_Last updated: 2026-06-21 — 0.4.1 shipped (folding + document highlight); 0.4.0 navigation + M3 parser/symbol-table done. Next focus: 0.5.0 / US-413 (completion)._

**Dialect scope:** GNU Smalltalk now; the parser is layered (core ANSI + pluggable GST
container formats) so other dialects (e.g. Pharo/Tonel) can be added later without a
rewrite.

**Sustainability:** every milestone is independently shippable; work-in-progress is kept to
one milestone at a time; formatting (0.8.0) is explicitly droppable from 1.0.
