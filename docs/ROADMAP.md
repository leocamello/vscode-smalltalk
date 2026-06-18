# Roadmap

A snapshot of the milestone plan. Detail lives in `docs/product/` (epics, user stories) and
`docs/decisions/` (ADRs). Architecture baseline:
[ADR-0001 — TypeScript bundled LSP server](decisions/0001-typescript-bundled-lsp-server.md).

| Version | Theme | Exit gate |
|---|---|---|
| **0.2.0** | Housekeeping + release the rewritten grammar, snippets, language config; fix bug #2; modernize packaging/CI | Marketplace release; CI green on all OSes |
| **0.3.0** | TypeScript scaffolding (`client/` + `server/`, esbuild) + "Run Current File" (US-301) | First imperative feature shipped |
| **M3** *(internal)* | Error-tolerant Smalltalk parser + symbol table (US-411) | GNU Smalltalk kernel smoke test passes |
| **0.4.0** | LSP scaffold + outline/workspace symbols + go-to-definition (US-410, US-412) | Navigation works without `gst` |
| **0.5.0** | Completion + GNU Smalltalk kernel index (US-413) | Closes issue #1 |
| **0.6.0** | Diagnostics — parser live; `gst` opt-in (US-414) | Squiggles on syntax errors |
| **0.7.0** | Hover (US-415) | Docs on hover |
| **0.8.0** | Formatting (US-416) — *droppable to 1.1 if it slips* | Idempotent, opt-in |
| **0.9.0** | Hardening, performance, beta polish | No P1 bugs for 2+ weeks |
| **1.0.0** | Product polish; remove `preview`; publish to Open VSX | Marketing-grade README + demos |

**Dialect scope:** GNU Smalltalk now; the parser is layered (core ANSI + pluggable GST
container formats) so other dialects (e.g. Pharo/Tonel) can be added later without a
rewrite.

**Sustainability:** every milestone is independently shippable; work-in-progress is kept to
one milestone at a time; formatting (0.8.0) is explicitly droppable from 1.0.
