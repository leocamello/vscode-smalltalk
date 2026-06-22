# Implementation Plan: References + Senders/Implementors (Two-Tier Engine)

**ID**: US-423 | **Date**: 2026-06-22 | **Spec**: ./spec.md | **Branch**: `feature/US-423-references-senders-implementors` (tbd)
**Architecture**: [ADR-0002](../../docs/decisions/0002-kernel-symbol-sourcing.md), [ADR-0003](../../docs/decisions/0003-cartridge-resolution.md)

## Summary
Build the two-tier cross-reference engine (workspace xref + cartridge `crossReference`) and expose it as
`references`, the Senders/Implementors commands, and call hierarchy — offline, with an honest
dynamic-dispatch (lexical-union) posture. Depends on US-430's cartridge loader.

## Approach
**The cartridge already ships the kernel half.** The missing half is a **workspace cross-reference index**
(selector → senders/implementors), built from the US-411 AST and kept fresh via `parseCache`/`didChange`
exactly like `workspaceIndex` does for symbols. The query is then two O(1) lookups + an O(k) merge
(spec §5.2). New code: `server/src/xref/` (workspace index + merge), `providers/references.ts`,
`providers/callHierarchy.ts`, the two commands, and a client `smalltalk-cartridge:` content provider.

## Steps (sliced into PRs)
1. **Slice A — workspace cross-reference index.** `server/src/xref/workspaceXref.ts`: walk message-send
   nodes → `selector → SendSite[]`; walk method defs → `selector → ImplementorRef[]`; incremental patch on
   `didChange`. Unit tests (build + patch).
2. **Slice B — merge engine + `references`.** `server/src/xref/resolve.ts` implementing spec §5.2
   (normalize, dedup incl. dev-box overlap, precedence, sort). Wire `referencesProvider` in `server.ts`;
   plural go-to-definition (AC3). Unit + `test:server` + e2e.
3. **Slice C — Senders/Implementors commands + virtual docs.** Structured tree with header node + provenance
   (AC2); client `TextDocumentContentProvider` for `smalltalk-cartridge:` so cartridge hits peek/jump to a
   read-only buffer. E2e on the command tree.
4. **Slice D — call hierarchy.** `callHierarchyProvider` (incoming = senders, outgoing = sends in a method)
   reusing the engine. Unit + e2e. Eval dataset `evals/datasets/references/`.

## Dependencies & Risks
- **Depends on US-430** (cartridge loader + `crossReference` views) — do not start before US-430 Slice B.
  Also US-411 (AST), US-412 (`workspaceIndex`/`parseCache`/dynamic config).
- **Risks** (spec §7): cartridge sender under-count (honest header), workspace xref perf/memory (lazy +
  cap, incremental), synthetic-URI plumbing for peek/jump.

## Verification
- **Automated**: `test:parser` (xref build/patch, merge/dedup/dev-box, ranking), `test:server`
  (`references` + `callHierarchy`), `test:e2e` (peek + command tree), `evals/datasets/references/`.
- **Manual**: `verification.md` — Senders/Implementors of `do:` (workspace + kernel + provenance + union
  header), dev-box de-dup, plural go-to-def, call hierarchy, no `gst`.
