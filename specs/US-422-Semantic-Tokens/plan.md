# Implementation Plan: Semantic Tokens (Cartridge-Aware)

**ID**: US-422 | **Date**: 2026-06-22 | **Spec**: ./spec.md | **Branch**: `feature/US-422-semantic-tokens` (tbd)
**Architecture**: [ADR-0003](../../docs/decisions/0003-cartridge-resolution.md)

## Summary
Add a `semanticTokens` provider over the US-411 AST + symbol scopes, with a cartridge-driven known-class
vs unknown-global distinction (the differentiator). Smallest cartridge consumer; ride a point release.

## Approach
Reuse the scope-resolution `documentHighlight` already does and the `parseCache`. The only new external
dependency is a `knownClass(name)` set from the US-430 loader. Emit the LSP delta-encoded token stream.

New module: `server/src/providers/semanticTokens.ts`; wiring + legend in `server/src/server.ts`.

## Steps
1. Define the legend (token types/modifiers); advertise `semanticTokensProvider` (full + range).
2. Implement classification: walk AST; resolve identifier role from symbol-table binding kind; keyword
   parts from message-send nodes; pseudo-vars from a fixed set. (AC1/AC3)
3. Wire the US-430 `knownClass` lookup for capitalized identifiers (AC2); capitalization fallback when no
   cartridge (AC4).
4. Tests + eval dataset (AC5).

## Dependencies & Risks
- **Depends on US-430** loader (`knownClass`/class set). US-411 AST + scopes (merged).
- **Risks** (spec §7): perf on edit (reuse `parseCache`, cancellation, range variant); theme dependence;
  scope creep — keep it hygiene-sized.

## Verification
- **Automated**: `test:parser` (classification per role; AC2 cartridge-vs-unknown over a fixture; AC4
  fallback), `test:server` (capability + token range), `evals/datasets/semantic-tokens/`.
- **Manual**: `verification.md` — roles visually distinct; kernel classes colored with no `gst`; unknown
  global vs class; pseudo-vars distinct.
