# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins  
**Type**: Requirements Quality Gate  
**US**: US-423 — References + Senders/Implementors (Two-Tier Engine) · **Validated**: 2026-06-25

---

## Section 1: Constitution Gates (Mandatory)
- [X] **Native Look & Feel**: Standard `textDocument/references`, `callHierarchy`, and plural
  `textDocument/definition` (`LocationLink[]`) LSP APIs drive VS Code's built-in Peek References, Call
  Hierarchy, and Go-to-Definition UI with no bespoke surface; the two branded commands return a tree the
  native command palette/peek renders (spec §5.4).
- [X] **Zero Config**: No setting required. The cartridge tier resolves via the US-430 Console
  (installed-first / frozen floor); absent `crossReference` ⇒ `[]`, graceful (spec §5.2 step 3). Works with
  no `gst` (AC7).
- [X] **Protocol First**: Pure LSP — `referencesProvider`, `callHierarchyProvider`, `definitionProvider`
  (plural), plus two `workspace/executeCommand` commands; no out-of-band channel.
- [X] **Robustness**: Front end never throws — providers return empty results, never errors; query path
  does no parsing/I-O so it cannot fail on malformed input (spec §5.2, §7). Under-counted cartridge senders
  are framed honestly in the header node rather than hidden (spec §7, AC2).
- [X] **Dialect Agnostic**: The engine merges `workspace ∪ cartridge.crossReference`; provenance is tagged
  `cartridge:<dialect>@<version>`. A future dialect cartridge lights up senders/implementors with no
  provider change (spec §5.1, §5.2).

## Section 2: Specification Completeness
- [X] Goals and Non-Goals explicitly listed (spec §2/§3 — no type inference/receiver resolution, no rename,
  no live image, no System Browser tree).
- [X] User story in standard format (spec §4) — the honest-union dynamic-dispatch posture is part of the
  story, not an afterthought.
- [X] Acceptance criteria defined and testable (AC1–AC7): union/dedup/precedence (AC1), tree + provenance
  header (AC2), plural go-to-def (AC3), call hierarchy reuse (AC4), O(1)+O(k) budget (AC5), `receiverHint`
  ranking-not-filtering (AC6), no-`gst` (AC7).
- [X] Edge cases identified: **dev-box overlap** (kernel source also open in the workspace → drop frozen,
  keep live, spec §5.2 step 5); absent `crossReference` tier; cartridge sender under-count (special-selector
  bytecodes, spec §7); synthetic read-only cartridge URIs for peek/jump (spec §7).
- [X] Dependencies listed (spec §5.5): hard dep on US-430 (`cartridgeLoader` + `crossReference` views —
  **confirmed merged**, the committed cartridge ships 1585 senders / 2459 implementors), US-412
  (`workspaceIndex`/`parseCache`/dynamic config), US-411 (AST).

## Section 3: Technical Design
- [X] API/Command contracts defined: `referencesProvider`, `callHierarchyProvider`, plural
  `definitionProvider`, and commands `smalltalk.sendersOf` / `smalltalk.implementorsOf` advertised in
  `server.ts` (spec §5.4); the command tree carries a header node + per-row provenance/`receiverHint` badge
  (AC2).
- [X] Data structures defined: workspace xref (`selector → SendSite[]`, `selector → ImplementorRef[]`) from
  the US-411 AST; the normalized `ResolvedRef { uri, range, provenance, receiverHint, classId, side }`
  merge target; dedup key `(canonicalUri, line, selector)`; synthetic `smalltalk-cartridge:` URIs (spec
  §5.1, §5.2). `SendSite`/`ImplementorRef`/`CrossReference` already defined in `knowledge-base.ts`.
- [X] Error-handling strategy defined: never-throw; empty on miss; absent-tier graceful; honest header for
  under-count; no I-O at query time so cancellation is trivial (spec §5.2, §7).
- [X] Testing strategy defined: unit (`test:parser` — xref build/incremental-patch, merge/dedup/dev-box
  overlap, `receiverHint` ranking, cartridge slice reads), `test:server` (real-server `references` +
  `callHierarchy` + capability ads), `test:e2e` (peek-references + command tree + cartridge jump), and an
  output-eval dataset `evals/datasets/references/` (spec §5.6).

## Section 4: Validation Result
- [X] **PASS — Ready for implementation.** The spec is complete, testable, and constitutionally clean. The
  central design tension — presenting a lexical union *honestly* (rank, never filter; tag provenance; never
  collapse go-to-def) — is pinned to assertions (AC2/AC3/AC6), the hard dependency (US-430 `crossReference`)
  is confirmed live, and the query-budget claim (AC5) is structurally guaranteed by precomputed O(1) tiers.
  Proceed to the Acceptance Harness (write the failing tests first).
