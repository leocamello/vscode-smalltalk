# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins  
**Type**: Requirements Quality Gate  
**US**: US-422 — Semantic Tokens (Cartridge-Aware) · **Validated**: 2026-06-25

---

## Section 1: Constitution Gates (Mandatory)
- [X] **Native Look & Feel**: Uses the standard `textDocument/semanticTokens` LSP API + the standard
  token-type/modifier legend, so the user's theme colors roles with no custom config (spec §5).
- [X] **Zero Config**: No setting required; degrades to a capitalization fallback with no cartridge
  (AC4). Works with no `gst`.
- [X] **Protocol First**: Pure LSP (`semanticTokens/full` + `range`); no bespoke channel.
- [X] **Robustness**: Front end never throws — the provider returns an (empty) token set, never an error
  (spec §5, §7); reuses `parseCache`, honors cancellation.
- [X] **Dialect Agnostic**: The known-class set is `workspace ∪ cartridge` via the US-430 Console facade
  (`kernelService.hasClass`); a future dialect cartridge lights up AC2 with no provider change.

## Section 2: Specification Completeness
- [X] Goals and Non-Goals explicitly listed (spec §2/§3 — no new parsing, no theming, layers on TextMate).
- [X] User story in standard format (spec §4).
- [X] Acceptance criteria defined and testable (AC1–AC5); each maps to a role/behavior assertion.
- [X] Edge cases identified: no-cartridge fallback (AC4); pseudo-variables; unknown global vs class;
  shadowing (lexical param/temp wins over a class field, per the US-415 hover scope rule).
- [X] Dependencies listed (spec §Dependencies): US-430 loader class set, US-411 AST + symbol scopes.

## Section 3: Technical Design
- [X] API/Command contracts defined: `semanticTokensProvider` (full + range) with an explicit legend
  advertised in `server.ts` (spec §5).
- [X] Data structures defined: LSP delta-encoded `(deltaLine, deltaStartChar, length, type, modifiers)`;
  token-type + modifier legend (`class`, `property`, `variable`, `parameter`, `method`, `keyword`;
  modifiers `static`, `defaultLibrary`).
- [X] Error-handling strategy defined: never-throw; empty result on miss; cancellation/`range` for perf.
- [X] Testing strategy defined: unit (per-role classification + AC2 cartridge-vs-unknown + AC4 fallback),
  `test:server` (capability + a token assertion), e2e token-range assertion, and an output-eval dataset
  `evals/datasets/semantic-tokens/` (AC5).

## Section 4: Validation Result
- [X] **PASS — Ready for implementation.** The spec is complete, testable, and constitutionally clean;
  the one differentiator (AC2, cartridge-driven class/global) and the fallback (AC4) are both pinned to
  assertions. Proceed to the Acceptance Harness (write the failing tests first).
