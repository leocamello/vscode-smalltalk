# Specification: Semantic Tokens (Cartridge-Aware)

**ID**: US-422
**Feature**: `textDocument/semanticTokens` — role-accurate highlighting, including a cartridge-driven known-class vs unknown-global distinction
**Status**: Draft
**Owner**: Leonardo Nascimento
**Created**: 2026-06-22
**Architecture**: [ADR-0003](../../docs/decisions/0003-cartridge-resolution.md)

## 1. Overview
Color Smalltalk by *role*, not just token shape — instance var vs class var vs temp vs block/method arg
vs *known class/global* vs unknown global vs keyword-selector part vs pseudo-variable — from the US-411
AST + symbol table. The one part that is distinctly ours and offline: a capitalized identifier is colored
`class` only if it resolves to a known `ClassId` in `workspace ∪ cartridge` — so kernel classes
(`OrderedCollection`, `Transcript`) light up correctly with **no `gst`**. This is the first *visible*
consumer of the cartridge (US-430).

## 2. Goals
- `textDocument/semanticTokens/full` (+ `range`) over `parseCache`, classifying the role set above.
- Cartridge-driven class/global distinction (the AC2 differentiator).
- Distinct tokens for keyword-message parts and the six pseudo-variables.
- Graceful no-cartridge fallback; works with no `gst`.

## 3. Non-Goals
- No new parsing — pure consumption of the US-411 AST/symbols + the US-430 cartridge class set.
- No semantic-token-based diagnostics or hover (US-414/US-415).
- Does not replace the TextMate grammar; it layers on top (VS Code merges them).

## 4. User Stories & Acceptance Criteria
**US-422**: As a Smalltalk developer, I want role-accurate semantic highlighting so that I read code by
meaning, even for kernel classes I never declared.

- **AC1**: `semanticTokens/full` (+ `range`) classifies ivar / classvar / temp / block-arg / method-arg /
  class / unknown-global / keyword-selector-part / pseudo-variable from the AST + symbol-table scopes;
  reuses `parseCache`.
- **AC2**: A capitalized identifier is `class` **iff** it resolves to a `ClassId` in `workspace ∪
  cartridge`; otherwise `variable.other.global`.
- **AC3**: Keyword-message parts and pseudo-variables (`self super nil true false thisContext`) carry
  distinct token types/modifiers.
- **AC4**: Degrades cleanly with no cartridge loaded (capitalization fallback; never errors).
- **AC5**: Works with no `gst`; output-eval dataset `evals/datasets/semantic-tokens/`.

## 5. Technical Design
- **Provider** `server/src/providers/semanticTokens.ts`: walk the US-411 AST + resolve identifiers against
  the symbol-table scope at each position (the same scope resolution `documentHighlight` already uses).
  Emit `(deltaLine, deltaStartChar, length, tokenType, tokenModifiers)` per the LSP encoding.
- **Legend**: declare token types/modifiers in the provider + advertise `semanticTokensProvider` (full +
  range) with the legend in `server.ts`.
- **Cartridge lookup (AC2)**: a `knownClass(name): boolean` from the US-430 loader (workspace class set ∪
  cartridge `classes` keys, namespace-aware). Capitalized identifier ∈ set → `class`, else `variable.other.global`.
- **Role resolution**: ivar/classvar/temp/arg come from the symbol-table binding kind at the cursor scope;
  keyword-selector parts from message-send nodes; pseudo-vars from a fixed set.
- **No-cartridge fallback (AC4)**: if the loader has no cartridge, fall back to "capitalized ⇒ class".

## 6. Manual Verification
Extension Development Host: open a kernel file + a workspace file; confirm ivars/temps/args/classvars are
visually distinct, kernel class names are colored as classes (with no `gst`), an unknown capitalized name
is colored as a global, and pseudo-vars are distinct. Matrix in `verification.md`.

## 7. Risks & Limitations
- **Lightest cartridge consumer** — graded *hygiene* (table-stakes), not a moat; AC2 is the only
  distinctly-ours part. Keep scope small (a 0.7.x-style point release).
- **Perf**: semantic tokens fire on edit — reuse `parseCache`, honor cancellation; range variant for large
  files. (Constitution: typing latency must not block the UI.)
- **Theme dependence**: role colors depend on the user's theme mapping standard token types; we only
  classify, we don't theme.

## Dependencies
- **US-430** loader (AC2 class set). US-411 AST + symbol scopes. No `gst`.
