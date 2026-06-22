# Tasks: Semantic Tokens (Cartridge-Aware)

**ID**: US-422 | **Spec**: ./spec.md | **Plan**: ./plan.md

## Phase 1 — Spec & Setup
- [ ] T001 Spec reviewed; `requirements-validation.md` gate passed.
- [ ] T002 Confirm US-430 loader exposes a `knownClass(name)` / class set.

## Phase 2 — Implementation
- [ ] T010 Legend (token types/modifiers); advertise `semanticTokensProvider` (full + range) in `server.ts`.
- [ ] T011 `providers/semanticTokens.ts`: AST walk + symbol-scope role resolution (ivar/classvar/temp/args). (AC1)
- [ ] T012 Keyword-message parts + pseudo-variables as distinct tokens. (AC3)
- [ ] T013 Cartridge `knownClass` lookup for capitalized identifiers; capitalization fallback. (AC2/AC4)
- [ ] T014 Reuse `parseCache`; honor cancellation; range variant.

## Phase 3 — Verify
- [ ] T900 `evals/datasets/semantic-tokens/` added + green. (AC5)
- [ ] T901 `verification.md` gate passed (roles distinct; kernel classes colored with no gst; pseudo-vars).
- [ ] T902 CI green on Linux/macOS/Windows.
