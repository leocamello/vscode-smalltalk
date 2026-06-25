# Tasks: Semantic Tokens (Cartridge-Aware)

**ID**: US-422 | **Spec**: ./spec.md | **Plan**: ./plan.md

## Phase 1 — Spec & Setup
- [X] T001 Spec reviewed; `requirements-validation.md` gate passed (PASS, 2026-06-25).
- [X] T002 Confirm US-430 loader exposes a known-class lookup — `KernelIndexService.hasClass(name)`
  (workspace classes ∪ cartridge), used by `semanticContext()` in `server.ts`.

## Phase 2 — Implementation
- [X] T010 Legend (`class`/`property`/`variable`/`parameter`/`method`/`keyword`; modifiers
  `static`/`defaultLibrary`); advertise `semanticTokensProvider` (full + range) in `server.ts`.
- [X] T011 `providers/semanticTokens.ts`: single-pass scope-tracking AST walk + symbol-scope role
  resolution (ivar→property, classvar→property+static, temp→variable, params→parameter). (AC1)
- [X] T012 Keyword/binary/unary message + method-pattern + pragma selector parts → `method`; the six
  pseudo-variables → `keyword`. (AC3)
- [X] T013 Cartridge known-class lookup for capitalized identifiers (cartridge ⇒ `defaultLibrary`);
  unknown-capitalized ⇒ global `variable` with a cartridge, ⇒ `class` capitalization fallback without
  one. (AC2/AC4)
- [X] T014 Reuses `parseCache` (`getAst`/`getSymbols`/`getTokens`); `range` variant implemented.
  *(Cancellation: computation is synchronous and sub-ms for typical files, so the handler returns
  directly rather than polling a token — revisit only if a perf budget regresses on huge files.)*

## Phase 3 — Verify
- [X] T900 `evals/datasets/semantic-tokens/` added + green (9/9; `npm run eval`). (AC5)
- [X] T901 Automated layers green: `test:parser` (14 new unit rows), `test:server` (capability +
  decoded `class` token off the bundled cartridge), `test:e2e` (23 passing, incl. 2 US-422 rows).
  Manual-QA workspace authored (`manual-qa-workspace/`, fixture verified to parse 0-diagnostics and
  match every README role); **Extension-Host run pending** before release sign-off.
- [ ] T902 CI green on Linux/macOS/Windows (pending push/PR).
