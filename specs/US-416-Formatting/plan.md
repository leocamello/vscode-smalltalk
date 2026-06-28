# Implementation Plan: Formatting

**ID**: US-416 | **Date**: 2026-06-27 | **Spec**: ./spec.md | **Branch**: `feature/US-416-formatting`
**ADR**: [ADR-0005](../../docs/decisions/0005-formatter-whitespace-rewriter.md) (formatter = whitespace-only token-stream rewriter)

## Summary
Deliver conservative, **idempotent** GNU Smalltalk formatting (document + range + on-type) as a
**whitespace-only token-stream rewriter** over the US-411 lexer/AST: keep every token and comment verbatim,
rewrite only the whitespace between them (indent by block depth, canonical spacing, blank-line collapse,
cascade alignment, keyword-message wrapping). This makes idempotence and token-stream invariance structural
guarantees rather than hopes — the cardinal "no data loss" requirement. Ships **off by default**
(`smalltalk.format.*`) and needs no `gst`.

## Approach
- **Pure core** `server/src/format/formatter.ts` (no `vscode` import; runs under `tsx`): `format(text,
  tokens, ast, options) → string`. Walks the significant-token stream in order, emitting each token's exact
  text with computed whitespace. A depth model derived from the AST/bracket structure drives indentation; a
  `(prevKind, nextKind)` spacing table drives inter-token spacing; cascade/keyword-wrap decisions come from
  the AST node spanning the current tokens. Reuses `parser/token.ts` `TokenKind` + `ast.ts` `NodeKind`.
- **Skip-on-error**: identify clean vs `Error`-node spans from the AST; never emit whitespace decisions
  inside an unparseable region — copy those bytes through untouched. A fully-unparseable file → return input
  unchanged (caller emits `[]`).
- **Provider** `server/src/providers/formatting.ts`: `formatDocument` / `formatRange` / `onType`, each
  reusing `parseCache` (tokens+AST) and returning `TextEdit[]` via a **minimal diff** (line-granular edits;
  whole-doc = single replace only if changed; range = replace the clamped whole-line span). On-type is a
  thin, local variant (dedent the just-typed `]` line; indent after `[`; keep cascade alignment).
- **Config gate**: read `smalltalk.format.enable` (+ `indentSize`, `cascades`, `keywordWrap`) in the
  `didChangeConfiguration` handler and per-request (the config gotcha: pull in handler, not only at init).
  Disabled → `[]`. Indent char/width also honor the request's `FormattingOptions` (insertSpaces/tabSize).
- **Wiring** `server.ts`: advertise `documentFormattingProvider`, `documentRangeFormattingProvider`,
  `documentOnTypeFormattingProvider: { firstTriggerCharacter: ']', moreTriggerCharacter: ['\n', ';'] }`.
- **Client** `package.json`: declare `smalltalk.format.*` config. (Format Document/Selection/onType are
  built-in editor commands that route to the provider — no new client command needed.)

## Steps
1. **ADR-0005** + spec/gate already done → record the whitespace-rewriter decision and the AC2 "reprint"
   reinterpretation; link from spec/plan.
2. **Acceptance Harness (RED first)**: write the failing property test `server/test/format.property.test.ts`
   (idempotence + token-stream invariance), the failing handshake assertions (3 capabilities; `[]` when
   disabled), the failing output-eval `evals/datasets/formatting/`, and the failing e2e in
   `client/test-e2e/US-416.acceptance.test.js`. Prove RED.
3. **Core engine** `format/formatter.ts`: depth model + spacing table + blank-line collapse → pass the
   spacing/indent goldens + idempotence on simple inputs.
4. **Cascade alignment + keyword wrapping** behind `cascades`/`keywordWrap`; extend goldens.
5. **Skip-on-error** span handling → malformed-input goldens/unit return input unchanged.
6. **Provider + minimal-diff TextEdits** `providers/formatting.ts`; **wire** capabilities in `server.ts`;
   **config** in `package.json`; gate on `enable`.
7. **Drive all layers GREEN**: property tests over the 122-file kernel corpus, output eval, handshake, e2e.
8. **Manual-QA workspace** `specs/US-416-Formatting/manual-qa-workspace/` (create it — `new-story.sh`
   doesn't); run the matrix; fill `verification.md`.
9. **Doc-rot sweep** (deferred to release with the version bump, but stage the status edits): user-stories
   US-416 → Done, EPIC-004, ROADMAP, README features, CLAUDE.md, CHANGELOG.

## Dependencies & Risks
- **Depends on** US-411 lexer/parser/AST + `parseCache`; VS Code formatting APIs.
- **Risk: data loss** → mitigated by ADR-0005 (no re-synthesis) + the token-invariance property test as a
  hard CI gate; malformed regions are copied through, never reformatted.
- **Risk: idempotence bugs** → normal-form derivation + a property test asserting the fixed point over the
  corpus; treat any non-fixed-point as a release blocker.
- **Risk: 1.0 slip** (story is L + droppable) → all three capabilities share one engine; if on-type proves
  costly it can be the last thing cut to 1.1 without touching document/range.
- **Risk: range-formatting mid-expression surprises** → snap ranges to whole lines / statement starts.

## Verification
- **Property unit** `server/test/format.property.test.ts`: idempotence + token-stream invariance over kernel
  corpus + fuzzed inputs (AC3, AC5 malformed).
- **Output eval** `evals/datasets/formatting/` (`npm run eval`, 3-OS CI): canonical goldens (spacing, indent,
  cascade align, keyword wrap, blank-line collapse, comment/blank preservation) (AC2/AC5).
- **Handshake** `server/test/handshake.test.mjs`: 3 capabilities advertised; edits when enabled, `[]` when
  disabled (AC4).
- **Acceptance harness (TDD e2e):** user-observable ACs (AC1 range/on-type, AC2 doc changes buffer, AC4
  no-op when disabled) pinned by failing tests in `client/test-e2e/US-416.acceptance.test.js` *before*
  implementation (red → green). Routing recorded in `requirements-validation.md` §3.5.
- **Manual QA**: `manual-qa-workspace/` matrix in the Extension Host (real source + clean VSIX).
