# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins
**Type**: Requirements Quality Gate
**Story**: US-416 — Formatting

---

## Section 1: Constitution Gates (Mandatory)
- [x] **Native Look & Feel**: Uses standard VS Code formatting APIs — `documentFormattingProvider`,
  `documentRangeFormattingProvider`, `documentOnTypeFormattingProvider`. Hooks straight into
  `editor.formatOnSave`/`formatOnType` and the Format Document/Selection commands. No bespoke UI.
- [x] **Zero Config**: Sensible defaults (`indentSize: 4`, `cascades: align`, `keywordWrap: 100`); tabs vs
  spaces inherit from `editor.insertSpaces`/`tabSize`. The one deliberate non-default is `enable: false`
  (AC4 mandates off-for-a-release for a destructive-by-nature feature) — a safety choice, not config debt.
- [x] **Protocol First**: Pure LSP — three standard `textDocument/*Formatting` requests returning
  `TextEdit[]`. No out-of-band channel.
- [x] **Robustness**: Front end never throws; unparseable regions → no edits (never corrupts code);
  disabled → `[]`. Idempotence + token-stream invariance are property-tested CI gates.
- [x] **Dialect Agnostic**: The rewriter consumes the layered US-411 lexer/AST (core ANSI + GST
  chunk/container) and a spacing/indent model that is structural, not GST-lexeme-specific. New dialects
  reuse the engine via the same token/AST contract; no GST-only assumptions in `format/`.

## Section 2: Specification Completeness
- [x] Goals and Non-Goals explicitly listed? (§2 / §3 — incl. the hard "no AST re-synthesis" non-goal.)
- [x] User stories in standard format? (§4 — role/capability/benefit.)
- [x] Acceptance scenarios (Given/When/Then) defined? (§4 — six scenarios.)
- [x] Edge cases identified? (§4 — empty file, trailing newline, CRLF/LF, mixed indent, mid-line comments,
  chunks/containers, pragmas/arrays.)
- [x] Dependencies listed? (§4 — US-411 lexer/parser/AST, parseCache, VS Code formatting APIs.)

## Section 3: Technical Design
- [x] API/Command contracts defined? (§5 — three providers, capability advertisement incl. on-type trigger
  chars, `smalltalk.format.*` config keys + defaults.)
- [x] Data structures defined? (§5 — pure `format/formatter.ts` core over token stream + AST; spacing table
  keyed by `(prevKind, nextKind)`; depth counter; `TextEdit[]` output via minimal diff.)
- [x] Error handling strategy defined? (§5/§7 — never throws; skip `Error`-node regions; disabled → `[]`.)
- [x] Testing strategy (Unit vs Integration) defined? (§5 — property unit tests, output eval, handshake,
  e2e; routed in §3.5 below.)

## Section 3.5: Acceptance Harness (TDD e2e plan)
**AC routing** (user-observable → e2e; internal contract/invariant → unit or `evals/` golden; LSP protocol
shape → handshake):

| AC  | Nature | Routed to |
|-----|--------|-----------|
| AC1 (range + on-type behavior) | user-observable | **e2e** `client/test-e2e/US-416.acceptance.test.js` (on-type dedent on `]`; range touches only selection) |
| AC2 (document whitespace-normalize, preserve comments/blanks) | user-observable + golden | **e2e** (buffer changes on Format Document) **+ output eval** `evals/datasets/formatting/` (canonical goldens incl. comment/blank preservation) |
| AC3 (idempotence + token-stream invariance) | internal invariant | **unit property tests** `server/test/format.property.test.ts` over kernel corpus + fuzzed inputs |
| AC4 (off by default; enable gates edits) | user-observable | **e2e** (no-op when disabled) **+ handshake** (`[]` when disabled, edits when enabled) |
| AC5 (no `gst`; malformed → no edits; CI 3-OS) | internal contract | **unit** (malformed input → `[]`) **+ output eval** in CI matrix |

- [x] Each AC is routed to a verification layer (table above).
- [x] User-observable ACs (AC1, AC2, AC4) will be pinned by acceptance tests **written before
  implementation** (red → green) in `client/test-e2e/US-416.acceptance.test.js`; the AC3 invariant is
  pinned by a red property test first.
- [x] The story **has** a user-observable surface — the scaffolded e2e stub is kept and filled.

## Section 4: Validation Result
- [x] **PASS** — Ready for implementation (Plan phase next).

**Notes**: New **ADR-0005** (formatter = whitespace-only token-stream rewriter, not an AST pretty-printer)
must be authored alongside the plan — it records the AC2-wording reinterpretation ("reprint" → "whitespace
normalize") that resolves the AC2/AC3 tension, per the artifact-hierarchy rule (record decisions as ADRs
and propagate in the same change).
