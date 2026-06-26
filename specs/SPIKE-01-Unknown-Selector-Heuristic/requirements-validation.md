# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins  
**Type**: Requirements Quality Gate  
**US**: SPIKE-01 — Unknown-Selector Heuristic (research spike) · **Validated**: 2026-06-26

---

## Section 1: Constitution Gates (Mandatory)
- [X] **Native Look & Feel**: N/A — a measurement spike. **Ships nothing**: no published diagnostics, no
  UI, no command (spec §3). The gate is a pure predicate, tree-shaken from the bundle.
- [X] **Zero Config**: N/A — runs offline over the bundled cartridge ∪ a parsed corpus; no `gst`, no setting.
- [X] **Protocol First**: N/A — no LSP surface by design (if ever adopted, the feature would publish
  `textDocument/publishDiagnostics`).
- [X] **Robustness**: Pure predicate; the gate's whole posture is bias-to-silence (open world ⇒ silent);
  never throws (spec §4/§8).
- [X] **Dialect Agnostic**: The `ClassWorld` resolves over any cartridge ∪ workspace; the gate has no
  GST-specific logic beyond the `Object` DNU root (parameterised via `rootDnuClass`).

## Section 2: Specification Completeness
- [X] Goals and Non-Goals explicitly listed (spec §2/§3 — measurement only, ships nothing).
- [X] User story in standard format — research spike framing (gate any future linter on evidence).
- [X] Acceptance criteria defined and testable (AC1 gate/truth-table, AC2 six hatches, AC3 corpus
  precision+coverage, AC4 go/no-go memo).
- [X] Edge cases identified (spec §8 + the §4 truth table): abstract/Template-Method self-sends, proxy/DNU
  forwarding, perform:/reflective, extension/incomplete tables, un-indexed third-party receivers.
- [X] Dependencies listed: US-430 `cartridgeLoader.methodTableOf`, US-411 AST/symbols, US-423 `forEachSend`.

## Section 3: Technical Design
- [X] API/Command contracts: `evaluateMissingSelector(ctx, world)` / `shouldEmitMissingSelectorWarning`;
  the `ClassWorld` resolution interface (spec §4).
- [X] Data structures: the §4 truth table; the `ClassWorld` (respondsTo / respondsToSelf subclass-closure /
  dnuDefiner / metaclass protocol); the corpus harness tally.
- [X] Error-handling strategy: bias-to-silence; `undefined` table ⇒ open world ⇒ silent.
- [X] Testing strategy: unit (`unknownSelectorGate.test.ts`) + the corpus harness as the measurement.

## Section 4: Validation Result
- [X] **PASS — proceeded to implementation.** Outcome recorded in `verification.md` (go/no-go memo):
  **SHELVE** — the zero-FP bar is unmet (~7-8 cartridge-gap false positives on the GST kernel) and
  closed-world coverage is low (~27%). Gate code parked, flagged off; reconsider after cartridge hardening.
