# Specification: Signature Help

**ID**: US-425
**Feature**: `textDocument/signatureHelp` — keyword-message signature help with active-parameter tracking, over the two-tier (workspace ∪ cartridge) index
**Status**: Draft
**Owner**: Leonardo Nascimento
**Created**: 2026-06-26
**Architecture**: [ADR-0002](../../docs/decisions/0002-kernel-symbol-sourcing.md), [ADR-0003](../../docs/decisions/0003-cartridge-resolution.md)

## 1. Overview
The keyword-message companion to completion (US-413): as the user types a keyword send
(`dict at: key put: …`), surface the matching keyword **selector signatures** drawn from the
workspace **∪** the active kernel cartridge, and **highlight the active parameter** — the keyword
whose argument the cursor is currently filling. It reuses the same Console index completion consumes
and the same robust token-stream cursor analysis (no AST dependency at an incomplete cursor), so it
works **offline, with no `gst`**. This is parity-plus filler graded honestly: small, native, and the
natural completion of the keyword-message UX.

## 2. Goals
- `textDocument/signatureHelp` for **keyword sends**: at a cursor inside a keyword message, return the
  matching keyword selectors from the workspace ∪ cartridge index as `SignatureInformation[]`.
- **Active-parameter tracking**: the keyword part whose argument the cursor is filling is reported as
  `activeParameter` and rendered as the highlighted parameter region.
- **Honest union** (consistent with US-423): a keyword *prefix* matches every selector that starts with
  the typed keywords (`at:` → `at:`, `at:put:`, `at:ifAbsent:`); provenance is shown per signature; the
  full union is returned, never a single false target.
- Works with **no `gst`** (AC2): the bundled reference floor supplies kernel signatures.

## 3. Non-Goals
- **No unary/binary signature help** — those carry no parameter sequence worth tracking; signature help
  is keyword-only by design.
- **No receiver type resolution** — we do not pretend to know which class the receiver is; we offer the
  lexical union of selectors that match the typed keyword prefix (the US-423 posture).
- **No argument *names*** — the cartridge stores selectors/arity/keywords as facts, not parameter names;
  the highlighted parameter is the **keyword part** (`put:`), not a synthesized `anObject`.
- **No new index** — reuse the workspace symbol index (method selectors) + `kernelService` selectors; no
  cross-reference walk needed.

## 4. User Stories & Acceptance Criteria
**US-425**: As a Smalltalk developer, I want keyword-message signature help as I type, so that I see
selector signatures and the active parameter without navigating away.

- **AC1**: `textDocument/signatureHelp` at a cursor inside a keyword send returns the matching keyword
  selectors from the workspace ∪ cartridge index, with **active-parameter tracking** (the keyword part
  currently being filled is `activeParameter`). A typed keyword *prefix* matches every selector starting
  with it; each `SignatureInformation` carries its provenance; binary/unary sends and non-keyword cursors
  return **null** (no signature help).
- **AC2**: Works with **no `gst`** — kernel signatures come from the bundled reference floor; the path is
  pure (parser/token + index lookups), never throws, returns null on miss.

## 5. Technical Design

### 5.1 The provider (`server/src/providers/signatureHelp.ts`)
Pure, `vscode-languageserver-types`-only (mirrors `completion.ts`). Entry point:

```ts
signatureHelpAt(offset, text, tokens, signatures: SignatureCandidate[]): SignatureHelp | null
```

where `SignatureCandidate { selector, keywords, provenance }` is the merged workspace + active-kernel
keyword-selector set (each provenance-tagged). Steps:

1. **Cursor analysis (token stream, robust at an incomplete cursor).** Scan **backward** from the cursor
   through significant tokens, tracking bracket depth (`(`/`[`/`{`). Skip nested groups (a `)`/`]`/`}`
   increments depth; the matching opener decrements it). At depth 0, collect **`Keyword` tokens** in
   order until a boundary halts the scan — an **unmatched opener**, a statement terminator (`.`, `^`,
   `!`), a cascade `;`, an assignment `:=`, or a `|`. The collected keyword parts are the selector
   typed *so far* (keyword messages are greedy → all same-level parts form **one** selector). Binary
   selectors and other tokens between keyword parts are skipped, not boundaries (they live in args).
2. If **no** keyword part was collected ⇒ return **null** (unary/binary/head cursor — out of scope).
3. **Match** — `activeParameter = typedKeywords.length - 1`. A candidate matches iff
   `candidate.keywords.length >= typedKeywords.length` and its keywords agree positionally with the typed
   prefix. Dedup by selector, best provenance wins (`Workspace ≻ Installed ≻ Bundled`, reusing the
   completion rank).
4. **Package** — one `SignatureInformation` per matching selector: `label` is the selector spelled as its
   keyword parts (`at: put:`); `parameters` map each keyword part to its label region so VS Code bolds the
   active one; `documentation`/`detail` carries the provenance label. `activeParameter` is set per §5.1.3;
   `activeSignature` prefers a candidate whose keyword count equals the typed length (the fully-typed
   selector), else 0. Sort: provenance, then keyword count, then selector text.

### 5.2 Wiring (`server/src/server.ts`)
- Advertise `signatureHelpProvider: { triggerCharacters: [':', ' '], retriggerCharacters: [':'] }`.
- `connection.onSignatureHelp`: build `SignatureCandidate[]` from `index.all()` method symbols (keyword
  selectors, `Provenance.Workspace`) ∪ `kernelService.selectors()` (keyword selectors, kernel provenance),
  deriving `keywords` from the selector text; call `signatureHelpAt`.

### 5.3 Dependencies
- US-413 workspace index + `kernelService` selectors; US-430 cartridge (the kernel signature source);
  US-411 lexer (the token stream). No US-423 cross-reference tier needed.

### 5.4 Testing
- **Unit (`test:parser`)**: `signatureHelp.test.ts` — cursor analysis at a keyword send (active param 0
  after `at:`, 1 after `at: x put:`); prefix matching (`at:` surfaces `at:put:`); nested-arg skip
  (`at: (a foo: b) put:`); cascade/assignment/paren boundaries; **null** for unary/binary/head cursors and
  empty index.
- **Server (`test:server`)**: real bundled server advertises `signatureHelpProvider` and answers
  `textDocument/signatureHelp` at a keyword-send cursor with a known kernel selector + active parameter,
  **no `gst`**.
- **E2E (`test:e2e`)**: `US-425.acceptance.test.js` — `vscode.executeSignatureHelpProvider` at a keyword
  send returns a signature with the expected active parameter.
- **Eval**: `evals/datasets/signature-help/` (output eval over keyword-send cursors; `npm run eval`).

## 6. Manual Verification
Extension Development Host: typing `aDictionary at: 1 put: ` pops signature help showing `at:put:` with the
second parameter active; typing `coll do:` shows `do:`/`do:separatedBy:`…; a workspace keyword method
surfaces with `workspace` provenance; all with no `gst`. Matrix in `verification.md` +
`manual-qa-workspace/`.

## 7. Risks & Limitations
- **No argument names** — keyword parts stand in for parameter labels; honest given the facts-only
  cartridge. If names are wanted later, they would come from a richer cartridge tier, not this story.
- **Union breadth** — a short prefix like `at:` can match many selectors; we rank (provenance, keyword
  count) and rely on VS Code's "1 of N" cycling, never filtering the union (US-423 posture).
- **Graded parity-plus filler** — copyable; the moat is the offline Console beneath it. Small by design.
