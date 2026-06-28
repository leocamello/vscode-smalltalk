# ADR-0005: Formatter is a whitespace-only token-stream rewriter, not an AST pretty-printer

* **Status:** Accepted
* **Date:** 2026-06-27
* **Deciders:** Leonardo Nascimento
* **Relates to:** US-416 (Formatting, EPIC-004 → ~1.0). Realizes Constitution principles on **Robustness**
  (never corrupt user code) and **Native Look & Feel** (standard VS Code formatting providers). Builds on
  US-411 (lexer/parser/AST).

## Context

US-416 asks for "conservative, idempotent code formatting" so a developer can "normalize style without
risking my code." The user story's own risk note is blunt: **data loss = trust loss**; idempotence is
mandatory. Two of its acceptance criteria pull in different directions if read naively:

* **AC2** — "`documentFormatting` **reprints from the AST**, preserving comments and blank lines,
  normalizing only indentation and keyword-message wrapping."
* **AC3** — `format(format(x)) === format(x)` **and** the **lexer token stream is unchanged** before/after.

A literal "reprint from the AST" means re-synthesizing source text from `ast.ts` nodes. But the US-411 AST
is deliberately **not lossless**:

* **Comments are not structural** — they're separate `TokenKind.Comment` tokens recovered out-of-band
  (`parser/comments.ts`), not children of the nodes they sit between. A pretty-printer must re-attach them by
  a nearest-offset heuristic, which is lossy at the edges (a comment between two statements, a trailing
  comment, a comment inside a literal array) — exactly the data loss the story forbids.
* **Blank lines carry no node.** Author intent ("a blank line separates these two methods") would have to be
  reconstructed from offsets.
* The parser is **error-tolerant**: it emits `Error` nodes for fragments it can't parse. You cannot reprint
  an `Error` node — so an AST pretty-printer either drops or mangles unparseable regions.
* Re-synthesis makes the **AC3 token-invariance** guarantee something you *audit after the fact* rather than
  something true *by construction*.

## Decision

The formatter is a **whitespace-only token-stream rewriter**, not an AST pretty-printer.

It walks the US-411 **significant-token stream in order** (identifiers, selectors, literals, brackets,
periods, semicolons — **and comment tokens**), emits **each token's exact source text byte-for-byte**, and
computes only the **whitespace between consecutive tokens**: indentation (block/container/method depth),
inter-token spacing (a `(prevKind, nextKind)` table — one space around binary selectors and after a keyword
colon, none before `.`/`;`/closers, none after openers), blank-line-run collapse (2+ → 1), cascade-segment
alignment, and long-keyword-message wrapping. The **AST is consulted only for structure** (how deep are we,
which tokens form a cascade segment, does this keyword send exceed the wrap width) — never to regenerate
text.

**We reinterpret AC2's "reprint from the AST" as "whitespace-normalize, AST-informed."** The intent of AC2
(normalize indentation + keyword wrapping, preserve comments + blank lines) is fully met; only the
*mechanism* (re-synthesis) is rejected, because it conflicts with AC2's own "preserving comments and blank
lines" and with AC3.

### Consequences

* **Token-stream invariance (AC3) is structural, not audited.** The output is a permutation-free copy of the
  input token texts with different inter-token whitespace; no token can be added, dropped, split, or merged.
* **Idempotence (AC3) is provable.** All whitespace is derived from a normal form (depth + spacing table +
  wrap decision computed from the normalized width), so a second pass is a fixed point. Pinned by a property
  test over the 122-file kernel corpus + fuzzed inputs — a hard CI gate.
* **Comments and blank lines survive** because they are preserved tokens / preserved single-blank runs, not
  reconstructed artifacts. A comment between two tokens stays between those two tokens; we do not re-flow
  prose.
* **Malformed code is never corrupted.** Spans covered by `Error` nodes (or otherwise unparseable) are
  **copied through untouched**; a fully-unparseable file yields no edits. Formatting is best-effort and
  strictly non-destructive (Non-Goal in spec §3).
* **Less "canonical" than a full pretty-printer — but the gap is opt-in, not architectural.** By default we
  will not re-break a hand-wrapped expression within budget or collapse a multi-line message the author
  chose to spread. The opinionated touches are settings: `cascades` (align cascade segments), `keywordWrap`
  (wrap long keyword sends), and **`blockStyle: expand`** — which reflows method/class/multi-statement-block
  bodies one-statement-per-line (the "expanded" aesthetic a full pretty-printer would impose). Crucially,
  `expand` is implemented as **AST-derived structural forced-breaks over the existing token stream**, *not*
  re-synthesis: it adds whitespace at statement/body boundaries the parser already identifies, so it inherits
  the idempotence + token-invariance guarantees (verified over the 122-file kernel corpus in both modes).
  This is the payoff of the decision: we can offer the pretty-printer's *look* without taking on its
  data-loss risk. The whole feature still ships **off by default** (`smalltalk.format.enable`, AC4) for ≥1
  release, and `blockStyle` defaults to `preserve`.
* **Dialect-agnostic.** The rewriter depends only on the layered lexer/AST contract (`TokenKind`/`NodeKind`),
  not GST-specific lexemes, so future cartridge dialects reuse it.

## Alternatives considered

* **Full AST pretty-printer (re-synthesize from nodes).** Rejected: lossy comment/blank re-attachment,
  cannot render `Error` nodes, makes token-invariance an after-the-fact audit — directly at odds with the
  "no data loss" mandate. (See Context.)
* **External formatter / `gst`-based formatting.** Rejected: violates the zero-runtime-dependency posture
  (ADR-0001); `gst` has no offline formatting entry point we'd want to depend on.
* **Pure regex/line-based reindenter (no parser).** Rejected: cannot reliably track block depth, cascades,
  or keyword sends through strings/comments/literal arrays; would mis-handle exactly the cases that matter.
