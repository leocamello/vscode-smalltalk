# ADR-0001: Language server in TypeScript, bundled with the extension

* **Status:** Accepted
* **Date:** 2026-06-13
* **Deciders:** Leonardo Nascimento
* **Supersedes:** EPIC-004 (original), US-401, US-402, US-403

## Context

The Phase 2 plan called for a **language server implemented in Smalltalk**, talking
to the TypeScript client over hand-rolled JSON-RPC (see the original EPIC-004 and the
research stories US-401/402/403). Revisiting this before starting Phase 2:

* GNU Smalltalk has had **no stable release since 2013 (v3.2.5)** and is hard to install
  on modern systems.
* There is no mature JSON-RPC / LSP library in the GNU Smalltalk ecosystem; we would be
  building protocol plumbing from scratch on an unmaintained platform.
* Requiring a working `gst` for *any* language intelligence would exclude most users
  (issue #1 — a user asking for autocompletion — has been open since 2019).
* Pharo already has a maintained Smalltalk-based LSP/VS Code extension; duplicating that
  approach adds no differentiation for our file-based GNU Smalltalk audience.

## Decision

Implement the language server in **TypeScript** (`vscode-languageserver-node`), **bundled
inside the VSIX**. It parses `.st`/`.gst` files with a hand-written, error-tolerant
Smalltalk parser and is **fully functional without any external Smalltalk installation**.

`gst` (GNU Smalltalk) is spawned only as an **optional external tool**:
* the "Run Current File" command (US-301), and
* opt-in compile diagnostics (US-414, default off).

The parser is layered — a core ANSI-Smalltalk layer plus pluggable container formats
(GST chunk `!` and GST brace/class syntax) — so other dialects (e.g. Pharo/Tonel) can be
added later without a rewrite (honours Constitution Principle IV, *Dialect Agnostic*).

## Consequences

* **Positive:** works out of the box for every user; one language/toolchain (TypeScript)
  for client + server; portable across OSes; no dependency on a dead platform; directly
  unblocks completion/navigation/diagnostics.
* **Negative:** we must write a Smalltalk parser ourselves (US-411, ~XL effort) rather than
  reusing a Smalltalk image's compiler; semantic fidelity is parser-based, not image-based.
* **Follow-up:** EPIC-004 retitled "Language Intelligence — TypeScript LSP"; US-401/402/403
  closed (GitHub #14/#15/#16); replaced by US-410–US-416 (GitHub #22–#28); Constitution
  amended to v1.1.0; `docs/product/high-level-plan.md` §4 revised.

## Update (2026-06-21) — container-format seam deferred

The "layered (core ANSI + pluggable GST chunk/brace container formats)" claim above describes the
**capability**, not yet a built **seam**. As shipped (US-411…US-413), the brace and chunk container
formats are handled **inline in `server/src/parser/parser.ts`** — there is no separate
`server/src/parser/containers/{gstBrace,gstChunk,index}.ts` nor a `ContainerFormat` interface (as the
genesis `plan.md` sketched). The parser *does* handle both GNU Smalltalk formats; what is deferred is
the **pluggability** that lets a *second* dialect (Tonel/Pharo/Cuis) plug in without touching the core.

This is a conscious **YAGNI deferral**: extracting the seam now, with a single dialect, would be
speculative. It is tracked as **US-418 (Container-Format Seam / Dialect Door)**, to be built when the
first additional dialect actually lands. Constitution Principle IV (*Dialect Agnostic*) remains the
intent; this note keeps the architecture description honest until US-418 realises the seam.
