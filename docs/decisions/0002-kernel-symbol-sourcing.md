# ADR-0002: Kernel symbol sourcing — installed-first, bundled-fallback; neutral model + per-dialect source adapters

* **Status:** Accepted
* **Date:** 2026-06-21
* **Deciders:** Leonardo Nascimento
* **Relates to:** [ADR-0001](0001-typescript-bundled-lsp-server.md) (TypeScript bundled LSP),
  Constitution Principle IV (*Dialect Agnostic*); realised by US-413.

## Context

US-413 adds completion for standard kernel-library selectors/classes, not just the user's
workspace symbols. Those kernel symbols (`Object`, `Collection`, `String`, …) do not live in
the user's `.st` files — they live in a Smalltalk *installation*. This forces a sourcing
decision, complicated by two project realities:

* **ADR-0001** mandates that language intelligence is **fully functional without `gst`** — a
  deliberate differentiator. A "read the installed runtime" approach (how vscode-java/JDT and
  vscode-csharp/Roslyn work — they index the configured JDK / .NET SDK metadata at runtime)
  would give *nothing* to a user who just installed VS Code and our extension with no Smalltalk
  yet. That first-run user is a primary use case.
* **Constitution Principle IV** requires we stay dialect-agnostic so Pharo/Squeak/Tonel can be
  added later. GST ships its kernel as readable `.st` source (e.g. `…/share/smalltalk/kernel`);
  image-based dialects (Pharo/Squeak) keep their base library inside a serialized **image**, not
  loose source — so "parse a source directory" does not generalise.

A static, single-version bundled index alone risks **misleading the user** ("it completed, so it
must be installed and runnable") and pins us to one GST version.

## Decision

Kernel symbols come from a **precedence chain**, exposed as a sourcing strategy, over a
**dialect-neutral index model** fed by **per-dialect source adapters**.

1. **Tiers (highest-confidence first):**
   * **Workspace** (US-412) — always indexed; the user's own code.
   * **Kernel tier** — exactly *one* active source, chosen by
     `smalltalk.completion.kernelLibrary = auto | bundled | off`:
     * `auto` (default): discover an installed GST kernel dir and index it **live**; else fall
       back to the **bundled** index.
     * `bundled`: force the shipped reference index.
     * `off`: no kernel tier.
   * Mixing installed + bundled is disallowed (would duplicate/confuse). The bundle only fills the
     gap when nothing installed is found.

2. **Neutral index model** (`vscode`-free, library-agnostic):
   `{ dialect, library, version, source, classes: { name → { superclass?, instanceSelectors,
   classSelectors } } }`, selector = `{ selector, arity }`. **Facts only** — names/arities/
   superclasses, never method-comment prose (LGPL-2.1; see Licensing).

3. **Source adapters** produce that model; the completion provider only ever consumes the model:
   * **GST file adapter** (now): parse a kernel **directory** of `.st` with our existing parser.
     The *same* indexer serves both the bundled corpus and a live installed dir. Its discovery
     override is `smalltalk.completion.kernelPath` (a directory).
   * **Image-based adapter** (future, Pharo/Squeak): no source dir — build the index by
     **reflective export** (run the dialect VM headless against its image, dump
     `allSubclasses`/`selectors`/arity to the same JSON), or ship that export as a `bundled`
     index. Introduces its **own** config (`imagePath`/`vmPath`); does **not** overload
     `kernelPath`. A richer "query a live image" mode is a possible later enhancement.

4. **Honesty / anti-confusion (so a suggestion is never mistaken for guaranteed availability):**
   * Completion items carry **provenance** (workspace / installed / bundled-reference).
   * The resolved kernel identity is shown **ambiently** in a status-bar item
     ("Smalltalk kernel: bundled (gst 3.2.5)" / "installed" / "off"); a one-time notice on first
     fallback to the bundle. Identity lives in the index header, *not* in the enum literal — hence
     `bundled` (a strategy) rather than `gst-3.2.5` (an artifact we will bump).

5. **`smalltalk.dialect` axis** is deferred: when more than one bundle ships, a separate
   (auto-detected) dialect selector chooses *which* library; `kernelLibrary` stays the orthogonal
   *sourcing* axis. `bundled` is forward-compatible with this.

## Engine boundary (build our own Roslyn/JDT? spin out?)

We will **not** set out to build a Smalltalk "Roslyn/JDT," and **not** spin the engine into a
separate project now. Rationale:

* Smalltalk's compiler-as-a-service already exists — **the image** (self-hosting, reflective).
  The long-term win is our lightweight, no-runtime TS engine **plus optional delegation to a live
  image** when present, not reinventing a compiler.
* There is exactly one consumer (this extension); a separate repo adds release/versioning cost for
  no current benefit. Keep a **modular monolith**: `server/src/parser` + the new
  `KernelIndex`/adapters stay a clean, `vscode`-free, dialect-pluggable core — **designed for
  extraction** but not extracted. Trigger to extract: a real second consumer or partner dialect.

## Consequences

* **Positive:** first-run/no-`gst` users still get kernel completions (honours ADR-0001); users
  with GST installed get version-correct completions from their *actual* kernel; the neutral model
  + adapter seam makes new dialects an additive change (Principle IV); provenance/status keep the
  experience honest; reusing our `.st` parser makes the installed path cheap (no binary-metadata
  reader, unlike JDT/Roslyn).
* **Negative:** US-413 grows (bundled generator **and** live installed indexing **and**
  provenance/status UX), enlarging the 0.5.0 milestone (~4 PR slices vs 3).
* **Follow-up:** US-413 ACs amended (AC5 → `auto|bundled|off`; new AC6 installed indexing, AC7
  provenance/status). A future story covers an image-based dialect adapter (Pharo/Squeak) and the
  `smalltalk.dialect` axis. Hover (US-415) revisits shipping kernel comment prose with attribution.

## Licensing

GNU Smalltalk kernel sources are LGPL-2.1. The index stores **facts** (class names, superclass
links, selector names + arity) — not the creative method-comment prose. The generator/adapters
must never copy comment text or method bodies into the index; a test asserts the index carries no
free-text fields. Shipping comment prose (with attribution) is deferred to the hover story (US-415).
