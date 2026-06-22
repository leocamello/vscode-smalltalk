# ADR-0003: Cartridge resolution — generate-and-cache preferred, rich frozen floor

* **Status:** Accepted
* **Date:** 2026-06-22
* **Deciders:** Leonardo Nascimento
* **Extends:** [ADR-0002](0002-kernel-symbol-sourcing.md) (kernel sourcing precedence) and
  [ADR-0001](0001-typescript-bundled-lsp-server.md) (zero-runtime); Constitution II (*Zero
  Configuration*) & IV (*Dialect Agnostic*). Realized by US-430 and EPIC-006 (US-601/US-603).

## Context

US-430 shipped a **frozen GST Cartridge #01**. The naive way to scale to other dialects — ship a
frozen cartridge per **(dialect × version)** — has two failure modes:

* **Combinatorial explosion** of artifacts to generate, ship, and maintain (VSIX bloat; we become a
  release bottleneck for every new dialect version).
* **Mismatch to the user's actual install** — a single frozen version won't match a user on a
  different point release, and never reflects their **loaded packages / custom classes**. Completions
  and hover can be subtly wrong *for their case*.

Three facts shape the decision:

1. The reflective exporter (`scripts/export-gst-cartridge.st`) **is already the per-user generator** —
   same schema, same script; we merely ran it at build time. "Frozen vs generated" is a *deployment*
   choice over one mechanism, not two mechanisms.
2. Two separable costs: the per-**dialect** *adapter* (unavoidable, and identical whether it runs at
   build- or user-time) vs the per-**(dialect × version)** *frozen data blob* (avoidable via
   generation + caching). The explosion fear is the latter only.
3. **Hard constraint (ADR-0001):** language intelligence must work with **no runtime and no install** —
   the zero-install first-run user is a primary persona and our differentiator. Generation therefore
   cannot be the *only* source. We also need a deterministic, known cartridge for the `evals/` harness,
   CI, and bug reproduction.

## Decision — the Cartridge Resolution Chain

Generalize ADR-0002's precedence into a three-tier chain, evaluated per dialect:

1. **Tier 0 — Workspace (always):** the user's own parsed code (US-412).
2. **Tier 1 — User cartridge (preferred):** generated **once** from the user's *actual* install and
   **cached** (key = `dialect + version + source content-hash`; stored in extension global storage;
   invalidated/regenerated when the install changes). The adapter is chosen by what the dialect ships:
   * **Source-shipping dialects (GST):** the **no-runtime** static source-directory parse
     (`indexKernelDirectory`) — cheap, needs no VM.
   * **Image-only dialects (Pharo/Squeak):** **opt-in** reflective export (headless VM run) with
     explicit consent, a progress notice, and the US-301/US-414 process discipline (timeout, no
     zombies). Never automatic, never required.
3. **Tier 2 — Frozen reference floor (fallback):** a **rich** frozen reference cartridge per *primary*
   dialect, shipped in the VSIX, used when no install is discoverable. It guarantees the
   zero-install/offline experience (ADR-0001) and is the deterministic baseline for `evals/`, CI, and
   support. **Generated in CI from a pinned install** — "just another adapter run", not hand-maintained.

### Floor richness (the sized sub-decision): **rich, not minimal**

The Tier-2 floor carries the **full base-image facts** (GST = 249 classes / 4746 signatures), **not** a
minimal ANSI-core. Rationale: the first-run/no-install user should get the full experience, identical to
today's GST behavior; and a rich floor is the more useful eval baseline. The cost (VSIX size) is bounded
— **one rich floor per primary dialect, not per version**.

### Honesty / provenance

Extend the existing status-bar identity (ADR-0002) to distinguish **floor (frozen reference)** vs
**generated/cached (installed)**, with version, and keep the one-time notice on first fall-back to the
floor. A completion/hover item's provenance never overstates availability.

### Licensing nuance (a Tier-1 bonus)

The shipped Tier-2 floor stays **facts-only** (we redistribute it; GST kernel is LGPL-2.1 — see ADR-0002
Licensing). A **Tier-1 cartridge generated on the user's own machine** may additionally carry **comment
prose** for hover, because nothing is redistributed *by us* — a richer hover with no licensing
constraint. The `carriesProse` header flag is set by the adapter/source accordingly.

## Rejected alternatives

* **Ship a frozen blob per (dialect × version)** — the explosion this ADR exists to avoid.
* **Require a runtime / generation for first-run** — breaks ADR-0001 and our differentiator.
* **A network/CDN cartridge registry now** — against offline-first; adds distribution/supply-chain
  surface; premature. Revisit only if community-contributed cartridges become a real need.

## Consequences

* **Positive:** no version explosion in the shipped set (one rich floor per primary dialect); installed
  users get version-correct, package-aware, and potentially prose-rich facts; the zero-install moat and a
  reproducible baseline are *both* preserved; the same exporter/schema serves the build-time floor and
  user-time generation.
* **Negative:** adds a cache + invalidation concern; the reflective adapter needs a *runtime* (on-user-
  machine) mode for image dialects; two resolution paths coexist (already true since ADR-0002); we
  maintain *N adapters* (unavoidable) + *N CI-generated floors*.
* **Follow-ups:** US-430 elevates the installed adapter to **preferred + cached** and keeps the static
  indexer as the no-runtime Tier-1 path; US-601 (Pharo) builds its cartridge via the opt-in reflective
  adapter + cache, with a CI-generated floor; US-603 (multi-cartridge loader) implements this chain
  including cache keying/invalidation; the exporter gains a runtime mode. A dedicated
  "generate-and-cache + invalidation" story may be split out under EPIC-006 when that work starts.
