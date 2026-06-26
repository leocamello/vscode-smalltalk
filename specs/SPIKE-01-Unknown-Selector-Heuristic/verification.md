# SPIKE-01 — Go/No-Go Memo (Unknown-Selector Heuristic)

**Type**: Research spike outcome (the deliverable IS this memo + the corpus report)
**Decision date**: 2026-06-26 · **Owner**: Leonardo Nascimento

---

## Recommendation: **SHELVE** (do not ship a published linter now) — *reconsider* after cartridge hardening

The decision gate (spec §2) is **adopt only if the false-positive rate is effectively zero**. On a real
corpus the gate is **not** at zero false positives, and **closed-world coverage is low (~27%)** — itself a
kill signal (AC4). The gate *logic* is sound and found genuine bugs, but it inherits the cartridge's
completeness, and today the cartridge under-captures primitive + class-side methods. **Park the code
(flagged off, no diagnostics); revisit only after the two conditions below are met.**

## What was built (AC1, AC2)

- `server/src/diagnostics/unknownSelectorGate.ts` — the pure §4 gate (`evaluateMissingSelector` /
  `shouldEmitMissingSelectorWarning`) with the single-emit truth table and **all six §5 escape hatches**
  (perform-family, reflective allowlist, custom DNU, proxy/mock name, incomplete/extension table, opt-out).
- `server/src/diagnostics/cartridgeClassWorld.ts` — the closed-world resolver over **cartridge ∪
  workspace**, modelling two facts that dominate correctness:
  1. **Metaclass protocol** — a class object also responds to `Behavior`/`Class` instance methods
     (`new`, `name`, `category`); without this, `Foo new` is itself a false positive.
  2. **`self` subclass closure (Template Method)** — `self` in an *abstract* class can be any concrete
     subclass, so the understood-set unions descendants' own methods.
- `scripts/spike-unknown-selector.ts` — the corpus harness (output saved to `corpus-report.txt`).
- `server/test/unknownSelectorGate.test.ts` — 16 unit tests pinning the truth table + every hatch + the
  cartridge Template-Method / metaclass behaviour. **It ships nothing**: nothing imports the gate from
  `server.ts`, so esbuild tree-shakes it out of the bundle (verified: `dist/server.js` unchanged).

## Corpus results (AC3)

Corpus: **GST 3.2.5 kernel** (122 files, **21,711 sends**) + **learning-smalltalk** (2 files, 54 sends).
Driven over the bundled GST 3.2.5 cartridge ∪ the parsed workspace, no `gst`.

| Metric | Value |
|---|---|
| Closed-world coverage (receiver resolved) | **6,334 / 21,765 = 29.1%** |
| Table-consulted (definite understood/emit) | **5,886 / 21,765 = 27.0%** |
| Open-world (Unknown receiver → silent) | **70.8%** |
| **Emits — naive `self` resolution** | **58** (all false positives) |
| **Emits — with the `self` subclass-union fix** | **12** |

**The subclass-union (Template Method) insight is essential**: without it, abstract classes alone
(`Float>>asFraction` sending `self exponent`, implemented on `FloatD`/`FloatE`/`FloatQ`) generate 46 false
positives. With it, 58 → 12.

### Triage of the 12 remaining emits

**Genuine catches (≈4) — the heuristic working:**
- `Object>>checkIndexableBounds:put:` → `self primtiveFailed` — a **confirmed misspelling** of
  `primitiveFailed` (cf. `Object.st:1301`/`:1345`). A real latent typo in the GST 3.2.5 kernel.
- `WeakKeyDictionary class >> postLoad` → `self primSize` / `self primAt:` (×3) — a **class-side**
  method sends instance-only selectors (`primSize`/`primAt:` are defined instance-side at
  `WeakObjects.st:457`/`:462`). A cross-side DNU-if-invoked (dead/buggy code).
- (`ArchiveFile>>release` → `self primUnlink:` — `primUnlink:` is defined only on `File`, a *sibling* of
  `ArchiveFile`'s ancestor `FileWrapper`; likely a real cross-class assumption.)

**False positives (≈7-8) — cartridge-completeness gaps, NOT logic flaws:**
- `ObjectDumper` → `self nextSignByte` / `nextInt64` (×4) — **VM primitives** with no static source
  definition; statically uncapturable.
- `PackageLoader class >> canLoad:` → `self extractDependenciesFor:ifMissing:` — **defined**
  (`PkgLoader.st:136`) but the cartridge mis-sides it (`PackageLoader` is class-side-only, `inst#0`).
- `SecurityError for:` — an exception class-side constructor the cartridge didn't capture.
- `URL>>decodedFile` → `self file` — a likely instance-variable accessor the cartridge missed.

**Precision is therefore NOT zero-FP**: ~7-8 false positives on pristine kernel code (~0.035% of sends,
but **> 0** — and a single squiggle on valid code is the uninstall risk the spec warns of).

## Why shelve (AC4)

1. **Zero-FP bar unmet** — ~7-8 false positives remain, every one on valid, shipping code.
2. **Low coverage** — only ~27% of sends reach a definite verdict; 71% have an Unknown (open-world)
   receiver the heuristic cannot speak to. Even at zero FP the feature would be quiet. Per the gate, *low
   closed-world coverage is itself a shelve signal*.
3. **The residual FPs are the cartridge's, not the gate's** — they vanish only by capturing primitives +
   class-side methods + fixing a few class chains, which is a separate body of work.

## Reconsider conditions (the path back)

Re-open an unknown-selector **feature** story only when **both** hold:
- **(a) Cartridge completeness** — the GST cartridge captures primitive-backed and class-side methods (and
  correct class-side method tables), driving corpus false positives to ~0; and
- **(b) Scope restriction** — ship only the highest-confidence slice (e.g. `self`-sends in *concrete*
  classes with a complete table; default severity `Hint`; opt-out; allowlist), measured back to zero FP on
  this same harness.

Until then the gate stays parked (this branch / git history), flagged off, ships nothing.

## Side value (do regardless)
- Report `Object>>checkIndexableBounds:put:` `primtiveFailed` (and the `WeakKeyDictionary class>>postLoad`
  cross-side sends) **upstream to GNU Smalltalk** — real latent bugs the spike surfaced.

---

## Checklist
- [x] **AC1** — §4 gate implemented behind a flag, no published diagnostics (tree-shaken from the bundle).
- [x] **AC2** — all six §5 escape hatches implemented + unit-tested.
- [x] **AC3** — corpus report: precision (triaged), closed-world coverage (`corpus-report.txt`).
- [x] **AC4** — written go/no-go recommendation (this memo): **SHELVE, reconsider after cartridge hardening**.
- [x] `npm run lint` / `check-types` / `test:parser` (incl. 16 gate tests) green; bundle unaffected.
- [x] PO sign-off: _pending review of this memo._
