# Specification (Spike): Unknown-Selector Heuristic — false-positive validation

**ID**: SPIKE-01
**Type**: Time-boxed research spike (2–3 days); gates any unknown-selector feature story
**Status**: Draft
**Owner**: Leonardo Nascimento
**Created**: 2026-06-22

## 1. Overview
The static shadow of `doesNotUnderstand:` — flag a message send whose selector is provably unknown. In a
dynamically typed language an over-eager linter is a virus: a single false-positive squiggle on valid code
is a ten-minute uninstall. This spike implements the gate **behind a flag (no published diagnostics)** and
**measures false-positive rate + coverage on a real corpus** to decide adopt vs shelve.

## 2. Goal & decision gate
**Adopt only if the false-positive rate is effectively zero** on `learning-smalltalk/` + the GST 3.2.5
kernel. Output a go/no-go memo. Low closed-world coverage at zero-FP is also a kill signal.

## 3. Non-Goals
- No published diagnostics, no code actions, no UI — measurement only.
- Not the feature itself; if adopted, a follow-up feature story implements the published linter
  (default severity `Hint`, opt-out, allowlist).

## 4. The gate (Heuristic of Extreme Caution)
`shouldEmitMissingSelectorWarning(selector, receiverNode, workspace, cartridge): boolean`

**Resolve the receiver to a closed-world class** in exactly two high-confidence cases:
- `self` / `super` sent inside a class we have fully indexed (its chain resolves to a cartridge root → we
  own the whole method table), or
- a **literal known-class receiver** (`Array new do:`, `OrderedCollection foo`) whose class is in
  `workspace ∪ cartridge`.
Everything else ⇒ `Unknown`. The resolved **method table** = own ∪ inherited (walked chain) ∪
trait-composed (minus exclusions, plus aliases).

**Truth table — emit in exactly one row:**

| receiverResolved | selectorInResolvedTable | methodParsesClean | escapeHatch | EMIT |
|---|---|---|---|---|
| Unknown | * | * | * | false (open world — receiver could be an un-indexed class) |
| KnownClosedWorld | true | * | * | false (it is understood) |
| KnownClosedWorld | false | false | * | false (don't lint mid-edit / syntax-broken code) |
| KnownClosedWorld | false | true | true | false (an escape hatch fired) |
| **KnownClosedWorld** | **false** | **true** | **false** | **TRUE** |

Even when it fires: severity is `Information`/`Hint`, never `Error`.

## 5. Escape hatches (force silence)
1. **Custom `doesNotUnderstand:`** anywhere in the receiver's chain *other than* the root's default error
   DNU → open world → silent.
2. **`perform:` family** (`perform:`, `perform:with:`, `perform:withArguments:`, …) — silence that send
   unconditionally (computed selectors are invisible statically).
3. **Proxy/forwarding signals** — receiver implements DNU-forwarding, composes a known forwarding trait,
   or (soft, configurable) matches `*Proxy | *Mock | *Stub | *Adapter`.
4. **Incomplete table** — receiver is `kind:'extension'` or from a partial/un-indexed cartridge.
5. **Reflective/meta allowlist** — a tiny curated set (`respondsTo:`, `perform:`, `->`, `value`,
   `instVarNamed:`-style accessors).
6. **Explicit opt-out** — a method/class pragma (`<lint: #ignoreUnknownSelectors>`) or workspace setting;
   off inside unsaved scratch/do-it contexts unless a class context is known.

## 6. Method & deliverables
- Implement the gate over the US-411 AST/symbols + a loaded cartridge (US-430), **flagged off**.
- Run against `learning-smalltalk/` + the GST 3.2.5 kernel; report:
  - **precision** (false-positive count — the bar is 0), with every FP triaged;
  - sample **true catches** (real typos);
  - **closed-world coverage** — fraction of sends that resolve to `KnownClosedWorld` (how often the
    heuristic can even speak).
- **Memo**: adopt → file a follow-up unknown-selector feature story; or shelve with evidence.

## 7. Acceptance Criteria
- **AC1**: gate implemented per §4 truth table, behind a flag, no published diagnostics.
- **AC2**: all six §5 escape hatches enforced.
- **AC3**: corpus report — precision (0 FP bar), sample catches, closed-world coverage.
- **AC4**: written go/no-go recommendation.

## 8. Risks & Limitations
- **False positives = trust death** — hence the gate's bias to silence and the zero-FP bar.
- **Coverage may be low** — if few sends are closed-world (lots of `Unknown` receivers), the feature
  speaks rarely even at zero FP; that is itself a (shelve) signal.
- **Un-indexed third-party packages** are the main FP source — the closed-world gate (receiver fully known)
  is precisely the mitigation; do not relax it.
