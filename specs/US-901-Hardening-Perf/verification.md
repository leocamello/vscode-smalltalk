# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding
**Type**: Implementation Verification
**Story**: US-901 — Hardening & Perf | **Issue**: #112

---

## Section 1: Acceptance Criteria
- [x] **AC1 — Index budget:** `npm run bench` — index **1000 files cold ≈ 219 ms** (budget < 5000 ms) **PASS**. Linear scaling confirmed at 2000 files (≈ 380 ms).
- [x] **AC2 — Completion budget:** completion **p95 ≈ 4.9 ms** (budget < 100 ms) **PASS** (p50 ≈ 2.3 ms, max ≈ 7.2 ms over 200 requests, candidate lists rebuilt per request as the handler does).
- [x] **AC3 — No telemetry:** `server/test/noTelemetry.test.ts` (matcher has teeth + scans server/client sources) green; stance documented in README + CHANGELOG.
- [x] **AC4 — Never-throw:** `server/test/robustness.test.ts` (2000 random inputs + malformed fixtures) + `server/test/providerRobustness.test.ts` (735 provider checks over docs × boundary offsets) — **0 throws**. The one defect found (deep-nesting stack overflow) is fixed.
- [x] **AC5 — Stress limits:** deep-nested blocks/parens/brace-arrays (depth 5000) bounded via `MAX_EXPRESSION_DEPTH` (diagnostic, not overflow); 1 MB source parses < 2 s ceiling; class-rename scan ≈ 108 ms at 1k files (info).
- [x] **AC6 — Bug-bash:** provider × edge matrix run (735 checks). **Triage: 1 defect found → 1 fixed → 0 deferred.** The defect (parser stack overflow on pathological nesting) is fixed and pinned red-first by `robustness.test.ts`; the provider matrix is committed as a durable guard. No self-closing GitHub issue was filed since the sole defect was found-and-fixed in-story (documented here + in CHANGELOG).

### Perf baseline (dev box, `npm run bench`)
```
  index 1000 files (cold)                       219.4 ms  <5000 ms  PASS
  completion p50                                  2.3 ms      —     info
  completion p95                                  4.9 ms   <100 ms  PASS
  completion max                                  7.2 ms      —     info
  class-rename scan (Widget0000, 770 files hit)  108.2 ms     —     info
```
**Decision (measure-first):** the flagged suspect `allWorkspaceFiles()` is **not** a bottleneck (108 ms for a
one-shot, user-initiated rename with a preview; linear). Per the spec's own discipline it is **not** optimized
— avoiding premature-optimization risk to the open-doc-wins / skip-unreadable semantics.

## Section 2: Code Quality
- [x] `npm run lint` passes (client/src, server/src).
- [x] `npm run check-types` passes (strict, no `any` added).
- [x] `npm run test:parser` (incl. robustness/noTelemetry/providerRobustness), `test:server`, `test:client`, and full `npm run eval` (grammar + snippets + 9 datasets) all green.
- [x] JSDoc on the new public surface (`generateCorpus`, the depth guard, the guards).

## Section 3: Constitutional Compliance
- [x] **Native:** no new UI; existing LSP surfaces only.
- [x] **Zero Config:** bench/gen-corpus are dev tools; no new runtime setting; no-telemetry is the default.
- [x] **Robustness:** the front end never throws (fuzz + provider matrix); pathological input is bounded.
- [x] **TDD:** the never-throw fix was pinned red (stack overflow) → green (depth guard); guards written before/with the fix.

## Section 4: Manual Verification
- [ ] `specs/US-901-*/manual-qa-workspace/` matrix run in the Extension Host: deep-nest file → no crash toast, single "Expression nesting too deep" diagnostic, editor responsive; features still work on `Normal.st`; no network traffic in Dev Tools. **(pending — owner to run before release)**
- [x] No errors in the standalone harness runs (bench, tests).

## Section 5: Sign-Off
- [x] Automated gates green; spec ACs met. **Pending:** manual-QA matrix in the Extension Host + owner approval to open the PR / release (outward actions).
