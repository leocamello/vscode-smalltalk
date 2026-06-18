# Trajectory Eval Rubric (human / LLM-as-judge)

Evaluates *how* a change was produced, from its PR, commits, and linked artifacts — not just
the endpoint. A clean diff produced by an unsound process is still a defect. Score each
item ✅ / ⚠️ / ❌.

## 1. Intent was established before code
- [ ] The change traces to a `US-XXX` story / GitHub issue (not invented ad hoc).
- [ ] For non-trivial work, a spec (`specs/US-XXX/`) exists and ambiguities were clarified first.
- [ ] Scope matches the story; unrelated changes were split out.

## 2. Verification accompanied the work
- [ ] Tests were added/updated **with** the change (not bolted on after).
- [ ] The relevant **output eval** (dataset + metric) exists and passes.
- [ ] **CI is green on Linux, macOS, and Windows.** ("Done" was not claimed before CI ran.)
- [ ] Manual QA in an Extension Development Host where behaviour is user-visible.

## 3. Decisions and consistency
- [ ] Significant/irreversible choices are recorded as an **ADR** (`docs/decisions/`).
- [ ] No drift left behind: constitution, product docs, specs, and roadmap agree with the change.
- [ ] Hard-to-reverse or outward-facing actions (publishes, releases, issue/PR mutations) were confirmed, not assumed.

## 4. Craft & honesty
- [ ] Conventional, scoped commits (`feat(US-XXX):` …); no `Co-Authored-By` trailer.
- [ ] Outcomes reported faithfully — failures/skips surfaced, not hidden.
- [ ] Reused existing patterns/utilities instead of re-deriving them.

**Trajectory verdict:** ☐ Sound ☐ Sound with notes ☐ Unsound (do not merge)

**Reviewer notes:**
