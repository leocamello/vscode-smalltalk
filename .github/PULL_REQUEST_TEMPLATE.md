<!--
This template encodes our quality harness (see evals/README.md).
A PR is mergeable only when Output and Trajectory evals pass and a human accepts it.
-->

## Summary

<!-- What changed and why, in a sentence or two. -->

**Closes:** #<!-- issue --> &nbsp;|&nbsp; **Story:** US-<!-- xxx --> &nbsp;|&nbsp; **ADR (if any):** <!-- docs/decisions/NNNN-... -->

## Output Eval — *is the artifact right?*

- [ ] Acceptance Criteria for the linked story are met.
- [ ] Output eval dataset added/updated and **passing** (`npm run eval`).
- [ ] CI green on **Linux, macOS, and Windows**.
- [ ] Manual QA in an Extension Development Host (if user-visible).
- [ ] [Output rubric](../evals/rubrics/output-eval.md) completed — no `0` scores.

<!-- Paste eval results / note new fixtures here. -->

## Trajectory Eval — *was it produced the right way?*

- [ ] Traces to a story/issue; spec existed and was clarified before coding (non-trivial work).
- [ ] Tests written with the change; decisions recorded as ADRs; no drift left behind.
- [ ] Conventional scoped commits; outcomes reported faithfully.
- [ ] [Trajectory rubric](../evals/rubrics/trajectory-eval.md): ☐ Sound ☐ Sound w/ notes ☐ Unsound

## Human sign-off

- [ ] Reviewer approves output **and** trajectory.
- [ ] PO accepts the story (DoD met).
