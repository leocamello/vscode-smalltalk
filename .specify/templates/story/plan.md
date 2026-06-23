# Implementation Plan: {{TITLE}}

**ID**: {{US_ID}} | **Date**: {{DATE}} | **Spec**: ./spec.md | **Branch**: `{{BRANCH}}`

## Summary
<!-- Primary requirement + chosen approach, in 2-3 sentences. -->

## Approach
<!-- How we'll build it; name the files/components and the existing utilities/patterns to reuse. -->

## Steps
1.
2.

## Dependencies & Risks
-

## Verification
<!-- How we'll prove it works: tests, eval dataset (evals/), and manual QA. -->
- **Acceptance harness (TDD e2e):** user-observable ACs are pinned by failing tests in
  `client/test-e2e/{{US_ID}}.acceptance.test.js` *before* implementation (red → green); ACs with no
  user-observable surface are routed to unit/eval. Routing is recorded in `requirements-validation.md` §3.5.
