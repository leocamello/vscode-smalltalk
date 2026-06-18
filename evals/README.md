# Quality & Evaluation Harness

This project is built with AI agents as first-class contributors (**agentic engineering**).
That only produces a trustworthy product if every change passes through a harness that
combines **structure**, **automated verification**, and **human judgement** — and if we
evaluate not just *what* was produced but *how*.

## Three pillars

1. **Structure** — the spec-driven scaffolding (constitution → ADRs → product docs → specs
   → issues → commits) documented in [`../CONTRIBUTING.md`](../CONTRIBUTING.md). It makes
   work legible and reviewable before a line of code exists.
2. **Automated verification** — deterministic checks that run in CI: type-checking, lint,
   the test suites, and the **output evals** below. A red check blocks merge.
3. **Human judgement** — what machines can't score: design quality, UX, spec clarity, and
   product acceptance. Applied at PR review via rubrics and PO sign-off.

## Two evaluation lenses

### Output Eval — *was the artifact correct and high-quality?*

Each product capability has a **golden dataset** (inputs + expected outputs) and a
**metric**. Output evals are deterministic and run in CI (`npm run eval`). Where exact
match is too blunt (e.g. "is this hover text *useful*?"), a human/LLM rubric supplements it
([`rubrics/output-eval.md`](rubrics/output-eval.md)).

| Capability | Story | Dataset | Metric | Gate |
|---|---|---|---|---|
| Grammar highlighting | US-201 | `docs/research/gst-syntax/test-cases/*.st` + `src/test/snapshots/` | exact scope-snapshot match | **CI (live)** |
| Parser / AST | US-411 | `evals/datasets/parser/` | snapshot AST match + **zero-crash kernel smoke** (`../smalltalk-3.2.5/kernel/`) + mutation no-throw | CI (when built) |
| Symbols / definition | US-412 | `evals/datasets/navigation/` | precision/recall vs. expected symbols & targets | CI |
| Completion | US-413 | `evals/datasets/completion/` | expected item in top-k; ranking sanity | CI |
| Diagnostics | US-414 | `evals/datasets/diagnostics/` | precision/recall on expected ranges + codes | CI |
| Formatting | US-416 | `evals/datasets/formatting/` | **idempotence** `f(f(x))==f(x)` + token round-trip + golden | CI |

> Dataset folders are created by the milestone that needs them. The grammar dataset already
> exists in its research location and is wired through `npm run test:grammar`. **A feature is
> not Done until its eval dataset and metric exist and pass in CI** (this is part of the DoD).

### Trajectory Eval — *was the change produced the right way?*

The endpoint can look fine while the path was unsound (no spec, tests written after the
fact, drift left behind, "done" claimed without running CI). The trajectory rubric
([`rubrics/trajectory-eval.md`](rubrics/trajectory-eval.md)) scores the *process* of a
change from its PR, commits, and linked artifacts. It is applied by the human reviewer at PR
time; an **LLM-as-judge** may pre-fill a draft score from the same evidence (planned —
deterministic checks first, judge as an assist, human as the decision).

## Running evals

```bash
npm run eval          # deterministic output evals (grows per milestone; grammar today)
npm run test:grammar  # grammar snapshots only
```

Human rubrics are completed in the **PR template** ([`../.github/PULL_REQUEST_TEMPLATE.md`](../.github/PULL_REQUEST_TEMPLATE.md)),
which carries the trajectory checklist, output-eval results, and sign-off. CI runs the
output evals on Linux/macOS/Windows and blocks merge on failure.

## Principle

> Deterministic checks first, LLM-as-judge as an assist, **human as the decision**.
> No change is "Done" until Output **and** Trajectory evals pass and a human accepts it.
