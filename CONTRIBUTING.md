# Contributing to vscode-smalltalk

Thanks for your interest! This project follows a deliberate **Spec-Driven Development**
process. This document describes how we work so that contributors (and future us) can keep
working the same way.

## Philosophy

We don't start from code — we start from intent, written down and traceable. Every change
can be followed from a principle, to a product goal, to a spec, to a task, to a GitHub
issue, to a commit, to a verification. This keeps a small/solo project honest and makes the
roadmap legible to outsiders.

We use **[GitHub Spec Kit](https://github.com/github/spec-kit)** (the `.specify/` toolkit
and the `speckit.*` prompts/agents under `.github/`) on top of a lightweight
product-management layer.

## The artifact hierarchy (source of truth)

From most authoritative to most specific:

| Layer | Lives in | Answers |
|---|---|---|
| **Constitution** | `.specify/memory/constitution.md` | The non-negotiable principles. Supersedes everything else. |
| **Decision records (ADRs)** | `docs/decisions/NNNN-*.md` | *Why* a significant/irreversible choice was made. |
| **Product plan** | `docs/product/high-level-plan.md` | Vision, audience, phased rollout. |
| **Epics** | `docs/product/epics.md` | Themes of work (`EPIC-00X`). |
| **User stories** | `docs/product/user-stories.md` | The canonical `US-XXX` stories, with Acceptance Criteria + DoR/DoD. **This is the source of truth for a story's scope and ACs.** |
| **Feature specs** | `specs/US-XXX-*/` | The Spec Kit working set for a feature: `spec.md`, `plan.md`, `tasks.md`, `verification.md`, `requirements-validation.md`. |
| **GitHub issues** | the repo's Issues | One issue per story/task, for tracking and discussion. |
| **Roadmap snapshot** | `docs/ROADMAP.md` | The current milestone map (versions → themes). |

> When two layers disagree, the higher one wins — and that's a bug to fix. See
> *Keeping things consistent* below.

## The workflow (Spec Kit pipeline)

For a new feature, move through these stages (each has a `speckit.*` prompt under
`.github/prompts/`):

1. **`speckit.constitution`** — establish or amend the principles (rare).
2. **`speckit.specify`** — write `specs/US-XXX/spec.md` (overview, goals, user stories, ACs, technical design, risks).
3. **`speckit.clarify`** — resolve ambiguities/questions in the spec before planning.
4. **`speckit.plan`** — produce `specs/US-XXX/plan.md` (implementation approach).
5. **`speckit.tasks`** — break the plan into `specs/US-XXX/tasks.md`.
6. **`speckit.taskstoissues`** — open a GitHub issue per task/story (only against this repo's remote).
7. **`speckit.implement`** — build it, committing against the story (see commit convention).
8. **`speckit.analyze`** — cross-check artifacts for consistency (run this after any architecture change).
9. **`speckit.checklist`** — generate/verify quality checklists.

Small stories may legitimately use a reduced artifact set (e.g. `spec.md` + `tasks.md`
only). Research-heavy features do a **research-first** pass into `docs/research/` before
specifying (as US-200 did for the grammar before US-201).

## Conventions

### ID numbering
* **User stories** `US-XYZ` are grouped by band:
  * `1xx` — Documentation & onboarding
  * `2xx` — Declarative editing (grammar, snippets, language config)
  * `3xx` — Workflow integration (commands)
  * `4xx` — Language intelligence (LSP)
* **Epics** `EPIC-00X`. **ADRs** `NNNN` (zero-padded, incrementing).

### GitHub issue labels
* Kind: `user-story`, `bug`, `enhancement`, `type:research`, `type:prototype`
* Planning: `phase:1`/`phase:2`, `priority:high|medium|low`, `size:S|M|L|XL`, `status:ready|blocked`
* Area: `epic:documentation`, `epic:declarative-features`, `epic:workflow`, `epic:lsp`

### Branches & commits
* Work on a feature branch; open a PR against `master`.
* **Conventional Commits**, scoped to the story where applicable:
  * `feat(US-XXX): …`, `fix(US-XXX): …`, `test(US-XXX): …`
  * `docs: …`, `chore: …`, `ci: …`
  * Reference issues (`Fixes #N`) so they close on merge to the default branch.
  * **Do not** add `Co-Authored-By` trailers.

### Definition of Ready (DoR) / Definition of Done (DoD)
Every story carries DoR/DoD checklists (see existing stories for the template). **A story
is not Done until:** ACs demonstrably met, tests written and passing, **CI green on Linux,
macOS, and Windows**, docs updated, and the change verified in a clean VS Code instance.

## Local development

```bash
npm install
npm run build:grammar     # YAML grammar -> syntaxes/gnu-smalltalk.tmLanguage.json (gitignored, built artifact)
npm run test:grammar      # snapshot tests for the TextMate grammar
npm run test:grammar -- --update   # regenerate snapshots after an intentional grammar change
npx vsce package --no-dependencies # build a VSIX to inspect what ships
```

Press `F5` in VS Code to launch an **Extension Development Host** for manual QA.

The built grammar JSON is **gitignored**; it is produced by the `vscode:prepublish` hook at
package/publish time, so a clean checkout never ships a stale or missing grammar.

> The TypeScript client/server (`client/`, `server/`) arrive with the LSP milestones
> (US-410+). Until then the extension is purely declarative (grammar, snippets, language
> configuration).

## Evals & quality harness

AI agents are first-class contributors here, so quality depends on a harness that wraps
**structure + automated verification + human judgement** around every change and evaluates
it on two axes:

* **Output Eval** — *is the artifact right?* Golden datasets + metrics per capability, run
  deterministically in CI via `npm run eval`.
* **Trajectory Eval** — *was it produced the right way?* A rubric scoring the process
  (spec → issue → tests → CI → docs/ADR, no drift), applied at PR review.

The [PR template](.github/PULL_REQUEST_TEMPLATE.md) makes both gates explicit, and **a
feature is not Done until its eval dataset exists and passes**. Full design, the eval
matrix, and the rubrics live in [`evals/`](evals/README.md). Guiding principle:
*deterministic checks first, LLM-as-judge as an assist, human as the decision.*

## Decision records

When you make a choice that is hard to reverse or that contradicts an existing plan, write
an **ADR** in `docs/decisions/` (`NNNN-short-title.md`: Status, Context, Decision,
Consequences). Then propagate it: amend the Constitution if a principle changed, update the
affected product docs/specs, and link the ADR from them. See
[ADR-0001](docs/decisions/0001-typescript-bundled-lsp-server.md) as the worked example.

## Keeping things consistent

The biggest risk in a layered, spec-driven repo is **drift** — a decision recorded in one
place but not the others. To prevent it:

1. Record significant decisions as an ADR (single source of "why").
2. After any architecture change, run `speckit.analyze` and fix every doc the decision
   touches in the *same* change.
3. Treat a contradiction between layers as a defect, not a detail.

## Reporting issues

Use the GitHub issue tracker. For bugs, include your OS, VS Code version, GNU Smalltalk
version (if relevant), a minimal `.st` snippet, and what you expected vs. saw.
