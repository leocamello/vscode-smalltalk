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
| **User stories** | `docs/product/user-stories.md` | The **backlog**: every `US-XXX` story with its initial ACs + DoR/DoD. The entry point for picking up work. |
| **Feature specs** | `specs/US-XXX-Title/` | The working set, created when a story is started (or *reverse-engineered* from finished work). **Once it exists, `spec.md` is the detailed source of truth for that feature's ACs.** Canonical package: `spec.md`, `plan.md`, `tasks.md`, `requirements-validation.md` (Spec-phase gate), `verification.md` (Verify-phase gate). |
| **GitHub issues** | the repo's Issues | One issue per story/task, for tracking and discussion. |
| **Roadmap snapshot** | `docs/ROADMAP.md` | The current milestone map (versions → themes). |

> When two layers disagree, the higher one wins — and that's a bug to fix. See
> *Keeping things consistent* below.

## The workflow (per user story)

We run a **bespoke, Spec-Kit-inspired cycle** for each story. The `speckit.*` prompts under
`.github/prompts/` *assist* each phase, but the artifacts (named to the `US-XXX` convention)
are the deliverables — the cycle is what matters, not any one tool.

1. **Clarify** — pull the story from `docs/product/user-stories.md`; resolve ambiguities and
   pin down testable ACs before writing code (`speckit.clarify` helps).
2. **Spec** — scaffold the package with `npm run new-story -- US-XXX "Short Title" [--branch]`
   (stamps the full 5-file `specs/US-XXX-Title/` set from `.specify/templates/` and optionally
   cuts the `feature/US-XXX-…` branch), then fill in `spec.md` (overview, goals, non-goals,
   user stories, ACs, technical design, risks). Gate it with `requirements-validation.md`.
3. **Plan** — break the approach into `plan.md`.
4. **Task** — decompose into granular, checkable steps in `tasks.md`; open a GitHub issue
   per story/task.
5. **Implement** — make precise, test-driven edits on a `feature/US-XXX-…` branch.
6. **Verify** — validate against the spec (e.g. `npm run eval` / grammar snapshots), fill
   `verification.md`, tick off `tasks.md`, merge to `master`, and close the issue.

**Reverse-engineering mode:** when code lands ahead of its spec (it happens), back-fill the
full `specs/US-XXX-Title/` package to document the finished work — the spec set must exist
either way. Omitting `plan.md`/`verification.md` is drift to avoid, not a shortcut.

**Research-first:** complex features do a research pass into `docs/research/` before
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
* **Branch per story**, named for it: `feature/US-XXX-short-name` for new work,
  `fix/US-XXX-short-name` for corrections. Open a PR against `master`; the PR closes the
  linked issue on merge.
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
