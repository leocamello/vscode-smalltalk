# Output Eval Rubric (human / LLM-as-judge)

Use this for quality dimensions that deterministic golden datasets cannot capture. Score
each **0–2** (0 = fails, 1 = partial, 2 = meets). A change should not merge with any `0`.

| # | Dimension | What "meets" looks like | Score |
|---|---|---|---|
| 1 | **Acceptance Criteria** | Every AC in the linked `US-XXX` is demonstrably satisfied. | ☐ |
| 2 | **Correctness** | Behaviour is right on the golden dataset *and* on at least one adversarial/edge input. | ☐ |
| 3 | **Robustness** (Constitution V) | Handles malformed/incomplete input without crashing the extension host. | ☐ |
| 4 | **Native feel** (Constitution I) | Uses standard VS Code/LSP APIs; respects themes, settings, keybindings. | ☐ |
| 5 | **Performance** (Constitution) | No UI-thread blocking; activation/latency within budget. | ☐ |
| 6 | **Code quality** | Strict types, no `any`, reuses existing utilities, reads like the surrounding code. | ☐ |
| 7 | **Test quality** | Tests assert behaviour (not implementation), include edge cases, and would fail if the feature regressed. | ☐ |
| 8 | **Docs & UX copy** | Settings, commands, messages, and README/CHANGELOG are clear and updated. | ☐ |

**Verdict:** ☐ Accept ☐ Accept with follow-ups (file issues) ☐ Reject

**Reviewer notes:**
