# US-901 — Manual QA workspace (Hardening & Perf)

A ready-to-open workspace for the `verification.md` §4 matrix. US-901 adds **no new feature** — it hardens
the existing offline engine (robustness + perf + no-telemetry). This workspace proves the **never-crash /
never-hang** behaviour on pathological input while the normal feature surface keeps working.

- `Normal.st` — a clean class (`Account`), 0 diagnostics. Use it to confirm completion / hover / rename /
  references / formatting / semantic tokens **still work** while the pathological file is open.
- `Pathological-DeepNest.st` — a method body with **~800 levels of nested blocks** (`[[[…]]]`). Before US-901
  this overflowed the recursive-descent parser's stack (server crash). Now the parser **caps nesting depth**
  and emits a single **"Expression nesting too deep"** diagnostic instead — no crash, no hang.

## QA matrix

1. **No crash on deep nesting.** Open `Pathological-DeepNest.st`. Expect: the editor stays responsive, a
   single diagnostic squiggle ("Expression nesting too deep"), and **no** "server crashed / will restart"
   toast. Check **Output → Smalltalk Language Server** — no stack-trace / exit.
2. **Features survive.** With the pathological file still open, switch to `Normal.st`: F2-rename `Account`,
   hover `deposit:`, trigger completion, Find All References on `balance` — all work normally.
3. **No telemetry / offline.** Disconnect the network (or watch the Dev Tools **Network** tab). Nothing the
   extension does should produce network traffic. (Guarded automatically by `server/test/noTelemetry.test.ts`.)
4. **Perf budgets** (dev-box, terminal, not the Extension Host): `npm run bench` → `index 1000 files` **PASS**
   (< 5 s) and `completion p95` **PASS** (< 100 ms). `npm run bench -- --files 2000 --keep` to inspect a corpus.

## Open it
1. In the Extension Development Host (`F5` from the repo): **File → Open Folder…** → this
   `manual-qa-workspace/` folder. The extension auto-activates (the folder has `.st` files).
2. Run the matrix above. Any server crash toast, editor hang, or network request is a bug.

---

> Note: VS Code auto-writes `.vscode/settings.json` here; it's **gitignored** (see the repo `.gitignore`).
