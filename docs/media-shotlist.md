# Demo media shot-list (US-902, 1.0)

The README **Demo** section references five animated captures under `media/`. This is the deterministic
recipe to record them — same sample code, same gestures, same crop — so the results look consistent. The
`v1.0.0` tag should not be cut until these files exist (otherwise the README shows broken images).

## Recording setup (do once)
- **Theme:** Default Dark Modern. **Font:** default, zoom so text is legible in a ~900 px-wide GIF.
- **Window:** hide the Activity Bar clutter you don't need; keep the Explorer + editor visible.
- Enable semantic highlighting so the highlighting demo is accurate:
  `"editor.semanticHighlighting.enabled": true`.
- Use the **ultimate QA workspace** as the on-screen code:
  `specs/US-902-Product-Polish-Open-VSX/manual-qa-workspace/` (the Bank domain — `Account.st`, `Bank.st`,
  `SavingsAccount.st`, `Literals.st`, `Diagnostics-Playground.st`, `Demo.st`, …). Its `README.md` is the full
  feature matrix, so each capture below maps to a matrix row. Trust the workspace so Run works.
- **Format:** animated GIF (or MP4 → GIF), ≤ ~900 px wide, ≤ ~8 s, looped. Keep each under ~2–3 MB.
- **Tool:** any screen recorder (e.g. Peek on Linux, Kap on macOS). Crop to the editor region.

## Captures

### 1. `media/demo-highlighting.gif` — Syntax & semantic highlighting
- **Show:** `Account.st` open. Slowly select an instance variable
  (`balance`) so its semantic color and occurrences are visible; hover a class name (`OrderedCollection`).
- **Point:** variables, selectors, and real kernel classes each carry a distinct color.
- **Length:** ~5 s.

### 2. `media/demo-outline.gif` — Outline & breadcrumbs
- **Show:** open the **Outline** view (Explorer) with `Account.st` and `Bank.st`; click a method to jump; click the
  **breadcrumb** bar and navigate class → method.
- **Point:** structure (class → methods, ivars) without running anything.
- **Length:** ~5 s.

### 3. `media/demo-completion.gif` — Completion
- **Show:** in a fresh line type `Transcript show` and trigger completion; then type a receiver + space and
  pick a kernel selector; show a keyword selector (`at:put:`) inserting as a snippet with tab-stops.
- **Point:** kernel + workspace completion, keyword sends as snippets. Note the status-bar kernel source.
- **Length:** ~7 s.

### 4. `media/demo-diagnostics.gif` — Diagnostics + quick fix
- **Show:** delete a closing `]` so a `smalltalk(parse)` squiggle appears as you type; open the quick-fix
  lightbulb and apply **insert missing `]`**; the squiggle clears.
- **Point:** live syntax checking + quick fixes, no `gst`.
- **Length:** ~6 s.

### 5. `media/demo-run.gif` — Run Current File
- **Show:** with the workspace **trusted** and `gst` installed, run **Smalltalk: Run Current File** on
  `Demo.st`; the integrated terminal prints `Hello, Smalltalk!` and `Sum 1 to 100 = 5050`.
- **Point:** the one feature that uses `gst`. (If recording without `gst`, show the command in the palette
  instead and skip the terminal.)
- **Length:** ~6 s.

## After recording
1. Drop the five `.gif` files into `media/`.
2. Preview the README (VS Code Markdown preview) — confirm all five render and are legible.
3. Commit the assets (`media/*.gif`) before creating the `v1.0.0` release.
