---
name: Bug report
about: Something isn't working — a wrong result, a crash, or a squiggle where there shouldn't be one
title: ''
labels: bug
assignees: ''
---

<!--
Thanks for helping improve vscode-smalltalk! The extension is fully offline and needs no `gst`
for language intelligence, so most bugs are reproducible from just a small `.st` snippet.
Please keep the repro as small as possible.
-->

## What happened
A clear description of the bug.

## Minimal reproduction
The smallest `.st`/`.gst` snippet that shows it (paste it here — do not attach a whole project if you can avoid it):

```smalltalk
"your snippet"
```

Steps:
1. Open the snippet above.
2. …
3. …

## Expected vs. actual
- **Expected:**
- **Actual:**

## Which surface
<!-- Helps triage/route (e.g. area:parser, area:lsp). Check any that apply. -->
- [ ] Syntax highlighting / grammar
- [ ] Parser / diagnostics (squiggles, quick fixes)
- [ ] Language intelligence (completion, hover, outline, references, rename, formatting, semantic tokens)
- [ ] Run Current File / `gst`
- [ ] Other / not sure

## Environment
- Extension version:
- VS Code version:
- OS:
- GNU Smalltalk (`gst --version`), if relevant:

## Logs / screenshots
Any error notifications, `smalltalk(parse)` messages, or a short GIF. (The extension sends **no telemetry**, so nothing is collected automatically — please paste what you see.)
