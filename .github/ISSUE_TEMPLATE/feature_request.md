---
name: Feature request
about: Suggest a capability or improvement
title: ''
labels: enhancement
assignees: ''
---

<!--
This project is spec-driven and has a public roadmap. Before filing, a quick look at
docs/ROADMAP.md and docs/product/user-stories.md may show the idea is already planned —
if so, comment on the existing story instead.
-->

## The problem / motivation
What are you trying to do, and where does the extension fall short today?

## Proposed capability
What should it do? Be concrete (a command, a provider, a setting, a grammar rule…).

## Which area
- [ ] Grammar / syntax highlighting
- [ ] Language intelligence (LSP: completion, hover, references, rename, formatting, …)
- [ ] Parser / diagnostics
- [ ] Dialect support (beyond GNU Smalltalk)
- [ ] Runtime / `gst` integration (Live Bridge)
- [ ] Other

## Offline vs. runtime
The extension's north star is **offline parity** — most features work with no `gst`. Does this need a running image/`gst`, or can it be done statically? (Fine either way — it helps us route to the right epic.)

## Alternatives / prior art
Anything from other Smalltalk IDEs or editors worth referencing.
