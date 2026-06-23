# Change Log

All notable changes to the "vscode-smalltalk" extension will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.

## [Unreleased]

### Added

-   **Diagnostics (error checking)** for GNU Smalltalk, powered by the bundled language engine:
    -   **Live parser diagnostics** — syntax errors are flagged with squiggles **as you type** (debounced, no `gst` required), badged `smalltalk(parse)`, with severity as the parser reports it. They clear as you fix the code.
    -   **Quick fixes** — insert a missing `]` or `)` directly from the reported diagnostic.
-   **Opt-in `gst` compile diagnostics** — when `smalltalk.diagnostics.useGst` is enabled, the real GNU Smalltalk compiler runs **on save** and surfaces its errors (badged `gst(compile)`) alongside the parser diagnostics. Runs are bounded by a timeout and killed on edit, so no stray `gst` processes accumulate. A **Smalltalk: Validate with gst** command runs the same check on demand.

### Settings

-   **`smalltalk.diagnostics.useGst`** (`boolean`, default `false`) — also run `gst` on save for authoritative compile errors. Resolved from `smalltalk.gnuSmalltalkPath` or `PATH`; silently inert when `gst` isn't found.

The parser tier works with no GNU Smalltalk installed; the `gst` tier is strictly optional.

## [0.5.0] - 2026-06-21

### Added

-   **Auto-completion** for GNU Smalltalk, powered by the bundled language engine — no `gst` required:
    -   **Selectors after a receiver** — unary and keyword selectors from your workspace and the standard kernel library, ranked workspace-first, with prefix and camel-hump matching. Multi-part keyword selectors insert as snippets (e.g. `at:put:` → `at:${1} put:${2}`).
    -   **Class names & in-scope variables** — class names in expression-head position, plus the temporaries, parameters, and instance/class variables visible at the cursor.
-   **GNU Smalltalk kernel index** — standard-library classes and selectors are available for completion out of the box. By default (`auto`) the extension indexes your **installed** GNU Smalltalk kernel when it can find one (version-correct), otherwise it falls back to a **bundled** reference (GNU Smalltalk 3.2.5). The active source is shown in a **status-bar item**, and completion items indicate their origin (workspace / installed / bundled) so a suggestion is never mistaken for one guaranteed to be installed.

### Settings

-   **`smalltalk.completion.kernelLibrary`** (`auto` | `bundled` | `off`, default `auto`) — how kernel-library completions are sourced.
-   **`smalltalk.completion.kernelPath`** — optional path to a GNU Smalltalk kernel source directory (the folder of `.st` files) used by `auto`.

This answers the long-standing autocompletion request (issue #1). Everything works without GNU Smalltalk installed; install `gst` (or set `kernelPath`) for version-correct kernel completions.

## [0.4.1] - 2026-06-21

### Added

-   **Code folding** — collapse class/namespace bodies, method bodies, blocks, and multi-line comments precisely (semantic `foldingRange`, on top of the default indentation folding).
-   **Highlight occurrences** — placing the cursor on a symbol highlights its other occurrences in the file: a message selector highlights every send of it; a variable highlights its same-name uses within its scope (a local in one method does not highlight a same-named local in another).

Both work without GNU Smalltalk (`gst`) and need no configuration.

## [0.4.0] - 2026-06-20

### Added

-   **Code navigation** for GNU Smalltalk, powered by a new bundled language engine — no external tools or `gst` required:
    -   **Outline & breadcrumbs** (`textDocument/documentSymbol`) — classes with their instance- and class-side methods (selector + arity) and instance/class variables, for both brace-style (`Object subclass: Foo [ … ]`) and chunk-style (`!Foo methodsFor: '…'! … ! !`) files.
    -   **Workspace symbol search** (`Ctrl/Cmd+T`) — find classes and method selectors across the whole workspace; respects `files.exclude` and updates as you edit.
    -   **Go to Definition** (`F12` / `Ctrl/Cmd+Click`) — jump from a class reference to its definition, or from a message send to every implementor of that selector.
-   Under the hood: an **error-tolerant lexer, recursive-descent parser, and per-document symbol table** (the engine behind navigation). It never throws on malformed/half-typed code and parses the entire GNU Smalltalk 3.2.5 kernel (122 files) cleanly. The bundled language server is no longer a no-op.

## [0.3.0] - 2026-06-19

### Added

-   **Run Current File** command (`Smalltalk: Run Current File`) — runs the active `.st`/`.gst` file with your GNU Smalltalk interpreter in the integrated terminal. Available from the Command Palette and the editor context menu. Resolves `gst` from the `smalltalk.gnuSmalltalkPath` setting, falling back to your `PATH`, and offers to open the setting if `gst` isn't found.
-   Groundwork for language intelligence: the extension now ships a bundled TypeScript language server (no external Smalltalk required). It is a no-op for now — completion, navigation, and diagnostics arrive in upcoming releases.
-   New setting `smalltalk.trace.server` to trace communication with the language server.

## [0.2.0] - 2026-06-18

### Added

-   Indentation rules and comment-marker code folding (`"--` … `--"`) in the language configuration.
-   Overhauled, expanded snippet set for common GNU Smalltalk patterns.
-   Highlighting for class definition keywords (`subclass:`, `instanceVariableNames:`, etc.) and names (`entity.name.type.class.smalltalk`).
-   Highlighting for method definition markers (`methodsFor:`) and category names (`entity.name.section.smalltalk`).
-   Highlighting for method termination markers (`! !`) (`keyword.control.smalltalk`).
-   Highlighting for the return operator (`^`) (`keyword.control.smalltalk`).
-   Highlighting for character literals (`$c`) (`constant.character.smalltalk`).
-   Highlighting for pseudo-variables `self`, `super`, `thisContext` (`variable.language.smalltalk`).
-   Highlighting for constants `nil`, `true`, `false` (`constant.language.smalltalk`).
-   Highlighting for common selectors `new`, `yourself` (`keyword.control.smalltalk`).
-   Highlighting for assignment operator `:=` (`keyword.operator.assignment.smalltalk`).
-   Highlighting for statement terminator `.` (`punctuation.terminator.statement.smalltalk`).
-   Highlighting for cascade separator `;` (`punctuation.separator.cascade.smalltalk`).
-   Highlighting for miscellaneous operators `->`, `,`, `@` (`keyword.operator.misc.smalltalk`).
-   Expanded highlighting for common control flow selectors (`to:do:`, `do:`, `timesRepeat:`, `collect:`, `select:`, `reject:`, `even`) (`keyword.control.loop.smalltalk`).
-   Expanded highlighting for common support selectors (`initialize`, `show:`, `cr`, `printString`, `space`, `new:`, `at:`, `at:put:`, `size`, `value`, `value:`, `nextPut:`) (`support.function.smalltalk`).

### Changed

-   **Major Refactoring:** Overhauled TextMate grammar rules for improved accuracy and scope naming consistency.
-   **Block Highlighting:** Replaced simple match with `begin`/`end` pattern for blocks (`[...]`), correctly scoping brackets (`punctuation.definition.block`), arguments (`variable.parameter.block.smalltalk`), separator (`|`), and enabling highlighting within the block body (`meta.block.smalltalk`).
-   **Variable Declaration:** Changed scope for temporary variables (`| var |`) from non-standard `support.type.variable` to `variable.other.local.smalltalk`, and improved matching pattern. Scoped delimiters (`punctuation.definition.variable`).
-   **Array Literals:** Improved highlighting for array literals (`#(...)`), correcting capture structure, adding inner patterns for content, and refining scopes (`constant.other.array.literal.smalltalk`, `punctuation.definition.constant.array`).
-   **Byte Arrays:** Improved highlighting for byte arrays (`#[]`), correcting capture structure and refining scopes (`meta.array.byte.smalltalk`, `punctuation.definition.constant`).
-   **Symbols:** Improved highlighting for symbols (`#symbol`, `#'symbol'`) (`constant.other.symbol.smalltalk`, `punctuation.definition.constant.symbol`).
-   **Operators:** Refined regex and scopes for arithmetic, comparison, and logical operators.
-   Removed ineffective/incorrect rule previously scoped as `constant.other.messages.smalltalk`.
-   Raised the minimum required VS Code version to 1.82.

### Fixed

-   Bracket characters inside character literals (`$[`, `$(`, `${`, …) no longer break bracket matching, guides, or jump-to-bracket ([#2]).
-   Removed the incorrect `lineComment` mapping; Smalltalk has no line comments, so comment toggling (`Ctrl+/`) now correctly uses block comments (thanks @madmini, [#21]).

[#2]: https://github.com/leocamello/vscode-smalltalk/issues/2
[#21]: https://github.com/leocamello/vscode-smalltalk/pull/21