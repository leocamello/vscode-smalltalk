# Change Log

All notable changes to the "vscode-smalltalk" extension will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.

## [Unreleased]

### Added

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