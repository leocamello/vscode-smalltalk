# TextMate Scoping Strategy for GNU Smalltalk

This section outlines the proposed TextMate scoping strategy for GNU Smalltalk syntactic elements. The goal is to map language constructs to standard or conventional TextMate scopes to ensure semantic accuracy, good theme compatibility, and a useful level of detail for developers. This strategy will guide the implementation or refinement of the TextMate grammar (US-201).

The proposed scopes adhere to the [TextMate Scope Naming Conventions](https://macromates.com/manual/en/language_grammars#naming_conventions) where possible, using standard root names and a dot-separated hierarchy. The suffix `.smalltalk` is used for general Smalltalk constructs, while `.gst.smalltalk` indicates constructs specific to GNU Smalltalk or those with a particular GST interpretation.

## 5.1 Comments

*   **GST Syntactic Element:** Double-quoted comments
*   **Brief Description/Example:** `"This is a comment."`, `"A comment spanning
    multiple lines."`
*   **Proposed TextMate Scopes:**
    *   Overall: `comment.block.double-quoted.smalltalk`
    *   Delimiters: `punctuation.definition.comment.begin.smalltalk` (for `"`), `punctuation.definition.comment.end.smalltalk` (for `"`)
*   **Rationale/Convention Source:** Standard `comment.block` for multi-line capable comments. `punctuation.definition.comment` for delimiters.

*   **GST Syntactic Element:** Shebang / Script Header
*   **Brief Description/Example:** `#!/usr/bin/gst -f`
*   **Proposed TextMate Scopes:**
    *   Overall: `comment.line.shebang.gst.smalltalk`
    *   Punctuation: `punctuation.definition.comment.shebang.gst.smalltalk` (for `#!`)
*   **Rationale/Convention Source:** `comment.line.shebang` is a common convention for script interpreters.

## 5.2 Literals

#### 5.2.1 Numeric Literals

*   **GST Syntactic Element:** Simple Integers
    *   **Brief Description/Example:** `123`, `-45`, `0`
    *   **Proposed TextMate Scopes:** `constant.numeric.integer.smalltalk`
    *   **Rationale/Convention Source:** Standard `constant.numeric.integer`.

*   **GST Syntactic Element:** Radix Integers
    *   **Brief Description/Example:** `2r1011`, `16rFF`, `8r070`
    *   **Proposed TextMate Scopes:**
        *   Overall: `constant.numeric.integer.radix.smalltalk`
        *   Radix prefix (e.g., `2r`): `constant.numeric.radix.base.smalltalk`
        *   Number part (e.g., `1011`): `constant.numeric.radix.number.smalltalk` (or inherit overall scope)
    *   **Rationale/Convention Source:** `constant.numeric` with specificity for radix.

*   **GST Syntactic Element:** Float Literals (Double-precision)
    *   **Brief Description/Example:** `1.0`, `1.0e2`, `1.2d`, `-3.14e-5`, `123e2`
    *   **Proposed TextMate Scopes:** `constant.numeric.float.smalltalk`
        *   Exponent indicator (`e`, `d`): `constant.numeric.float.exponent-indicator.smalltalk` (within the float constant)
    *   **Rationale/Convention Source:** Standard `constant.numeric.float`. Sub-scope for exponent indicator for semantic precision.

*   **GST Syntactic Element:** Scaled Decimal Literals
    *   **Brief Description/Example:** `1.2s2`, `1s`, `123s0`, `2r101s1`
    *   **Proposed TextMate Scopes:** `constant.numeric.scaled-decimal.gst.smalltalk`
        *   Scale indicator (`s`): `constant.numeric.scaled-decimal.scale-indicator.gst.smalltalk` (within the constant)
        *   Scale value (e.g., `2` in `1.2s2`): `constant.numeric.integer.scale-value.gst.smalltalk` (within the constant)
    *   **Rationale/Convention Source:** `constant.numeric` with GST-specific qualifier. Sub-scopes for scale indicator and value for semantic precision.

#### 5.2.2 String Literals

*   **GST Syntactic Element:** String Literals
*   **Brief Description/Example:** `'Hello'`, `''`, `'a ''quote'''`
*   **Proposed TextMate Scopes:**
    *   Overall: `string.quoted.single.smalltalk`
    *   Delimiters: `punctuation.definition.string.begin.smalltalk` (for `'`), `punctuation.definition.string.end.smalltalk` (for `'`)
    *   Escaped quote (`''`): `constant.character.escape.apostrophe.smalltalk`
*   **Rationale/Convention Source:** Standard `string.quoted.single` and `punctuation.definition.string`. `constant.character.escape` for escaped elements.

#### 5.2.3 Character Literals

*   **GST Syntactic Element:** Simple Character Literals
*   **Brief Description/Example:** `$a`, `$$`
*   **Proposed TextMate Scopes:** `constant.character.simple.smalltalk`
    *   Prefix (`$`): `punctuation.definition.character.smalltalk`
*   **Rationale/Convention Source:** `constant.character` for character literals.

*   **GST Syntactic Element:** Special Character Literals (GST specific)
*   **Brief Description/Example:** `$<65>`, `$<16r42>`, `$<0>`
*   **Proposed TextMate Scopes:**
    *   Overall: `constant.character.numeric.gst.smalltalk`
    *   Delimiters: `punctuation.definition.character.numeric.begin.gst.smalltalk` (for `$<`), `punctuation.definition.character.numeric.end.gst.smalltalk` (for `>`)
    *   Radix (if present, e.g., `16r`): `constant.numeric.radix.base.smalltalk` (within the character constant)
    *   Value (e.g., `65`, `42`): `constant.numeric.integer.smalltalk` (within the character constant)
*   **Rationale/Convention Source:** `constant.character` with GST-specific qualifier for numeric forms.

#### 5.2.4 Symbol Literals

*   **GST Syntactic Element:** Identifier-like & Keyword-like Symbols
    *   **Brief Description/Example:** `#aSymbol`, `#ifTrue:ifFalse:`, `#anotherKeyword:`, `#:` (unary colon symbol)
    *   **Proposed TextMate Scopes:**
        *   Prefix (`#`): `punctuation.definition.symbol.smalltalk`
        *   Identifier-like (e.g., `aSymbol` in `#aSymbol`): `constant.other.symbol.identifier.smalltalk`
        *   Keyword-like (e.g., `ifTrue:ifFalse:` in `#ifTrue:ifFalse:`, `anotherKeyword:` in `#anotherKeyword:`):
            *   Overall keyword symbol: `constant.other.symbol.keyword.smalltalk`
            *   Keyword parts (e.g., `ifTrue:`, `anotherKeyword:`): `entity.name.function.keyword.symbol.smalltalk` (within the symbol scope)
            *   Colon within keyword symbol part: `punctuation.definition.keyword.symbol.smalltalk`
        *   Unary colon symbol (`:` in `#:`): `constant.other.symbol.operator.smalltalk` (or a more specific `constant.other.symbol.colon.smalltalk`)
    *   **Rationale/Convention Source:** `constant.other.symbol` is common for symbols. Subtypes for clarity. Finer granularity for keyword colons for styling consistency.

*   **GST Syntactic Element:** Quoted String Symbols
    *   **Brief Description/Example:** `#'a string symbol'`, `#'another-one!@#$'`
    *   **Proposed TextMate Scopes:**
        *   Overall: `constant.other.symbol.quoted.smalltalk`
        *   Prefix (`#`): `punctuation.definition.symbol.smalltalk`
        *   String part: `string.quoted.single.symbol.smalltalk` (or reuse `string.quoted.single.smalltalk`)
            *   Delimiters: `punctuation.definition.string.begin.symbol.smalltalk`, `punctuation.definition.string.end.symbol.smalltalk`
    *   **Rationale/Convention Source:** `constant.other.symbol` with subtype for quoted form.

*   **GST Syntactic Element:** Binary Operator & Numeric-like Symbols
    *   **Brief Description/Example:** `#+`, `#~=`, `#*`, `#123`, `#1.0`, `#1s2`
    *   **Proposed TextMate Scopes:**
        *   Operator-like: `constant.other.symbol.operator.smalltalk`
        *   Numeric-like: `constant.other.symbol.numeric.smalltalk` (containing `constant.numeric.*` for the number part)
        *   Prefix (`#`): `punctuation.definition.symbol.smalltalk`
    *   **Rationale/Convention Source:** `constant.other.symbol` with subtypes.

#### 5.2.5 Array Literals

*   **GST Syntactic Element:** Static Array Literals
*   **Brief Description/Example:** `#()`, `#(1 'two' #s)`
*   **Proposed TextMate Scopes:**
    *   Overall: `meta.array.literal.smalltalk`
    *   Delimiters: `punctuation.definition.array.begin.smalltalk` (for `#(`), `punctuation.definition.array.end.smalltalk` (for `)`)
    *   Elements: Scoped according to their own type (numbers, strings, symbols, etc.).
*   **Rationale/Convention Source:** `meta.array` for the construct. `punctuation.definition.array` for delimiters.

#### 5.2.6 Byte Array Literals

*   **GST Syntactic Element:** Byte Array Literals
*   **Brief Description/Example:** `#[0 10 255]`, `#[]`
*   **Proposed TextMate Scopes:**
    *   Overall: `meta.bytearray.literal.smalltalk`
    *   Delimiters: `punctuation.definition.bytearray.begin.smalltalk` (for `#[`), `punctuation.definition.bytearray.end.smalltalk` (for `]`)
    *   Elements (integers 0-255): `constant.numeric.integer.byte.smalltalk`
*   **Rationale/Convention Source:** `meta.bytearray` for the construct. Specific `constant.numeric` for byte values.

#### 5.2.7 GST-Specific Literals / Constants

*   **GST Syntactic Element:** Dynamic Array Constructor (GST specific)
*   **Brief Description/Example:** `{ 1. 2. 'foo' }`, `{}`
*   **Proposed TextMate Scopes:**
    *   Overall: `meta.array.constructor.dynamic.gst.smalltalk`
    *   Delimiters: `punctuation.definition.array.constructor.begin.gst.smalltalk` (for `{`), `punctuation.definition.array.constructor.end.gst.smalltalk` (for `}`),
    *   Separator: `punctuation.separator.array.constructor.gst.smalltalk` (for `.`)
    *   Elements: Scoped according to their expressions.
*   **Rationale/Convention Source:** `meta.array` with qualifiers for GST dynamic constructor.

*   **GST Syntactic Element:** Binding Constants (GST specific)
*   **Brief Description/Example:** `#{Object}`, `#{MyGlobal}`
*   **Proposed TextMate Scopes:**
    *   Overall: `constant.other.binding.gst.smalltalk`
    *   Delimiters: `punctuation.definition.binding.begin.gst.smalltalk` (for `#{`), `punctuation.definition.binding.end.gst.smalltalk` (for `}`)
    *   Content (e.g., `Object`): `support.class.smalltalk` or `variable.other.global.smalltalk`
*   **Rationale/Convention Source:** `constant.other` for a special constant type.

*   **GST Syntactic Element:** Compile-Time Constants (GST specific)
*   **Brief Description/Example:** `##(1+2)`, `##(Time now)`
*   **Proposed TextMate Scopes:**
    *   Overall: `meta.compile-time-constant.gst.smalltalk` (as it contains an expression)
    *   Constant marker: `constant.other.compile-time.gst.smalltalk` (for `##`)
    *   Delimiters: `punctuation.definition.compile-time.begin.gst.smalltalk` (for `(`), `punctuation.definition.compile-time.end.gst.smalltalk` (for `)`)
    *   Expression: Scoped according to its content.
*   **Rationale/Convention Source:** `constant.other` for the marker, `meta` for the overall construct.

## 5.3 Identifiers and Variables

*   **GST Syntactic Element:** Pseudo-variables
    *   **Brief Description/Example:** `self`, `super`, `true`, `false`, `nil`, `thisContext`
    *   **Proposed TextMate Scopes:**
        *   `self`, `super`: `variable.language.self.smalltalk`
        *   `thisContext`: `variable.language.this-context.gst.smalltalk`
        *   `true`, `false`: `constant.language.boolean.smalltalk`
        *   `nil`: `constant.language.nil.smalltalk`
    *   **Rationale/Convention Source:** Standard `variable.language` for context-dependent variables, `constant.language` for fixed value pseudo-variables.

*   **GST Syntactic Element:** Temporary Variables (in methods or blocks)
    *   **Brief Description/Example:** `| temp1 temp2 |`
    *   **Proposed TextMate Scopes:**
        *   Variables: `variable.other.local.smalltalk`
        *   Delimiters: `punctuation.definition.variable.local.begin.smalltalk` (for `|`), `punctuation.definition.variable.local.end.smalltalk` (for `|`)
    *   **Rationale/Convention Source:** `variable.other.local` for local scope variables.

*   **GST Syntactic Element:** Method/Block Parameters (Arguments)
    *   **Brief Description/Example:** `:arg1`, `:blockArg` in `[:blockArg| ...]` or `method: arg1 with: arg2 [...]`
    *   **Proposed TextMate Scopes:**
        *   Method parameters: `variable.parameter.method.smalltalk`
        *   Block parameters: `variable.parameter.block.smalltalk`
        *   Prefix (`:`): `punctuation.definition.variable.parameter.smalltalk`
    *   **Rationale/Convention Source:** Standard `variable.parameter`.

*   **GST Syntactic Element:** Instance Variables (declared in GST class definitions)
    *   **Brief Description/Example:** `| ivar1 ivar2 |` within `SomeClass subclass: ... [...]`
    *   **Proposed TextMate Scopes:**
        *   Variables: `variable.other.instance.smalltalk`
        *   Delimiters (if distinct syntax): `punctuation.definition.variable.instance.begin.smalltalk`, `punctuation.definition.variable.instance.end.smalltalk` (Often same as locals `|...|`)
    *   **Rationale/Convention Source:** `variable.other.instance` for object instance fields.

*   **GST Syntactic Element:** Class Variables (declared/assigned in GST class definitions)
    *   **Brief Description/Example:** `ClassVarName := initialValue.` within `SomeClass subclass: ... [...]`
    *   **Proposed TextMate Scopes:**
        *   Declaration/Assignment: `variable.other.class.smalltalk`
        *   Usage: `support.variable.class.smalltalk` or `variable.other.class.smalltalk`
    *   **Rationale/Convention Source:** `variable.other.class` for class-level static variables.

*   **GST Syntactic Element:** Global Variables / Class Names (used as variables/receivers)
    *   **Brief Description/Example:** `Transcript`, `Object`, `MyGlobal`, `AnotherClass`
    *   **Proposed TextMate Scopes:**
        *   Identifiers following `CapitalizedIdentifier` convention (potential class names): `entity.name.type.class.smalltalk`
        *   Predefined/core language classes (if a list is maintained): `support.class.smalltalk` (This is a subset of the above, for specific highlighting)
        *   Other global variables (e.g., `MyGlobal` if not matching class convention or known otherwise): `variable.other.global.smalltalk`
    *   **Rationale/Convention Source:** `entity.name.type.class.smalltalk` for conventionally capitalized class names. `support.class.smalltalk` for known built-ins. `variable.other.global.smalltalk` for other globals. This aligns with typical TextMate grammar behavior.

## 5.4 Operators and Punctuation

*   **GST Syntactic Element:** Assignment Operators
    *   **Brief Description/Example:** `:=`, `_`
    *   **Proposed TextMate Scopes:**
        *   `:=`: `keyword.operator.assignment.smalltalk`
        *   `_`: `keyword.operator.assignment.underscore.gst.smalltalk`
    *   **Rationale/Convention Source:** Standard `keyword.operator.assignment`.

*   **GST Syntactic Element:** Statement Terminator
    *   **Brief Description/Example:** `.`
    *   **Proposed TextMate Scopes:** `punctuation.terminator.statement.smalltalk`
    *   **Rationale/Convention Source:** Standard `punctuation.terminator.statement`.

*   **GST Syntactic Element:** Cascade Separator
    *   **Brief Description/Example:** `;`
    *   **Proposed TextMate Scopes:** `punctuation.separator.cascade.smalltalk`
    *   **Rationale/Convention Source:** Specific punctuation for cascades.

*   **GST Syntactic Element:** Return Operator
    *   **Brief Description/Example:** `^`
    *   **Proposed TextMate Scopes:** `keyword.control.return.smalltalk`
    *   **Rationale/Convention Source:** `keyword.control` for flow control statements; `^` is a key control flow element in Smalltalk.

*   **GST Syntactic Element:** Parentheses for Grouping
    *   **Brief Description/Example:** `(1 + 2)`
    *   **Proposed TextMate Scopes:**
        *   Overall: `meta.expression.parenthesized.smalltalk`
        *   Delimiters: `punctuation.section.group.begin.smalltalk` (for `(`), `punctuation.section.group.end.smalltalk` (for `)`)
    *   **Rationale/Convention Source:** Standard punctuation for grouping.

## 5.5 Message Sends

*   **GST Syntactic Element:** General Message Send Structure
    *   **Proposed TextMate Scopes:**
        *   Overall (optional, can wrap receiver + selector + args): `meta.message-send.smalltalk`
        *   Receiver: Scoped according to its type (variable, literal, result of another message).
    *   **Rationale/Convention Source:** `meta.message-send` for the broader construct.

*   **GST Syntactic Element:** Unary Message Selector
    *   **Brief Description/Example:** `anObject size`, `3 factorial`
    *   **Proposed TextMate Scopes:** `entity.name.function.unary.smalltalk`
    *   **Rationale/Convention Source:** `entity.name.function` for callable units, with `unary` subtype.

*   **GST Syntactic Element:** Binary Message Selector
    *   **Brief Description/Example:** `1 + 2`, `anObject // 2`
    *   **Proposed TextMate Scopes:**
        *   The message send construct (e.g., `1 + 2`): `meta.expression.binary.smalltalk` or `meta.message-send.binary.smalltalk`
        *   The selector token itself (e.g., `+`, `//`):
            *   Primary: `keyword.operator.binary.smalltalk` (or more specific like `keyword.operator.arithmetic.smalltalk`, `keyword.operator.comparison.smalltalk` if distinguishable by syntax alone).
            *   Secondary/Role: `entity.name.function.binary.smalltalk` (This describes its role as a selector in the message send).
    *   **Rationale/Convention Source:** `keyword.operator.*` for the visual token, aligning with common theme styling. `entity.name.function.binary.smalltalk` for its semantic role as a selector. `meta.*` for the overall expression. Grammars may apply both to the token or prioritize `keyword.operator`.

*   **GST Syntactic Element:** Keyword Message Selector Parts
    *   **Brief Description/Example:** `anObject at: 1 put: 'x'`, `ifTrue: []`
    *   **Proposed TextMate Scopes:** `entity.name.function.keyword.smalltalk` (for each part like `at:`, `put:`, `ifTrue:`)
    *   **Rationale/Convention Source:** `entity.name.function` for callable units, with `keyword` subtype.

## 5.6 Blocks

*   **GST Syntactic Element:** Block Literals
    *   **Brief Description/Example:** `[ 1 + 1 ]`, `[ :val | val * val ]`
    *   **Proposed TextMate Scopes:**
        *   Overall: `meta.block.literal.smalltalk`
        *   Delimiters: `punctuation.definition.block.begin.smalltalk` (for `[`), `punctuation.definition.block.end.smalltalk` (for `]`)
        *   Parameter prefix (`:`): `punctuation.definition.variable.parameter.block.smalltalk`
        *   Parameter/temporary separator (`|`): `punctuation.separator.variable.block.smalltalk`
        *   Parameters: `variable.parameter.block.smalltalk`
        *   Temporaries: `variable.other.local.smalltalk`
    *   **Rationale/Convention Source:** `meta.block` for the construct. Specific punctuation and variable scopes.

## 5.7 Definitions (GST-specific `[...]` syntax and `>>`)

#### 5.7.1 Namespace Definitions

*   **GST Syntactic Element:** Namespace Definition (GST specific)
    *   **Brief Description/Example:** `Namespace current: 'MyNamespace' [ ... ]`
    *   **Proposed TextMate Scopes:**
        *   Overall: `meta.namespace.definition.gst.smalltalk`
        *   `Namespace`: `support.class.gst.smalltalk` (assuming `Namespace` is a known class for this construct)
        *   `current:`: `keyword.declaration.namespace.gst.smalltalk`
        *   Namespace Name (e.g., `'MyNamespace'` string content): `entity.name.namespace.gst.smalltalk`
        *   Body Delimiters (`[` `]`): `punctuation.definition.namespace.body.begin.gst.smalltalk`, `punctuation.definition.namespace.body.end.gst.smalltalk`
        *   Keywords like `imports:`: `keyword.declaration.namespace.imports.gst.smalltalk`
    *   **Rationale/Convention Source:** `meta.namespace` and `entity.name.namespace` for namespaces. `keyword.declaration.*` for declarative parts. GST qualifiers.

#### 5.7.2 Class Definitions and Extensions

*   **GST Syntactic Element:** Class Definition (GST specific `subclass:`)
    *   **Brief Description/Example:** `ObjectClass subclass: #MyClass instanceVariableNames: 'ivar' ... [...]`
    *   **Proposed TextMate Scopes:**
        *   Overall: `meta.class.definition.gst.smalltalk`
        *   Superclass Name (`ObjectClass`): `entity.other.inherited-class.gst.smalltalk` (or `support.class.smalltalk`)
        *   `subclass:` keyword: `keyword.declaration.class.subclass.gst.smalltalk`
        *   Class Name (`#MyClass` symbol content): `entity.name.type.class.gst.smalltalk`
        *   Other declaration keywords (`instanceVariableNames:`, `classVariableNames:`, etc.): `keyword.declaration.class.gst.smalltalk`
        *   String arguments for names: `string.quoted.single.smalltalk`
        *   Body Delimiters (`[` `]`): `punctuation.definition.class.body.begin.gst.smalltalk`, `punctuation.definition.class.body.end.gst.smalltalk`
    *   **Rationale/Convention Source:** Standard `meta.class`, `entity.name.type.class`, `entity.other.inherited-class`, `keyword.declaration.class`.

*   **GST Syntactic Element:** Class Extension (GST specific `extend`)
    *   **Brief Description/Example:** `SomeClass extend [ ... ]`
    *   **Proposed TextMate Scopes:**
        *   Overall: `meta.class.extension.gst.smalltalk`
        *   Class Name (`SomeClass`): `entity.name.type.class.gst.smalltalk` (or `support.class.smalltalk`)
        *   `extend` keyword: `keyword.declaration.class.extend.gst.smalltalk`
        *   Body Delimiters (`[` `]`): `punctuation.definition.class.body.begin.gst.smalltalk`, `punctuation.definition.class.body.end.gst.smalltalk`
    *   **Rationale/Convention Source:** Similar to class definition, specific keyword for `extend`.

#### 5.7.3 Method Definitions

*   **GST Syntactic Element:** Method Definition (GST specific, in class `[...]` or `Class >> selector [...]`)
    *   **Brief Description/Example:** `myMethod [ ^self ]`, `Point >> x [ ^x ]`, `Point >> x: newX [ x := newX ]`
    *   **Proposed TextMate Scopes:**
        *   Overall: `meta.method.definition.gst.smalltalk`
        *   **Scoped Definition (`Class >> selector [...]`):**
            *   Class Name (`Point`): `entity.name.type.class.gst.smalltalk` (or `support.class.smalltalk`)
            *   Scope Operator (`>>`): `punctuation.definition.method.scoped.gst.smalltalk`
        *   **Selector Being Defined:**
            *   Unary (e.g., `myMethod`, `x`): `entity.name.function.definition.unary.gst.smalltalk`
            *   Binary (e.g., `+`): `entity.name.function.definition.binary.gst.smalltalk`
            *   Keyword parts (e.g., `x:`, `y:`): `entity.name.function.definition.keyword.gst.smalltalk`
        *   Arguments: `variable.parameter.method.smalltalk`
        *   Body Delimiters (`[` `]`): `punctuation.definition.method.body.begin.gst.smalltalk`, `punctuation.definition.method.body.end.gst.smalltalk`
    *   **Rationale/Convention Source:** `meta.method`, `entity.name.function.definition` for method definitions.

## 5.8 Other GST-Specific Constructs

*   **GST Syntactic Element:** `Eval [...]` (GST specific)
    *   **Brief Description/Example:** `Eval [ Transcript show: 'Hello'. ]`
    *   **Proposed TextMate Scopes:**
        *   Overall: `meta.eval.gst.smalltalk`
        *   `Eval` keyword: `keyword.control.eval.gst.smalltalk`
        *   Body Delimiters (`[` `]`): `punctuation.definition.eval.body.begin.gst.smalltalk`, `punctuation.definition.eval.body.end.gst.smalltalk`
    *   **Rationale/Convention Source:** `meta` for the construct, `keyword.control.eval.gst.smalltalk` best reflects its special parsing context and control-like nature.

*   **GST Syntactic Element:** Attributes (GST specific)
    *   **Brief Description/Example:** `<primitive: 1>`, `<category: 'accessing'>`
    *   **Proposed TextMate Scopes:**
        *   Overall: `meta.attribute.gst.smalltalk`
        *   Delimiters: `punctuation.definition.attribute.begin.gst.smalltalk` (for `<`), `punctuation.definition.attribute.end.gst.smalltalk` (for `>`)
        *   Attribute Name/Keyword (e.g., `primitive:`, `category:`): `entity.name.tag.attribute.gst.smalltalk` (or `entity.name.function.keyword.attribute.gst.smalltalk`)
        *   Attribute Value (e.g., `1`, `'accessing'`): Scoped according to its type (e.g., `constant.numeric.integer.smalltalk`, `string.quoted.single.attribute-value.gst.smalltalk`).
    *   **Rationale/Convention Source:** `meta.attribute` and `entity.name.tag` are common for attribute-like structures.

*   **GST Syntactic Element:** File-out Chunk Separator (GST specific)
    *   **Brief Description/Example:** `!` on a line by itself or after code.
    *   **Proposed TextMate Scopes:** `punctuation.separator.chunk.gst.smalltalk`
        *   Could also be part of a larger `meta.chunk.gst.smalltalk` if the line has significance.
    *   **Rationale/Convention Source:** Specific punctuation for a GST structural element.

This mapping strategy provides a foundation for developing a robust and semantically rich TextMate grammar for GNU Smalltalk. It prioritizes standard conventions while accommodating GST-specific features.
