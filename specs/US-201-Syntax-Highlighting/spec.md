# Specification: GNU Smalltalk Syntax Highlighting

**ID**: US-201 (Includes US-200 Research)
**Feature**: Syntax Highlighting
**Status**: Implemented
**Owner**: Leonardo Nascimento

## 1. Overview
Provide robust syntax highlighting for GNU Smalltalk (`.st` files) in VS Code. This is the foundational feature for the extension.

## 2. Goals
*   Accurate highlighting of standard Smalltalk-80 syntax.
*   Support for GNU Smalltalk extensions (Namespaces, Primitives, Eval, `!`).
*   Robust handling of comments, strings, and block nesting.
*   Integration with VS Code's TextMate grammar engine (YAML -> JSON).

## 3. User Stories

**US-200**: As a developer, I want to understand the best approach for Smalltalk grammar so I don't reinvent the wheel.
*   **AC1**: Research existing grammars.
*   **AC2**: Define file structure (`syntaxes/gnu-smalltalk.YAML-tmLanguage`).

**US-201**: As a Smalltalk developer, I want my code highlighted so I can read it easily.
*   **AC1**: Comments (quoted `"`).
*   **AC2**: Strings (single quote `'`) and Characters (`$c`).
*   **AC3**: Numbers (Integers, Floats, Scaled, Radix).
*   **AC4**: Symbols (`#symbol`) and Arrays (`#( ... )`).
*   **AC5**: Variables (Global, Temporary, Argument, Instance).
*   **AC6**: Pseudo-variables (`self`, `super`, `true`, `false`, `nil`).
*   **AC7**: Block structures (`[...]`) and Block Arguments (`:arg |`).
*   **AC8**: Message Sends (Unary, Binary, Keyword).
*   **AC9**: GNU Smalltalk Specifics (Namespaces `Namespace current:`, Primitives `<primitive: ...>`, File-out separators `!`).

## 4. Technical Design
*   **Engine**: TextMate Grammar (Regex based).
*   **Source**: `syntaxes/gnu-smalltalk.YAML-tmLanguage`.
*   **Build**: Compiled to `syntaxes/gnu-smalltalk.tmLanguage.json` via `js-yaml`.
*   **Scope Name**: `source.smalltalk.gnu`.

## 5. Risks & Limitations
*   **Limitation**: TextMate grammars cannot handle semantic highlighting (e.g., knowing if `foo` is a class or a global without context).
*   **Risk**: Complex nesting of blocks and arrays can be tricky in Regex.
