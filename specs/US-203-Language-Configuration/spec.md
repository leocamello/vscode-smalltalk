# Specification: Language Configuration

**ID**: US-203
**Feature**: Language Configuration
**Status**: In Progress
**Owner**: Leonardo Nascimento

## 1. Overview
Configure VS Code's declarative language features for GNU Smalltalk to support bracket matching, comment toggling, and basic auto-indentation.

## 2. Goals
*   Enable correct comment toggling (`Cmd+/`).
*   Support bracket matching and auto-closing for `[]`, `()`, `{}` (GST dynamic arrays).
*   Provide basic indentation rules (indent after `[`, outdent after `]`).

## 3. User Stories
**US-203**: As a Smalltalk developer, I want reliable language configuration so editing is intuitive.
*   **AC1**: `language-configuration.json` exists and referenced.
*   **AC2**: Standard brackets defined: `[]`, `()`, `{}`.
*   **AC3**: Block comments defined as `"` ... `"`.
*   **AC4**: Basic auto-indentation rules defined.
    *   Increase indent after opening brackets.
    *   Decrease indent after closing brackets.
*   **AC5**: Auto-closing pairs configured for quotes and brackets.

## 4. Technical Design
*   **File**: `language-configuration.json`.
*   **Comments**:
    *   `blockComment`: `["\"", "\"\"]`
    *   *Note*: We will NOT define `lineComment` because Smalltalk doesn't have a prefix-only comment syntax. Toggling comments should wrap in quotes.
*   **Brackets**: `["[", "]"], ["(", ")"], ["{", "}"]`.
*   **Indentation Rules**:
    *   `increaseIndentPattern`: `^.*(\[[^\]]*|{|#\(|#\[)$` (Simplified: ends with open bracket)
    *   `decreaseIndentPattern`: `^\s*(\]|\}|)$` (Starts with close bracket)

## 5. Risks
*   Regex-based indentation is never perfect compared to AST-based (Phase 2 LSP), but sufficient for basic editing.
