# Specification: GNU Smalltalk Code Snippets

**ID**: US-202
**Feature**: Code Snippets
**Status**: In Progress
**Owner**: Leonardo Nascimento

## 1. Overview
Provide a set of productive code snippets for GNU Smalltalk to speed up development and ensure idiomatic code usage.

## 2. Goals
*   Provide snippets for common structures (control flow, methods, blocks).
*   Ensure snippets work well with the `source.smalltalk.gnu` scope.
*   Use VS Code's snippet features (placeholders, tabstops, variables).

## 3. User Stories
**US-202**: As a Smalltalk developer, I want snippets for common patterns to write code faster.
*   **AC1**: `snippets.json` configured in `package.json`.
*   **AC2**: Essential snippets implemented:
    *   **Control Flow**: `ifTrue:`, `ifFalse:`, `ifTrue:ifFalse:`, `ifNil:`, `do:`, `collect:`, `select:`, `reject:`.
    *   **Methods**: `method` (instance), `cmethod` (class), `cla` (class def).
    *   **Blocks**: `[...]`.
    *   **Variables**: `| temp |` (often handled manually, but maybe a snippet for the bar structure).
    *   **GST Specifics**: `Eval []`, `Namespace current:`.
*   **AC3**: Concise and intuitive prefixes.
*   **AC4**: Meaningful descriptions.
*   **AC5**: Correct syntax ($1, $2, etc.).
*   **AC6**: Idiomatic GNU Smalltalk.

## 4. Technical Design
*   **File**: `snippets/snippets.json`.
*   **Scope**: `source.smalltalk.gnu`.
*   **Structure**: Standard VS Code JSON format.

## 5. Snippet Inventory (Planned)

| Trigger | Description | Body Pattern |
| :--- | :--- | :--- |
| `if` | ifTrue:ifFalse: | `... ifTrue: [ $1 ] ifFalse: [ $2 ]` |
| `ift` | ifTrue: | `... ifTrue: [ $1 ]` |
| `iff` | ifFalse: | `... ifFalse: [ $1 ]` |
| `do` | do: loop | `do: [ :${1:each} | $0 ]` |
| `col` | collect: | `collect: [ :${1:each} | $0 ]` |
| `sel` | select: | `select: [ :${1:each} | $0 ]` |
| `rej` | reject: | `reject: [ :${1:each} | $0 ]` |
| `met` | Instance Method | `${1:message} [\n\t<category: '${2:accessing}'>\n\t$0\n]` |
| `cmet` | Class Method | `${1:Class} class >> ${2:message} [\n\t$0\n]` |
| `eval` | Eval block | `Eval [\n\t$0\n]` |
| `ns` | Namespace | `Namespace current: ${1:Name} [\n\t$0\n]` |
