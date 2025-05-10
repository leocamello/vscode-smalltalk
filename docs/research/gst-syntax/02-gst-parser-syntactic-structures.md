# GNU Smalltalk Parser: Syntactic Structures

This section details how tokens are combined into valid Smalltalk constructs by the GNU Smalltalk parser (`gst-parse.c`).

## C. Syntactic Structures (Inferred from `gst-parse.c`)

The parser (`gst-parse.c`) defines how tokens are combined into valid Smalltalk constructs.

*   **Expressions:**
    *   **Primary:** The most basic unit. Can be a literal, variable, block, parenthesized expression, or array constructor.
    *   **Cascades:** `message (';' selector_and_args)*`. Parsed by `parse_cascaded_messages()`.

*   **Statements:**
    *   A sequence of expressions. Each expression is optionally preceded by `^` (return) and typically followed by `.` (period).
    *   Structure: `( '^' expression | expression ) ( '.' ('^' expression | expression) )* '.'?`

*   **Blocks:**
    *   Parsed by `parse_block()`.
    *   **Temporaries:** `( '|' (IDENTIFIER)* '|' )?`. Parsed by `parse_temporaries()`.
