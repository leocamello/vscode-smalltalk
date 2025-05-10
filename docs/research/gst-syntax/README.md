# GNU Smalltalk Syntax Analysis for TextMate Grammar

This directory contains the detailed syntax analysis and strategic planning for the development of a new TextMate grammar for GNU Smalltalk, supporting the `vscode-smalltalk` extension (related to US-200 and US-201).

The research has been broken down into the following documents for clarity and maintainability:

*   **[`00-introduction-and-legacy-assessment.md`](./00-introduction-and-legacy-assessment.md):** Introduces the analysis and assesses the original TextMate grammar (`smalltalk.tmLanguage.json`) as a baseline.
*   **[`01-gst-lexer-token-analysis.md`](./01-gst-lexer-token-analysis.md):** Details the token inventory and recognition logic derived from the GNU Smalltalk lexer source code.
*   **[`02-gst-parser-syntactic-structures.md`](./02-gst-parser-syntactic-structures.md):** Describes how GNU Smalltalk tokens are combined by the parser to form valid syntactic structures.
*   **[`03-gap-analysis-existing-vs-gst.md`](./03-gap-analysis-existing-vs-gst.md):** Provides a comprehensive gap analysis comparing the capabilities of the existing TextMate grammar against the requirements of GNU Smalltalk syntax.
*   **[`04-textmate-mapping-strategy.md`](./04-textmate-mapping-strategy.md):** Outlines the proposed TextMate scope naming conventions and mapping strategy for various GNU Smalltalk syntactic elements.
*   **[`05-recommendations-and-grammar-plan.md`](./05-recommendations-and-grammar-plan.md):** Presents recommendations for the grammar development approach, file format, proposed high-level structure, and confirms the next steps for implementation.

These documents collectively provide the foundation for building a robust and accurate TextMate grammar for GNU Smalltalk.
