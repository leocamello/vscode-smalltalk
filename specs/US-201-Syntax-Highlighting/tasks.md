# Tasks & Acceptance Criteria

## Group 0: Research (US-200)
- [x] AC200.1: Analyze GST source code (lex.c, gst-parse.c).
- [x] AC200.2: Document syntax rules and findings in `docs/research/`.
- [x] AC200.3: Research TextMate best practices.
- [x] AC200.4: Collect GST code snippets for validation.
- [x] AC200.5: Define implementation strategy (YAML -> JSON).

## Group 6: Verification & Tests (DoD)
- [x] AC9.1: Create test infrastructure for grammar verification.
- [x] AC9.2: Generate snapshots for US-200 test cases (`docs/research/gst-syntax/test-cases`).
- [x] AC9.3: Verify all snapshots match expected output.
- [x] DoD.1: Add `test:grammar` script to CI/CD (package.json).

## Group 1: Basics
- [x] AC1.1: Highlight Comments (`"..."`).
- [x] AC1.2: Highlight Shebang (`#!...`).
- [x] AC1.3: Highlight Strings (`'...'`) with escapes.
- [x] AC1.4: Highlight Characters (`$a`, `$\n`).

## Group 2: Numbers & Constants
- [x] AC2.1: Integers (`123`, `-123`).
- [x] AC2.2: Floats (`1.23`, `1.2e10`).
- [x] AC2.3: Radix (`16rFF`).
- [x] AC2.4: Scaled Decimal (`1.23s2`).
- [x] AC2.5: Symbols (`#foo`, `#'foo bar'`).
- [x] AC2.6: Literal Arrays (`#(1 2 3)`).
- [x] AC2.7: Byte Arrays (`#[1 2 3]`).

## Group 3: Variables & Keywords
- [x] AC3.1: Reserved words (`self`, `super`, `nil`, `true`, `false`, `thisContext`).
- [x] AC3.2: Argument variables (`:foo`).
- [x] AC3.3: Temporary variables (`| foo |`).
- [x] AC3.4: Global/Class variables (Capitalized).

## Group 4: Control Flow & Messages
- [x] AC4.1: Assignment (`:=`).
- [x] AC4.2: Return (`^`).
- [x] AC4.3: Blocks (`[ ... ]`).
- [x] AC4.4: Unary Messages (`obj foo`).
- [x] AC4.5: Binary Messages (`+`, `-`, `,`, etc.).
- [x] AC4.6: Keyword Messages (`foo: bar:`).
- [x] AC4.7: Cascades (`;`).

## Group 5: GNU Smalltalk Extensions
- [x] AC5.1: Namespaces (`Namespace current: ...`).
- [x] AC5.2: Eval syntax (`Eval [ ... ]`).
- [x] AC5.3: Primitives (`<primitive: ...>`).
- [x] AC5.4: Chunk format (`!`).
- [x] AC5.5: Class Extensions (`String extend [`).
