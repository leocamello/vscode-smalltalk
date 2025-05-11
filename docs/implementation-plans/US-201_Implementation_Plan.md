# US-201: Refine Syntax Highlighting Grammar - Implementation Plan

**Overall Goal (Revised slightly for clarity on output):**
**"Your mission for US-201 is to author a new, maintainable, and highly accurate GNU Smalltalk TextMate grammar in YAML format (`syntaxes/gnu-smalltalk.YAML-tmLanguage`). This grammar must implement the scope mapping strategy defined in `docs/research/gst-syntax/04-textmate-mapping-strategy.md`, cover the syntactic constructs identified in `01-gst-lexer-token-analysis.md` and `02-gst-parser-syntactic-structures.md`, and address the deficiencies noted in `03-gap-analysis-existing-vs-gst.md`. A build process will convert this YAML source to a JSON grammar (`syntaxes/gnu-smalltalk.tmLanguage.json`) for use by VS Code. The final grammar must be validated against the test cases in `docs/research/gst-syntax/test-cases/` and align with the high-level structure proposed in `05-recommendations-and-grammar-plan.md`."**

---

**Core Development Workflow (Important!)**

For all subsequent steps (Step 2 onwards) in this plan:

1.  **Edit YAML:** All grammar rules and patterns will be implemented and modified in the YAML source file: `syntaxes/gnu-smalltalk.YAML-tmLanguage`.
2.  **Build JSON:** After making changes to the YAML file, you **must** run the build script (e.g., `npm run build:grammar` or `yarn build:grammar`) to regenerate the `syntaxes/gnu-smalltalk.tmLanguage.json` file.
3.  **Test in VS Code:** Launch or reload the VS Code Extension Development Host. VS Code will load the generated `syntaxes/gnu-smalltalk.tmLanguage.json` file for syntax highlighting.

This **Edit YAML -> Build JSON -> Test** cycle is crucial for all grammar development work in this user story.

---

**Phase A: Foundation & Core Lexical Elements**

**Step 1: Grammar File Setup, YAML-to-JSON Conversion, and VS Code Integration**

* **Primary Goal:** Create the new **YAML source** TextMate grammar file, define its top-level metadata, establish its basic structural skeleton. Set up a build process to convert the YAML to JSON. Integrate the **output JSON grammar** into the VS Code extension, replacing the old grammar.
* **Specific Actions & Outputs:**
    1.  **Create YAML Grammar Source File & Define Metadata:**
        * **Action:** Create `syntaxes/gnu-smalltalk.YAML-tmLanguage`.
        * **Action:** Populate with `name: GNU Smalltalk`, `scopeName: source.smalltalk.gnu`, `fileTypes: ['st', 'gst']`, and a new `uuid`.
        * **Reference:** `docs/research/gst-syntax/05-recommendations-and-grammar-plan.md` (Section 3: Top-level Metadata).
        * **Output:** `syntaxes/gnu-smalltalk.YAML-tmLanguage` source file with correct metadata.
    2.  **Establish Basic YAML Structure (`patterns`, `repository`):**
        * **Action:** In `syntaxes/gnu-smalltalk.YAML-tmLanguage`, create the top-level `patterns:` array. Populate it with `include: '#repository_key_name'` entries for each of the high-level conceptual constructs listed in "Top-Level `patterns` (Conceptual Order)" from `05-recommendations-and-grammar-plan.md` (e.g., `include: '#shebang'`, `include: '#comments'`, `include: '#gst_definition_structures'`, etc., down to `include: '#punctuation_general'`).
        * **Action:** Create the top-level `repository:` map. For each `#repository_key_name` referenced in the `patterns:` array, AND for each of the "Key `repository` Item Groups" also listed in `05-recommendations-and-grammar-plan.md` (e.g., `all_literals`, `literals_numeric`, `identifiers_and_variables`, etc.), create a corresponding placeholder entry. These can initially contain just a comment like `# TODO: Implement [repository_key_name]`.
        * **Reference:** `docs/research/gst-syntax/05-recommendations-and-grammar-plan.md` (Section 3: Proposed High-Level Grammar Structure/Organization).
        * **Output:** YAML file with a `patterns:` array and a `repository:` map that mirrors the planned high-level structure, filled with includes and placeholder repository items.
    3.  **Implement YAML-to-JSON Build Step:**
        * **Action:** Add `js-yaml` as a development dependency to the project (e.g., `npm install --save-dev js-yaml`).
        * **Action:** Create a script in `package.json` (e.g., `"build:grammar": "js-yaml syntaxes/gnu-smalltalk.YAML-tmLanguage -o syntaxes/gnu-smalltalk.tmLanguage.json"`) to perform this conversion.
        * **Action:** Configure `.gitignore` and `.vscodeignore`:
            *   **`syntaxes/gnu-smalltalk.tmLanguage.json` (Generated JSON):**
                *   **`.gitignore`**: For initial simplicity and to ensure the repository is always runnable, we will *not* add this to `.gitignore` for now. The generated JSON file will be committed.
                *   **`.vscodeignore`**: **Crucially, do NOT add this file to `.vscodeignore`.** It must be packaged with the extension for runtime use.
            *   **`syntaxes/gnu-smalltalk.YAML-tmLanguage` (Source YAML):**
                *   **`.gitignore`**: Do NOT add this file; it's source code.
                *   **`.vscodeignore`**: *Optional*. For now, we will NOT add it, allowing the YAML source to be packaged for reference.
        * **Output:** A working build command that converts the YAML source to a JSON grammar file. `syntaxes/gnu-smalltalk.tmLanguage.json` generated. `.gitignore` and `.vscodeignore` configured appropriately.
    4.  **Integrate *JSON Grammar* into `package.json`:**
        * **Action:** Modify `package.json`'s `contributes.grammars` section.
        * **Action:** Point the grammar entry to the *output JSON file*: `./syntaxes/gnu-smalltalk.tmLanguage.json`. Ensure `language` ID (e.g., `smalltalk`) and `scopeName` (e.g., `source.smalltalk.gnu`) are consistent. Remove/replace old grammar references.
        * **Action:** Ensure `contributes.languages.extensions` includes both `.st` and `.gst`.
        * **Output:** Updated `package.json`.
    5.  **Initial Test (VS Code Integration):**
        * **Action:** Run the grammar build script (e.g., `npm run build:grammar`).
        * **Action:** Launch the extension development host. Open an `.st` or `.gst` file.
        * **Expected Behavior:** No Smalltalk-specific highlighting (grammar is functionally empty). "Developer: Inspect Editor Tokens and Scopes" should show `source.smalltalk.gnu` as the language scope. This confirms `package.json` and the build process work correctly, and VS Code is loading the generated JSON.
* **Key Validation Questions for Step 1:**
    * Is `syntaxes/gnu-smalltalk.YAML-tmLanguage` correctly created with metadata and the full placeholder structure?
    * Is `js-yaml` added as a dev dependency?
    * Does the `build:grammar` script successfully convert the YAML to a valid `syntaxes/gnu-smalltalk.tmLanguage.json` file without errors?
    * Are `.gitignore` and `.vscodeignore` correctly configured so that the generated JSON is available to the packaged extension, and the YAML source is tracked by git?
    * Is `package.json` correctly updated to point to the *generated JSON file* for the `smalltalk` language ID with `scopeName: source.smalltalk.gnu`, and are `.st` and `.gst` extensions associated?
    * Does VS Code load the new (empty, but converted) grammar, confirmed by checking the language scope on `.st`/`.gst` files and the absence of loading errors in developer tools?

---

**Step 2: Implement Shebang & Comments**
* **Primary Goal:** Implement patterns for shebang lines and double-quoted comments in the **YAML source file**, ensuring they are correctly scoped and appear first in the matching order.
* **Guidance for Engineer:** Use `docs/research/gst-syntax/04-textmate-mapping-strategy.md` (Section 5.1) for scopes and `docs/research/gst-syntax/01-gst-lexer-token-analysis.md` for token details.
* **Specific Actions & Outputs:**
    1.  **Shebang (`#!`):**
        * **Action:** Implement the `shebang` pattern in the `repository` section of `syntaxes/gnu-smalltalk.YAML-tmLanguage`.
        * **Pattern:** Regex for lines starting `#!` to EOL.
        * **Scopes:** `comment.line.shebang.gst.smalltalk` (overall), `punctuation.definition.comment.shebang.gst.smalltalk` (for `#!`).
        * Ensure `include: '#shebang'` is the first item in the top-level `patterns` in the YAML.
        * **Output:** Completed `shebang` repository item in the YAML file.
    2.  **Double-Quoted Comments:**
        * **Action:** Implement the `comments` pattern in the `repository` section of `syntaxes/gnu-smalltalk.YAML-tmLanguage`.
        * **Pattern:** Regex for `"..."`, handling multi-line correctly.
        * **Scopes:** `comment.block.double-quoted.smalltalk`, `punctuation.definition.comment.begin.smalltalk`, `punctuation.definition.comment.end.smalltalk`.
        * Ensure `include: '#comments'` is in the top-level `patterns` in the YAML (usually after shebang).
        * **Output:** Completed `comments` repository item in the YAML file.
* **Iterative Testing (Step 2):**
    * **Action:** After implementing in YAML, run `npm run build:grammar`.
    * **Test Files:** `docs/research/gst-syntax/test-cases/15_edge_cases_shebang_whitespace.st`, `docs/research/gst-syntax/test-cases/04_comments.st`. Add new simple files focusing only on these constructs.
    * **Action:** Verify scopes using "Developer: Inspect Editor Tokens and Scopes" in the extension host.
    * **Expected Behavior:** Shebangs and comments are correctly highlighted.
* **Key Validation Questions for Step 2:**
    * Are shebangs and comments correctly scoped as per `04-textmate-mapping-strategy.md` after building and testing?
    * Does the comment pattern handle multi-line comments correctly and avoid consuming parts of (later-to-be-defined) string literals?

---

**Phase B: Literals**

**Step 3: Implement String and Character Literals**

* **Primary Goal:** Accurately highlight string literals (single-quoted with escapes) and both simple and GST-specific character literals.
* **Guidance for Engineer:** For each, consult `01-gst-lexer-token-analysis.md` (Token Recognition), `03-gap-analysis-existing-vs-gst.md` (Section 3.1.2, 3.1.3 for issues to avoid), and `04-textmate-mapping-strategy.md` (Section 5.2.2, 5.2.3 for scopes).
* **Specific Actions & Outputs:**
    1.  **Setup `all_literals` include:**
        * **Action:** In `repository`, ensure `all_literals` exists. Add `include: '#literals_string'` and `include: '#literals_character'` to its `patterns` array.
        * **Action:** Ensure `include: '#all_literals'` is present in the main `patterns` list (e.g., after `#comments`).
        * **Output:** Updated `all_literals` in repository.
    2.  **String Literals:**
        * **Action:** Implement the `literals_string` pattern in the `repository`.
        * **Pattern:** Regex for `'...'`, handling `''` escape.
        * **Scopes:** `string.quoted.single.smalltalk`, `punctuation.definition.string.begin.smalltalk`/`end.smalltalk`, `constant.character.escape.apostrophe.smalltalk`.
        * **Output:** Completed `literals_string` repository item.
    3.  **Character Literals:**
        * **Action:** Implement `literals_character` in `repository`. This item should itself contain a `patterns` array to match different character literal forms in order (GST numeric before simple, if there's an overlap risk).
        * **Sub-Pattern (GST Numeric):** For `$<65>`, `$<16rFF>`. Scopes: `constant.character.numeric.gst.smalltalk`, `punctuation.definition.character.numeric.begin.gst.smalltalk`/`end.gst.smalltalk`. Consider capturing the internal number/radix for more detailed (optional) sub-scoping by including relevant numeric patterns.
        * **Sub-Pattern (Simple):** For `$a`. Scopes: `constant.character.simple.smalltalk`, `punctuation.definition.character.smalltalk`.
        * **Output:** Completed `literals_character` repository item with ordered sub-patterns.
* **Iterative Testing (Step 3):**
    * **Test Files:** `docs/research/gst-syntax/test-cases/02_literals_strings_chars_symbols.st`.
    * **Action:** Verify scopes. Check correct handling of string escapes and the two types of character literals.
    * **Expected Behavior:** Strings and character literals are correctly highlighted. Comments/shebang still work.
* **Key Validation Questions for Step 3:**
    * Are string escapes (`''`) correctly handled and scoped?
    * Is there a clear distinction in highlighting between `$a` and `$<65>`? Are their respective scopes from `04-...` applied?

---

**Step 4: Implement Numeric Literals (Comprehensive)**

* **Primary Goal:** Implement patterns for all GNU Smalltalk numeric literal forms, including integers (with/without radix, with/without underscores), floats (various exponent/suffix forms, with/without underscores, radix), and scaled decimals.
* **Guidance for Engineer:** This is complex. Refer heavily to `01-gst-lexer-token-analysis.md` (Number Literals), `03-gap-analysis-existing-vs-gst.md` (Section 3.1.1 for common pitfalls), and `04-textmate-mapping-strategy.md` (Section 5.2.1 for detailed scopes). Pay close attention to the order of patterns if defined as separate regexes to avoid incorrect matching (e.g., an integer pattern accidentally consuming part of a float).
* **Specific Actions & Outputs:**
    1.  **Numeric Literals Repository Item:**
        * **Action:** Create/Update `literals_numeric` in the `repository`. This item will contain a `patterns` array with multiple regexes, ordered carefully (e.g., floats and scaled decimals before integers if prefixes overlap).
        * **Action:** Ensure `include: '#literals_numeric'` is in `all_literals.patterns`.
        * **Output:** `literals_numeric` repository item ready for sub-patterns.
    2.  **Implement Integer Patterns:**
        * **Sub-Pattern (Radix Integers):** For `2r1011_0101`, `16rFF_AB`. Scope: `constant.numeric.integer.radix.smalltalk`. Sub-scopes for radix prefix (`constant.numeric.radix.base.smalltalk`) and number part recommended. Handle underscores.
        * **Sub-Pattern (Simple Integers):** For `123`, `1_000_000`. Scope: `constant.numeric.integer.smalltalk`. Handle underscores.
        * **Order:** Radix integers might need to be matched before simple integers if not carefully constructed.
    3.  **Implement Float Patterns:**
        * **Sub-Pattern:** For `1.0`, `1.0e2`, `123e2` (integer with exponent), `1.2d`, `1_000.500e+5`. Scope: `constant.numeric.float.smalltalk`. Sub-scope for exponent indicator (`constant.numeric.float.exponent-indicator.smalltalk`). Handle underscores and various exponent forms (`e`, `d`, `q`). Consider radix floats if applicable from GST spec.
    4.  **Implement Scaled Decimal Patterns:**
        * **Sub-Pattern:** For `1.2s2`, `1s`, `1_234.5s-2`, `16rA.Bs2`. Scope: `constant.numeric.scaled-decimal.gst.smalltalk`. Sub-scopes for scale indicator (`constant.numeric.scaled-decimal.scale-indicator.gst.smalltalk`) and scale value (`constant.numeric.integer.scale-value.gst.smalltalk`). Handle underscores and radix.
* **Iterative Testing (Step 4):**
    * **Test Files:** `docs/research/gst-syntax/test-cases/01_literals_numbers.st`. Add more examples covering all underscore, radix, exponent, and scale combinations.
    * **Action:** Exhaustively test all numeric forms. Check for correct precedence (e.g., `123e2` is a float, not integer `123` then `e2`).
    * **Expected Behavior:** All specified numeric literals are correctly highlighted with their distinct scopes.
* **Key Validation Questions for Step 4:**
    * Are underscores correctly handled in all numeric types?
    * Is radix notation correctly handled for integers, floats, and scaled decimals?
    * Is the distinction between integers and floats (especially `123e2` vs. `123`) correct?
    * Are scaled decimals fully captured, including those without a decimal point before the `s`?

---

**Step 5: Implement Symbol, Static Array, and Byte Array Literals**

* **Primary Goal:** Implement patterns for Smalltalk symbols (all forms), static array literals `#(...)`, and byte array literals `#[...]`.
* **Guidance for Engineer:** Refer to `01-...` (Symbol, Array, Byte Array Literals), `03-...` (Sections 3.1.4, 3.1.5, 3.1.6), and `04-...` (Sections 5.2.4, 5.2.5, 5.2.6).
* **Specific Actions & Outputs:**
    1.  **Symbol Literals:**
        * **Action:** Implement `literals_symbol` in `repository`. Use a `patterns` array for ordered matching of symbol types. Add `include: '#literals_symbol'` to `all_literals.patterns`.
        * **Sub-Pattern (Quoted String Symbols):** For `#'a string symbol'`. Scope: `constant.other.symbol.quoted.smalltalk`, with internal `string.quoted.single.symbol.smalltalk`. Match this first if other symbol patterns are too greedy.
        * **Sub-Pattern (Operator/Numeric-like Symbols):** For `#+`, `#~=`, `#123`. Scopes: `constant.other.symbol.operator.smalltalk`, `constant.other.symbol.numeric.smalltalk` (potentially including numeric patterns for the content). Punctuation for `#`: `punctuation.definition.symbol.smalltalk`.
        * **Sub-Pattern (Identifier-like & Keyword-like Symbols):** For `#aSymbol`, `#ifTrue:`. Scopes: `constant.other.symbol.identifier.smalltalk`, `constant.other.symbol.keyword.smalltalk`. Punctuation for `#`: `punctuation.definition.symbol.smalltalk`.
        * **Output:** Completed `literals_symbol` repository item.
    2.  **Static Array Literals (`#(...)`):**
        * **Action:** Implement `literals_array_static` in `repository`. Add `include: '#literals_array_static'` to `all_literals.patterns`.
        * **Pattern:** For `#(` ... `)`. Scopes: `meta.array.literal.smalltalk`, `punctuation.definition.array.begin.smalltalk` (`#(`), `punctuation.definition.array.end.smalltalk` (`)`).
        * **Recursion:** The content within the array parentheses should `include: '#all_literals'` and potentially `#identifier_usages` (once defined) to scope nested elements correctly.
        * **Output:** Completed `literals_array_static` repository item.
    3.  **Byte Array Literals (`#[...]`):**
        * **Action:** Implement `literals_byte_array` in `repository`. Add `include: '#literals_byte_array'` to `all_literals.patterns`.
        * **Pattern:** For `#[` ... `]`. Scopes: `meta.bytearray.literal.smalltalk`, `punctuation.definition.bytearray.begin.smalltalk` (`#[`), `punctuation.definition.bytearray.end.smalltalk` (`)`).
        * **Content:** Elements inside should be scoped as `constant.numeric.integer.byte.smalltalk`. The pattern should ideally only match valid byte integers (0-255, no radix, though lexer output might be just INTEGER_LITERAL).
        * **Output:** Completed `literals_byte_array` repository item.
* **Iterative Testing (Step 5):**
    * **Test Files:** `docs/research/gst-syntax/test-cases/02_literals_strings_chars_symbols.st` (for symbols), `docs/research/gst-syntax/test-cases/03_literals_arrays.st`.
    * **Action:** Verify all symbol forms. Test nested arrays and arrays containing various literal types.
    * **Expected Behavior:** All symbol types, static arrays, and byte arrays are correctly scoped, including their contents.
* **Key Validation Questions for Step 5:**
    * Are all three major symbol forms (identifier/keyword, quoted, operator/numeric) correctly distinguished and scoped?
    * Do static arrays correctly scope their diverse nested elements?
    * Are byte array elements correctly scoped as byte integers?

---

**Step 6: Implement GST-Specific Literals/Constants**

* **Primary Goal:** Implement patterns for GST-specific literal-like constructs: dynamic array constructors `{...}`, binding constants `#{...}`, and compile-time constants `##(...)`.
* **Guidance for Engineer:** Refer to `01-...`, `03-...` (Sections 3.9 for these specific items), and `04-...` (Section 5.2.7).
* **Specific Actions & Outputs:**
    1.  **GST Special Literals Repository Item:**
        * **Action:** Create `gst_literals_special` in `repository`. Add `include: '#gst_literals_special'` to `all_literals.patterns`. This item will contain a `patterns` array for the following.
        * **Output:** `gst_literals_special` repository item ready for sub-patterns.
    2.  **Dynamic Array Constructor (`{...}`):**
        * **Sub-Pattern:** For `{ elem1. elem2 }`. Scopes: `meta.array.constructor.dynamic.gst.smalltalk`, delimiters (`punctuation.definition.array.constructor.begin.gst.smalltalk`/`end.gst.smalltalk`), separator (`punctuation.separator.array.constructor.gst.smalltalk` for `.`). Elements should be recursively scoped (e.g., `include: '$self'` or specific expression patterns).
    3.  **Binding Constants (`#{...}`):**
        * **Sub-Pattern:** For `#{SomeClass}`. Scopes: `constant.other.binding.gst.smalltalk`, delimiters (`punctuation.definition.binding.begin.gst.smalltalk`/`end.gst.smalltalk`). Content (e.g., `SomeClass`) should ideally be scoped as `entity.name.type.class.smalltalk` or `variable.other.global.smalltalk`.
    4.  **Compile-Time Constants (`##(...)`):**
        * **Sub-Pattern:** For `##(1+2)`. Scopes: `meta.compile-time-constant.gst.smalltalk` (overall), `constant.other.compile-time.gst.smalltalk` (for `##`), delimiters (`punctuation.definition.compile-time.begin.gst.smalltalk`/`end.gst.smalltalk`). Expression within `()` should be recursively scoped.
* **Iterative Testing (Step 6):**
    * **Test Files:** `docs/research/gst-syntax/test-cases/13_gst_specific_attributes_array_constructor.st` (for `{}`), `docs/research/gst-syntax/test-cases/14_gst_specific_binding_compile_time_constants.st`.
    * **Action:** Test these GST-specific forms, including nesting and varied content.
    * **Expected Behavior:** Dynamic arrays, binding constants, and compile-time constants are correctly scoped.
* **Key Validation Questions for Step 6:**
    * Are the delimiters and internal content of these GST-specific constants correctly scoped according to the mapping strategy?
    * How is recursion handled for their internal expressions/elements?

---

**Phase C: Identifiers, Variables, and Basic Statements**

**Step 7: Implement Identifiers, Pseudo-Variables, and Basic Punctuation**

* **Primary Goal:** Implement patterns for general identifiers (class names, other globals, fallback for variables), pseudo-variables, and basic punctuation not tied to complex structures yet.
* **Guidance for Engineer:** Refer to `01-...`, `03-...` (Section 3.3, 3.8), and `04-...` (Sections 5.3, 5.4). The order of inclusion in main `patterns` is important here: these general identifier patterns should usually come *after* more specific literal patterns to avoid conflicts (e.g., `true` as a keyword vs. an identifier starting with `true`).
* **Specific Actions & Outputs:**
    1.  **Identifiers and Variables Repository Item:**
        * **Action:** Create/Update `identifiers_and_variables` in `repository`. This will hold patterns for different identifier types. Add `include: '#identifiers_and_variables'` to the main `patterns` list, considering its order relative to literals.
        * **Output:** `identifiers_and_variables` repository item.
    2.  **Pseudo-Variables:**
        * **Sub-Pattern (within `identifiers_and_variables`):** For `self`, `super`, `true`, `false`, `nil`, `thisContext`. Use exact word matches.
        * **Scopes:** `variable.language.self.smalltalk`, `variable.language.this-context.gst.smalltalk`, `constant.language.boolean.smalltalk`, `constant.language.nil.smalltalk`.
        * **Order:** These should be matched *before* general identifiers.
    3.  **Class Name Convention (General):**
        * **Sub-Pattern (within `identifiers_and_variables`):** A general pattern for `CapitalizedIdentifiers` (e.g., `\b[A-Z]\w*\b`). Scope: `entity.name.type.class.smalltalk`. This acts as a fallback; more specific class name uses (declarations) will be handled later.
    4.  **General Identifiers (Fallback):**
        * **Sub-Pattern (within `identifiers_and_variables`):** A pattern for lowercase-starting identifiers (e.g., `\b[a-z_]\w*\b`). Scope: `variable.other.smalltalk`. This is for identifiers not otherwise classified.
    5.  **Basic Punctuation Repository Item:**
        * **Action:** Create `punctuation_general` in `repository`. Add `include: '#punctuation_general'` to the main `patterns` list.
        * **Output:** `punctuation_general` repository item.
    6.  **Implement Punctuation Patterns (within `punctuation_general`):**
        * Statement Terminator: `.` Scope: `punctuation.terminator.statement.smalltalk`.
        * Cascade Separator: `;` Scope: `punctuation.separator.cascade.smalltalk`.
        * Grouping Parentheses `()`: Scopes: `meta.expression.parenthesized.smalltalk` (overall if helpful, or just delimiters), `punctuation.section.group.begin.smalltalk`, `punctuation.section.group.end.smalltalk`. (Body of `()` should allow recursive matching of expressions).
* **Iterative Testing (Step 7):**
    * **Test Files:** `docs/research/gst-syntax/test-cases/05_core_identifiers_pseudo_vars.st`, `docs/research/gst-syntax/test-cases/09_core_method_definitions_statements.st` (for `.`, `()`).
    * **Action:** Test pseudo-variables, class name convention, general identifiers, and basic punctuation.
    * **Expected Behavior:** These elements are correctly scoped. Ensure `true` is `constant.language.boolean.smalltalk` and not `variable.other.smalltalk`.
* **Key Validation Questions for Step 7:**
    * Are pseudo-variables correctly scoped and prioritized over general identifier patterns?
    * Does the capitalized identifier pattern correctly scope potential class names?
    * Do `.` and `;` receive their correct punctuation scopes?

---

**Step 8: Implement Assignments, Returns, and Block Structures**

* **Primary Goal:** Implement patterns for assignment (`:=`, `_`), return (`^`), and full block structures (`[...]` including parameters and temporaries).
* **Guidance for Engineer:** Refer to `01-...`, `03-...` (Sections 3.5, 3.6, 3.8), and `04-...` (Sections 5.4, 5.6). Blocks are recursive and central.
* **Specific Actions & Outputs:**
    1.  **Assignments and Returns Repository Item:**
        * **Action:** Create `assignments_and_returns` in `repository`. Add `include: '#assignments_and_returns'` to main `patterns`.
        * **Output:** `assignments_and_returns` repository item.
    2.  **Assignment Operators (within `assignments_and_returns`):**
        * **Sub-Pattern:** For `:=`. Scope: `keyword.operator.assignment.smalltalk`.
        * **Sub-Pattern:** For `_` (underscore as assignment). Scope: `keyword.operator.assignment.underscore.gst.smalltalk`.
    3.  **Return Operator (within `assignments_and_returns`):**
        * **Sub-Pattern:** For `^`. Scope: `keyword.control.return.smalltalk`.
    4.  **Block Structures Repository Item:**
        * **Action:** Create `block_structures` in `repository`. Add `include: '#block_structures'` to main `patterns`.
        * **Output:** `block_structures` repository item.
    5.  **Implement Block Pattern (within `block_structures`):**
        * **Pattern:** For `[` ... `]`. This is a complex pattern.
            * Delimiters: `punctuation.definition.block.begin.smalltalk` (`[`), `punctuation.definition.block.end.smalltalk` (`]`). Overall scope `meta.block.literal.smalltalk`.
            * Block Parameters: `:arg1 :arg2 |` or `:arg ||`. Scopes: `variable.parameter.block.smalltalk` for args, `punctuation.definition.variable.parameter.block.smalltalk` for `:`, `punctuation.separator.variable.block.smalltalk` for `|` or `||`.
            * Block Temporaries: `| temp1 temp2 |`. Scopes: `variable.other.local.smalltalk` for temps, `punctuation.definition.variable.local.begin.smalltalk`/`end.smalltalk` for `|`.
            * Body: The content within the block must recursively match statements/expressions (e.g., by `include: '$self'` or includes for specific statement types, message sends, etc.).
* **Iterative Testing (Step 8):**
    * **Test Files:** `docs/research/gst-syntax/test-cases/07_core_assignments_cascades.st`, `docs/research/gst-syntax/test-cases/08_core_blocks.st`, `09_core_method_definitions_statements.st` (for `^`).
    * **Action:** Test assignments, returns. Test blocks with no args/temps, with args only, with temps only, with both. Test nested blocks.
    * **Expected Behavior:** Assignments, returns, and complete block structures (delimiters, params, temps, body) are correctly scoped.
* **Key Validation Questions for Step 8:**
    * Are both `:=` and `_` correctly scoped as assignments?
    * Are block parameters and temporaries correctly identified and scoped, including their delimiters?
    * Does the block body correctly allow for nested expressions/statements (initial test with literals and simple identifiers)?

---

**Phase D: Message Sends & Method/Class Definitions (Complex Structures)**

**Step 9: Implement Message Sends (Unary, Binary, Keyword)**

* **Primary Goal:** Implement robust patterns for unary, binary, and keyword message sends. This is one of the most complex parts of Smalltalk syntax due to precedence and chaining.
* **Guidance for Engineer:** This requires careful consideration of regex precedence and structure. Refer to `01-...` (selector token types), `03-...` (Section 3.4 for issues with old grammar's message send handling), and `04-...` (Section 5.5). The `05-recommendations-...` plan suggests a `message_send_components` repository group.
* **Specific Actions & Outputs:**
    1.  **Message Send Components Repository Item:**
        * **Action:** Create `message_send_components` in `repository`. This will hold patterns for individual selector types. Add `include: '#message_send_components'` to the main `patterns`. This include might actually be part of more complex expression rules that combine receivers with selectors.
        * **Output:** `message_send_components` repository item.
    2.  **Unary Selectors (within `message_send_components`):**
        * **Pattern:** For valid unary identifiers when used as selectors (e.g., `anObject size`). Scope: `entity.name.function.unary.smalltalk`.
    3.  **Binary Selectors (within `message_send_components`):**
        * **Pattern:** For binary operator characters when used as selectors (e.g., `1 + 2`). Scopes: `keyword.operator.binary.smalltalk` (or more specific like `keyword.operator.arithmetic.smalltalk`), potentially also `entity.name.function.binary.smalltalk`. Consider how to apply these if a token should have both an operator and function role.
    4.  **Keyword Selectors (within `message_send_components`):**
        * **Pattern:** For identifiers ending in a colon when used as selectors (e.g., `anObject at:put:`). Scope for each part (e.g., `at:`, `put:`): `entity.name.function.keyword.smalltalk`.
    5.  **Combining Receivers with Selectors (in main `patterns` or higher-level expression rules):**
        * **Action:** This is the most complex part. Define patterns that correctly parse sequences like `receiver unarySelector binarySelector argument keywordSelector: arg1 anotherKeyword: arg2`.
        * This will likely involve defining `expression` patterns that build up from primaries (literals, identifiers, parenthesized expressions, blocks) followed by sequences of message sends.
        * Use `meta.message-send.smalltalk`, `meta.expression.binary.smalltalk` as overall scopes where helpful.
* **Iterative Testing (Step 9):**
    * **Test Files:** `docs/research/gst-syntax/test-cases/06_core_message_sends.st`. Add many more complex examples involving chained messages of different types and precedences.
    * **Action:** Test simple and chained messages. Verify selector scopes and argument scopes.
    * **Expected Behavior:** Unary, binary, and keyword messages are correctly identified. Arguments are not part of the selector scope. Precedence should be visually parsable.
* **Key Validation Questions for Step 9:**
    * Are unary, binary, and keyword selectors distinctly and correctly scoped?
    * How are message arguments distinguished from selectors?
    * Does the grammar handle chained messages and basic precedence (e.g., unary before binary)?

---

**Step 10: Implement GST-Specific Definition Structures (Class, Namespace, Method)**

* **Primary Goal:** Implement patterns for GNU Smalltalk's primary definition constructs: namespaces, class definitions/extensions, and scoped method definitions. These should have high precedence in the main `patterns` list.
* **Guidance for Engineer:** These are complex, multi-part patterns. Refer to `01-...`, `02-...` for structural information, `03-...` (Section 3.9 for these items), and `04-...` (Section 5.7).
* **Specific Actions & Outputs:**
    1.  **GST Definition Structures Repository Item:**
        * **Action:** Create `gst_definition_structures` in `repository`. Ensure `include: '#gst_definition_structures'` is high in the main `patterns` order (e.g., after comments, before general message sends or literals not part of definitions).
        * **Output:** `gst_definition_structures` repository item.
    2.  **Namespace Definitions (within `gst_definition_structures`):**
        * **Pattern:** For `Namespace current: 'MyNamespace' [ ... ]`.
        * **Scopes:** `meta.namespace.definition.gst.smalltalk` (overall), `support.class.gst.smalltalk` (for `Namespace`), `keyword.declaration.namespace.gst.smalltalk` (`current:`), `entity.name.namespace.gst.smalltalk` (name), `punctuation.definition.namespace.body.begin.gst.smalltalk`/`end.gst.smalltalk` for `[]`. Body includes other definitions.
    3.  **Class Definitions & Extensions (within `gst_definition_structures`):**
        * **Pattern (Subclass):** For `Superclass subclass: #Name instanceVariableNames: 'vars' ... [...]`.
        * **Scopes:** `meta.class.definition.gst.smalltalk` (overall). Superclass: `entity.other.inherited-class.smalltalk`. `subclass:`: `keyword.declaration.class.subclass.gst.smalltalk`. Class Name (`#Name` content): `entity.name.type.class.gst.smalltalk`. Other keywords (`instanceVariableNames:`): `keyword.declaration.class.gst.smalltalk`. Body delimiters (`[]`): `punctuation.definition.class.body.begin.gst.smalltalk`/`end.gst.smalltalk`.
        * **Instance Variables:** Inside class def `[...]`, pattern for `| ivar1 ivar2 |`. Scope `variable.other.instance.smalltalk`.
        * **Pattern (Extension):** For `SomeClass extend [ ... ]`.
        * **Scopes:** `meta.class.extension.gst.smalltalk` (overall). Class Name: `entity.name.type.class.gst.smalltalk`. `extend`: `keyword.declaration.class.extend.gst.smalltalk`. Body delimiters similar to subclass.
    4.  **Scoped Method Definitions (within `gst_definition_structures`):**
        * **Pattern:** For `Class >> selector [...]` and `Class class >> selector [...]`.
        * **Scopes:** `meta.method.definition.gst.smalltalk` (overall). Class Name: `entity.name.type.class.gst.smalltalk`. Scope op `>>`: `punctuation.definition.method.scoped.gst.smalltalk`. `class` keyword: `storage.modifier.class-side.smalltalk`. Selector being defined (unary, binary, keyword parts): `entity.name.function.definition.unary.gst.smalltalk`, etc. Parameters: `variable.parameter.method.smalltalk`. Body `[]`: `punctuation.definition.method.body.begin.gst.smalltalk`/`end.gst.smalltalk`. Body itself will include statements.
* **Iterative Testing (Step 10):**
    * **Test Files:** `docs/research/gst-syntax/test-cases/11_gst_specific_namespace_class_def.st`, `docs/research/gst-syntax/test-cases/12_gst_specific_extend_scoped_methods.st`.
    * **Action:** Test full namespace, class, and method definitions. Verify all parts are correctly scoped (class names, superclass names, selectors, parameters, instance variables, keywords like `subclass:`, `extend`, `class`).
    * **Expected Behavior:** These complex definition structures are fully and correctly highlighted.
* **Key Validation Questions for Step 10:**
    * Are all components of namespace, class, and method definitions correctly scoped as per `04-textmate-mapping-strategy.md`?
    * Are instance variables within class definition blocks correctly identified as `variable.other.instance.smalltalk` and not `variable.other.local.smalltalk`?
    * Is the `class` keyword in `Class class >> ...` correctly scoped?

---

**Step 11: Implement Other GST-Specific Constructs & Remaining Punctuation**

* **Primary Goal:** Implement patterns for remaining GST-specific constructs (`Eval`, attributes, file-out chunk separators) and any general punctuation not yet covered.
* **Guidance for Engineer:** Refer to `03-...` (Section 3.9) and `04-...` (Sections 5.8).
* **Specific Actions & Outputs:**
    1.  **GST Other Constructs Repository Item:**
        * **Action:** Create `gst_other_constructs` in `repository`. Add `include: '#gst_other_constructs'` to main `patterns` (placement might be important, often before very general rules).
        * **Output:** `gst_other_constructs` repository item.
    2.  **`Eval [...]` (within `gst_other_constructs`):**
        * **Pattern & Scopes:** `meta.eval.gst.smalltalk` (overall), `keyword.control.eval.gst.smalltalk` (`Eval`), body delimiters `punctuation.definition.eval.body.begin.gst.smalltalk`/`end.gst.smalltalk`. Body recursively includes statements.
    3.  **Attributes (`<...>`)(within `gst_other_constructs`):**
        * **Pattern & Scopes:** `meta.attribute.gst.smalltalk` (overall), delimiters `punctuation.definition.attribute.begin.gst.smalltalk`/`end.gst.smalltalk`, name/keyword `entity.name.tag.attribute.gst.smalltalk`, value scoped by its type.
    4.  **File-out Chunk Separators (`!`) (within `gst_other_constructs` or a dedicated item like `gst_file_out_chunk_separators`):**
        * **Pattern & Scope:** For `!` (often on its own line or end of line). Scope: `punctuation.separator.chunk.gst.smalltalk`.
* **Iterative Testing (Step 11):**
    * **Test Files:** `docs/research/gst-syntax/test-cases/10_gst_specific_chunks_eval.st`, `docs/research/gst-syntax/test-cases/13_gst_specific_attributes_array_constructor.st`.
    * **Action:** Verify `Eval`, attributes, and chunk separators.
    * **Expected Behavior:** These GST-specific constructs are correctly highlighted.
* **Key Validation Questions for Step 11:**
    * Are `Eval` blocks and their contents parsed correctly?
    * Are attributes, including their names and values, correctly scoped?

---

**Phase E: Refinement & Finalization**

**Step 12: Contextual Identifier Scoping and Pattern Order Refinement**

* **Primary Goal:** Review and refine the entire grammar, focusing on the order of patterns in the main `patterns` array and within complex repository items. Ensure that more specific patterns take precedence over general ones to correctly scope identifiers in different contexts (e.g., method parameters vs. local variables vs. instance variables vs. global/class names).
* **Guidance for Engineer:** This is a holistic review. Use knowledge from all research documents. The goal is to resolve ambiguities and ensure the most accurate possible highlighting given TextMate's limitations.
* **Specific Actions & Outputs:**
    1.  **Review Identifier Scopes:**
        * **Action:** Examine how identifiers are currently scoped in various contexts (method parameters in `Class >> selector [ :param | ...]`, block parameters `[:bArg|...]`, temporaries `[| temp | ...]`, instance variables `| iVar |` in class defs).
        * **Action:** Adjust patterns and their order so that these specific declarations correctly scope their identifiers, rather than them falling back to `variable.other.smalltalk` or `entity.name.type.class.smalltalk` incorrectly.
        * **Output:** Refined patterns for variable declarations and their usage.
    2.  **Review Main `patterns` Order:**
        * **Action:** Critically assess the order of `include` directives in the top-level `patterns:` array. Ensure that more specific, complex structures (like full method definitions) are matched before simpler components they might contain (like standalone keyword selectors or blocks).
        * **Output:** Optimized `patterns` order.
* **Iterative Testing (Step 12):**
    * **Test Files:** Use a wide variety of complex, nested code examples from the test suite.
    * **Action:** Focus on subtle scoping issues, especially for identifiers in different declaration and usage contexts.
    * **Expected Behavior:** Highlighting is contextually as accurate as possible within TextMate's capabilities.
* **Key Validation Questions for Step 12:**
    * Is an identifier `foo` correctly scoped as `variable.parameter.method.smalltalk` when it's a method parameter?
    * Is it `variable.other.instance.smalltalk` when declared as such in a class `[...]` body?
    * Is it `variable.other.local.smalltalk` if it's a block temporary?

---

**Step 13: Comprehensive Validation Against US-201 ACs and Test Suite**

* **Primary Goal:** Systematically validate the completed grammar against all Acceptance Criteria for US-201 and the full suite of test cases.
* **Guidance for Engineer:** This is the final quality gate for the grammar itself.
* **Specific Actions & Outputs:**
    1.  **AC Checklist Review:**
        * **Action:** Go through each AC for US-201 (AC1 to AC9) and verify that the grammar fulfills it. Document any minor deviations or necessary clarifications.
        * **Output:** Confirmation of AC fulfillment.
    2.  **Full Test Suite Execution:**
        * **Action:** Use all test case files from `docs/research/gst-syntax/test-cases/`. For each file, visually inspect the highlighting and use "Developer: Inspect Editor Tokens and Scopes" to confirm correct scopes for all significant elements.
        * **Output:** Highlighting results for all test cases.
    3.  **Edge Case Testing:**
        * **Action:** Specifically test edge cases identified in the research or encountered during development (e.g., unusual whitespace, deeply nested structures, combinations of GST-specific features).
        * **Output:** Confidence in robustness.
* **Key Validation Questions for Step 13:**
    * Does the grammar satisfy every AC of US-201?
    * Does the grammar correctly highlight all constructs in all provided `.st` test files?
    * Are there any remaining known issues or areas where highlighting is incorrect or ambiguous?

---

**Step 14: Final Grammar Review, Cleanup, and Documentation**

* **Primary Goal:** Perform a final review of the `gnu-smalltalk.YAML-tmLanguage` file for clarity, consistency, maintainability, and performance. Add internal comments to the grammar.
* **Guidance for Engineer:** Ensure the grammar is not just functional but also a well-engineered artifact.
* **Specific Actions & Outputs:**
    1.  **Code Readability & Maintainability:**
        * **Action:** Read through the entire YAML grammar. Ensure consistent naming for repository keys. Check for clarity of regexes.
        * **Action:** Add comments within the YAML file to explain complex regexes, the purpose of specific repository items, or tricky ordering decisions.
        * **Output:** Well-commented and maintainable `gnu-smalltalk.YAML-tmLanguage`.
    2.  **Performance Considerations:**
        * **Action:** Look for any obviously inefficient regex patterns (e.g., excessive backtracking, overly broad wildcards in critical, frequently matched rules). While deep performance optimization is hard without tooling, avoid common pitfalls.
        * **Output:** Reasonably performant grammar.
    3.  **Remove Obsolete Patterns/Comments:**
        * **Action:** Ensure no placeholder comments like `# TODO:` remain unless they genuinely mark future work outside US-201's scope.
        * **Output:** Cleaned-up grammar file.
    4.  **(Optional but Recommended) Document Key Design Decisions:**
        * **Action:** If any particularly complex decisions were made regarding pattern design or scope mapping that aren't obvious from the grammar itself, briefly note them in an internal developer document or a well-commented section of the research pertaining to US-201.
        * **Output:** Internal documentation if needed.
* **Key Validation Questions for Step 14:**
    * Is the YAML grammar file well-commented and easy to understand?
    * Are there any glaring performance concerns?
    * Is the file free of temporary or placeholder content?