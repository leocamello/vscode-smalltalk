"
    File: 02_literals_strings_chars_symbols.st
    Purpose: Test cases for string, character, and symbol literals.
"

" --- String Literals (STRING_LITERAL) --- "
'Hello, world!'.
'This is a simple string.'.
'A string with an embedded ''single quote''.'.
'Another string with two '''' consecutively.'.
''.  " Empty string "

" Multi-line strings are typically done by concatenation in Smalltalk,
  but the lexer should handle a string token correctly regardless of content.
  Actual parsing of newlines within a raw string literal is not standard. "
'This is technically one line in Smalltalk'.

" String literal with actual newline characters (parser treats as one token) "
'multi-
line
string'.

" --- Character Literals (CHAR_LITERAL) --- "
$a.
$Z.
$0.
$$.          " Dollar sign character "
$_underscore. " Underscore character "
$<.          " Less than character "
$>.          " Greater than character "
$-.          " Hyphen character "
$+.          " Plus character "

" Special character forms (e.g., Unicode code points) "
$<65>.       " Character for ASCII 65 (A) "
$<163>.      " Character for £ (pound sign) "
$<9731>.     " Character for ☃ (snowman) "

" Hexadecimal character code points "
$<16r42>. " Character code point hex (B) "
$<16rFF>. " Character code point hex (ÿ) "


" --- Symbol Literals (SYMBOL_LITERAL) --- "
" Identifier-like symbols "
#aSymbol.
#anotherSymbol.
#symbolWith_Underscore.
#symbolWith123Numbers.
#yourself.

" Keyword-like symbols (ending with a colon) "
#aKeyword:.
#ifTrue:.
#value:value:.

" Multi-keyword symbol "
#keyword:anotherKeyword:.

" Quoted string symbols (allows spaces and special characters) "
#'a string symbol'.
#'symbol with spaces and ''quotes'''.
#'another-one!@#$%^&*()'.
#''.          " Empty string symbol "

" Binary operator symbols "
#+.
#-.
#*.
#/.
#\\.
#~=.
#==.
#<.
#<=.
#>.
#>=.
#@.
#,.
#&.
#|.
#--. " multi-char binary op symbol "

" Symbols that might look like numbers but are symbols "
#123.
#1.0.
#1s.

" --- Array Literals (from parser tests) --- "
" Note: Array literals are also covered in 03_literals_arrays_bytearrays.st,
  but these examples come from a parser test file associated with general literals. "
#(1 2 3).
#('string' $c #symbol 1.23).
#( ). "Empty array"
#(1 #(2 3) 4). "Nested array"
#(1 'two' #(nestedArray 10) #symbol). "Array with complex/mixed literals"


" --- Byte Array Literals (from parser tests) --- "
" Note: Byte Array literals are also covered in 03_literals_arrays_bytearrays.st. "
#[1 255 0].
#[]. "Empty byte array"


" --- Scaled Decimal Literals (GNU Smalltalk specific) --- "
1.2s2.  " 1.2 * 10^2 = 120 "
123.456s. " equivalent to 123.456s0 "
-3.14s1.


" End of String, Character, and Symbol Literals Test "
!
