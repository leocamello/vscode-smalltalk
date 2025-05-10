"
    File: 06_core_message_sends.st
    Purpose: Test cases for unary, binary, and keyword message sends.
"

| var anArray aString aNumber |
var := 10.
anArray := #(1 2 3).
aString := 'smalltalk'.
aNumber := 5.

" --- Unary Messages --- "
var printNl.
anArray size printNl.
aString reversed printNl.
Object new class name printNl. " Chained unary messages "
(1 + 2) class printNl. " Unary message to result of binary operation "
[ 1 + 1 ] value printNl. " Unary message to a block "

" Factorial (unary) "
10 factorial printNl.


" --- Binary Messages --- "
" Arithmetic "
(var + 5) printNl.
(aNumber * 2) printNl.
(100 / 10) printNl.
(10 \\ 3) printNl. " Modulo "
(var - aNumber) printNl.

" Comparison (result in true/false) "
(var > aNumber) printNl.
(aNumber < 10) printNl.
(var = 10) printNl.
(aNumber ~= 5) printNl. " Not equal "
(var >= 10) printNl.
(aNumber <= 5) printNl.

" Logical (for Booleans, but can be defined for others) "
(true & false) printNl.
(true | false) printNl.

" Other binary operators "
anArray, #('add', 'this') printNl. " Concatenation for collections "
(1 -> 'one') printNl. " Association creation "

" Chained binary messages (evaluate left to right for same precedence) "
(1 + 2 + 3 + 4) printNl. " Result is 10 "
(10 - 2 * 3) printNl.   " (10 - 2) * 3 = 24. Binary ops are typically left-associative at same precedence level. "
                        " Smalltalk standard precedence: Unary > Binary > Keyword.
                          Within binary, all have same precedence and evaluate left-to-right.
                          Parentheses control order. "
(10 - (2 * 3)) printNl. " 10 - 6 = 4 "

" Assignment with binary operator "
| count |
count := 0.
count := count + 1.
count printNl.

" Multi-character binary selectors "
(var == 10) printNl. " Equality check (can be different from = for identity) "
(var ~~ 11) printNl. " Not identical or specific not-equal "
(aNumber <= 5) printNl.


" --- Keyword Messages --- "
" Single keyword "
anArray at: 1 put: 100.
anArray printNl.
Smalltalk at: #MyGlobal put: 'Hello Global'.
MyGlobal printNl.

" Single keyword with numeric argument "
Array new: 10. " Creates an Array of size 10 "

" Multi-keyword "
anArray copyFrom: 1 to: 2.
aString copyReplaceAll: 'talk' with: 'rocks'.
aString printNl.

Date newDay: 1 month: 'January' year: 2024.

" Keyword message with literal arguments "
1 to: 5 do: [ :i | i printNl ].

" Keyword message with block argument "
true ifTrue: [ 'It was true!' printNl ] ifFalse: [ 'It was false!' printNl ].

" Keyword message with result of other messages as arguments "
anArray at: (1 + 1) put: (aString size).
anArray printNl.

" Nested keyword messages "
(Dictionary new)
    at: 'name' put: 'GNU Smalltalk';
    at: 'version' put: (Smalltalk version);
    yourself.


" --- Message Sends as Arguments --- "
Transcript show: (Date today monthName).
" anotherObject process: (someObject calculateValue: 5). " " Placeholder for a runnable example "


" --- Chained Messages --- "
" Chained unary messages "
Date today dayOfWeek name printNl.

" Chained keyword and unary messages "
((Array new: 5) at: 1 put: 'test') size printNl.


" --- Complex Nested Message Sends --- "
" ((MyClass new) perform: #aMethod with: arg1) ifTrue: [ Transcript show: 'Done' ]. " " Placeholder "


" --- Messages to Literals --- "
" Message to a literal array "
(#(10 20 30) at: 1) printNl.

" Message to a literal string "
('hello' size) printNl.

" Message to a block result (also shown under unary) "
([ 2 * 3 ] value) printNl.


" --- Distinction: Unary vs. Keyword part --- "
" anObject size.             size is unary "
" anObject at: 1 put: value. at: and put: are keywords, value is an identifier argument "


" End of Message Sends Test "
!
