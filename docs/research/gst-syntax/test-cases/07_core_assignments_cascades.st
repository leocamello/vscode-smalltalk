"
    File: 07_core_assignments_cascades.st
    Purpose: Test cases for assignments and cascades.
"

| var1 var2 var3 anObject |

" --- Assignments (ASSIGNMENT token: := or _) --- "
" Simple assignment with := "
var1 := 10.
var1 printNl.

" Simple assignment with _ (less common but valid GST syntax) "
var2 _ 20.
var2 printNl.

" Assignment with result of a message send "
var3 := 'hello' reversed.
var3 printNl.

" Chained assignments (evaluate right to left, value of assignment is the assigned value) "
var1 := var2 := var3 := 100.
var1 printNl.
var2 printNl.
var3 printNl.

var1 _ var2 _ var3 _ 200.
var1 printNl.
var2 printNl.
var3 printNl.

" Mixed assignment operators in chain (behavior might be parser dependent, := is preferred) "
" var1 := var2 _ 300. " " This might be tricky or non-standard "


" --- Cascades (;) --- "
anObject := OrderedCollection new.

" Simple cascade "
anObject
    add: 1;
    add: 2;
    add: 3.
anObject printNl.

" Cascade with different message types "
anObject
    addLast: 4;      " Keyword "
    reverse;         " Unary "
    addAll: #(5 6);  " Keyword "
    yourself.       " Unary, often used at the end of cascades to return the receiver "
anObject printNl.

" Cascade on a literal (less common but possible if the literal's class supports the messages) "
" (Array new)
    add: 'a';
    add: 'b';
    yourself printNl. " " This is more typical: cascade on (Array new) "

" Multiple cascades on the same line (less readable but valid) "
anObject removeFirst; removeLast.
anObject printNl.

" Cascade after a more complex expression "
(Dictionary newFromPairs: { 'key1' -> 'val1'. 'key2' -> 'val2' })
    at: 'key3' put: 'val3';
    removeKey: 'key1';
    yourself printNl.

" Empty cascade (syntactically odd, but check parser tolerance) "
" anObject ; . " " Likely a syntax error "


" End of Assignments and Cascades Test "
!
