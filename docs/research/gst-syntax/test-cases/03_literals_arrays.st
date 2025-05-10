"
    File: 03_literals_arrays.st
    Purpose: Test cases for array and byte array literals.
"

" --- Array Literals --- "
" Empty array "
#().

" Simple array with numbers "
#(1 2 3 4 5).

" Array with mixed literal types "
#(1 'two' $3 #fourSymbol #'five string' 6.0 7s1).

" Array containing nil, true, false "
#(nil true false).

" Nested arrays "
#(1 #(2 3) 4 #(5 #(6) 7)).

" Array with symbols that are operators "
#(#+ #- #*).

" Array with character literals "
#($a $b $c).

" Array with only one element "
#(1).
#('lonely string').

" Array with extra spaces "
#(  1   'spaced'   $s   #symbol   ).


" --- Byte Array Literals --- "
" Empty byte array "
#[].

" Simple byte array "
#[0 10 20 255].

" Byte array with various small integers "
#[1 2 3 127 128 200].

" Byte array with only one element "
#[42].

" Byte array with extra spaces "
#[  10   20   30  ].

" Byte array elements are INTEGER_LITERAL, not BYTE_LITERAL from lexer perspective "
" Values must be 0-255 "
" #[256]. " " This would be a parse error or runtime error "
" #[-1]. "  " This would be a parse error or runtime error "


" End of Array and Byte Array Literals Test "
!
