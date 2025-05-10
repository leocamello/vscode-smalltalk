"
    File: 10_gst_specific_chunks_eval.st
    Purpose: Test cases for GNU Smalltalk specific syntax:
             - File-out `!` chunk separators
             - Eval [...]
"

" --- File-out `!` Chunk Separators --- "
" Chunk 1: A simple doit "
Transcript show: 'This is the first chunk.'; cr.
1 + 1 printNl.
!
" Chunk 2: Another doit "
Transcript show: 'This is the second chunk.'; cr.
| x |
x := 10.
x * x printNl.
!
" Chunk 3: Could be method definitions (if context is set) or more doits "
" For this test, it's another doit "
'End of chunks for now.' printNl.
!

" --- Eval [...] --- "
" Simple Eval block "
Eval [
    Transcript show: 'Inside Eval block.'; cr.
    100 + 23.
].
" The result of the last expression in Eval is its value, but often Eval is for side effects. "

" Eval block with temporaries "
Eval [ | tempVar message |
    tempVar := Date today dayOfWeekName.
    message := 'Today is ', tempVar.
    Transcript show: message; cr.
    message. " This will be the 'result' of the Eval if captured "
].

" Eval block with no statements (result is nil) "
Eval [].

" Eval block with only temporaries (result is nil) "
Eval [ | a b | ].

" Eval block used in an expression (if its result is meaningful) "
| evalResult |
evalResult := Eval [ 3 * 4 ].
evalResult printNl. " Should print 12 "

" Nested Eval (less common, but syntactically possible) "
Eval [
    Transcript show: 'Outer Eval.'; cr.
    Eval [
        Transcript show: 'Inner Eval.'; cr.
        'Nested result'.
    ] printNl. " Print result of inner Eval "
].

" End of GST Specific Chunks and Eval Test "
!
