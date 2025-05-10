#! /usr/bin/env gst -f
"
    File: 15_edge_cases_shebang_whitespace.st
    Purpose: Test cases for GNU Smalltalk edge cases:
             - Shebang line
             - Whitespace variations
"

" --- Shebang Line --- "
" The line `#! /usr/bin/env gst -f` at the very top of this file
  is a shebang. It should be recognized if at the absolute beginning. "
Transcript show: 'Shebang test: If this runs, gst processed the file.'; cr.


" --- Whitespace Variations --- "

" Minimal spacing "
|var1|var1:=1+2*3.var1 printNl.Object new inspect.

" Generous spacing and newlines (where syntactically valid) "
| var2
var3 |

var2 :=
    100
    +
    200.

var3
    :=
    var2
    yourself
    .

Transcript
    show:
        'Var2: '
    ;
    print: var2
    ;
    show: ', Var3: '
    ;
    print: var3
    ;
    cr.


" Blocks with varied spacing "
[Transcript show:'Block with minimal space'.1]value.

[
    Transcript show: 'Block with generous space.'
    .
    2
] value.


[ :arg1
    | | " no temps "
    arg1 printNl
] value: 'Argument with newlines'.


[
    :argA
    :argB
    | | | tempX tempY |
    tempX := argA * argA.
    tempY := argB + argB.
    tempX + tempY
]
    value: 5
    value: 10.


" Array literals with varied spacing "
#(1 2 3).
#( 1 2   3 ).
#(
    1
    'two'
    $3
    #four
).

" Byte array literals with varied spacing "
#[1 2 3].
#[ 1  2 3 ].
#[
    10
    20
    30
].

" Array constructor with varied spacing "
{1. 2. 3}.
{ 1 . 2 . 3 }.
{
    1 + 1 .
    'hello' , ' world'
}.


" Method definition style (scoped) with varied spacing "
Object subclass: #WhitespaceDemo [
    WhitespaceDemo >> demoMethod : arg1 with : arg2 [
        | temp |
        temp := arg1 +
            arg2.
        ^ temp
    ]

    WhitespaceDemo class
        >>
        classDemoMethod
            [
                ^'Class side demo'
            ]
].


" End of Shebang and Whitespace Variations Test "
!
