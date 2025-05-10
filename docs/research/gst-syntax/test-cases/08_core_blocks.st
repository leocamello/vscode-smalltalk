"
    File: 08_core_blocks.st
    Purpose: Test cases for block structures.
"

| aBlock result |

" --- Simple Blocks --- "
" Block with one statement "
aBlock := [ 1 + 1 ].
result := aBlock value.
result printNl.

" Block with multiple statements "
aBlock := [
    Transcript show: 'Inside block. '.
    2 * 3. " The last expression is the block's return value "
].
result := aBlock value.
result printNl.

" Empty block "
aBlock := [].
result := aBlock value. " Result is typically nil for an empty block "
result printNl.


" --- Blocks with Arguments --- "
" Block with one argument "
aBlock := [ :x | x * x ].
result := aBlock value: 5. " Pass 5 to x "
result printNl.

" Block with multiple arguments "
aBlock := [ :a :b | a + b ].
result := aBlock value: 10 value: 20. " Pass 10 to a, 20 to b "
result printNl.

" Block arguments without a following | (if no temps) "
aBlock := [ :name | 'Hello, ', name ].
(aBlock value: 'GNU Smalltalk') printNl.

" Block with only arguments (no temps, no | before body) "
aBlock := [ :val | val inspect ].
" aBlock value: 'inspect me'. " " Example call "


" --- Blocks with Temporary Variables --- "
" Block with one temporary variable "
aBlock := [ | temp |
    temp := 10.
    temp := temp + 5.
    temp * 2.
].
result := aBlock value.
result printNl.

" Block with multiple temporary variables "
aBlock := [ | temp1 temp2 sum |
    temp1 := 5.
    temp2 := 7.
    sum := temp1 + temp2.
    sum.
].
result := aBlock value.
result printNl.

" Empty temporaries declaration "
aBlock := [ | | 1 + 1 ]. " Valid, just declares no temps "
(aBlock value) printNl.


" --- Blocks with Arguments and Temporary Variables --- "
" Arguments first, then temporaries "
aBlock := [ :x :y | | tempProduct |
    tempProduct := x * y.
    tempProduct + 10.
].
result := aBlock value: 3 value: 4. " x=3, y=4 "
result printNl.

" Block arguments separated by | from temporaries "
" Note: GST parser `parse_block_variables` expects `( ':' IDENTIFIER )*` for args.
  The `|` separates args from body, or temps from body, or args from temps.
  If args are present, `|` separates them from the rest.
  If only temps are present, `| temps |` is used.
"
aBlock := [ :input | | processedInput |
    processedInput := input reversed.
    Transcript show: processedInput; cr.
    processedInput size.
].
(aBlock value: 'test') printNl.

" Block with arguments separated by || (binary OR operator token) from the body "
aBlock := [ :str1 :str2 || " This || separates args from the block body "
    str1, ' ', str2.
].
result := aBlock value: 'Hello' value: 'World'.
result printNl.


" --- Nested Blocks --- "
aBlock := [ | outerVar |
    outerVar := 100.
    [ | innerVar |
        innerVar := 20.
        outerVar + innerVar. " Inner block can access outer block's temps/args "
    ] value. " Execute inner block "
].
result := aBlock value.
result printNl.

" Block as an argument to another block's value message (if it accepts blocks) "
| blockTaker |
blockTaker := [ :actionBlock | actionBlock value + 1 ].
result := blockTaker value: [ 5 * 5 ].
result printNl.

" Nested block execution directly "
([ [ 10 + 10 ] value ] value) printNl.

" Nested block with arguments and locals from outer scope "
aBlock := [ :x | | y |
	y := [ :z | z * 2 ] value: x. " Inner block uses x from outer block's arg "
	y + 5
].
(aBlock value: 7) printNl.


" --- Blocks in Control Structures --- "
1 to: 3 do: [ :i |
    Transcript show: 'Loop iteration: '; print: i; cr.
].

true ifTrue: [ 'Condition is true.' printNl ].

false ifFalse: [ 'Condition is false.' printNl ]
      ifTrue: [ "This won't execute" ].

" Block in collection iteration (select:) "
| anArray selectedArray |
anArray := #(1 2 3 4 5 6).
selectedArray := anArray select: [ :each | each isEven ].
selectedArray printNl.

" Block in collection transformation (collect:) with nested conditional block execution "
| aCollection transformedCollection |
aCollection := #(1 'special' 2 'another' 3).
transformedCollection := aCollection collect: [ :item |
	item isString ifTrue: [ [ item reversed ] value ] ifFalse: [ item * item ]
].
transformedCollection printNl.


" --- Blocks with Explicit Return (^) --- "
aBlock := [ ^ 42. 'This is not reached' printNl ].
(aBlock value) printNl.

aBlock := [ :x | x > 0 ifTrue: [ ^ x ] ifFalse: [ ^ 0 ]. 'Unreachable' printNl ].
(aBlock value: 5) printNl.
(aBlock value: -5) printNl.


" End of Blocks Test "
!
