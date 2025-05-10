"
    File: 09_core_method_definitions_statements.st
    Purpose: Test cases for basic method definitions (file-out style) and statement sequences.
"

" --- Statement Sequences --- "
" Multiple statements separated by . "
Transcript show: 'Statement 1'.
Transcript show: 'Statement 2'.
1 + 2 printNl.
'Done with simple statements.' printNl.

" A statement can be a simple literal (though usually not useful alone) "
123.
'a string'.
#aSymbol.

" A statement can be a message send "
Object new. " Creates an object, result discarded "

" A statement can be an assignment "
| x |
x := 10.
x := x + 5.
x printNl.

" Optional final period "
Transcript show: 'Last statement, no period after this if it's the end of a chunk/REPL line'


" --- Basic Method Definitions (Traditional File-Out Style) --- "
" These are typically within a class context, e.g., after 'ClassName methodsFor: ...' "
" For testing syntax, we'll just show the method part. "

" Assume we are in a class context for these examples.
  The following would typically be preceded by:
  Object subclass: #MyMethodTester
    instanceVariableNames: 'ivar1'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Test-Methods'.

  MyMethodTester methodsFor: 'unary messages' stamp: 'user 1/1/2023'.
"

" Unary method "
!unaryMessage
    " This is a unary method. "
    ^'Unary method result'.
!

" Unary method with temporary variables "
!unaryWithTemps
    | temp1 temp2 |
    temp1 := 10.
    temp2 := 20.
    ^temp1 + temp2.
!

" Binary method "
!+ anObject
    " This is a binary method. Argument is anObject. "
    " Assuming self is a number-like thing for this example. "
    ^self value + anObject value. " Fictional 'value' message "
!

" Binary method with temporary variables "
!* aFactor
    | result |
    result := self value * aFactor. " Fictional 'value' message "
    ^result.
!

" Keyword method "
!keywordMessage: arg1 with: arg2
    " This is a keyword method. "
    Transcript show: 'Arg1: '; print: arg1; cr.
    Transcript show: 'Arg2: '; print: arg2; cr.
    ^arg1, arg2. " Assuming args are concatenable "
!

" Keyword method with temporary variables "
!process: data ifError: errorBlock
    | processedData |
    data isNil ifTrue: [ ^errorBlock value ].
    processedData := data reversed. " Example processing "
    ^processedData.
!

" Method with only a return statement "
!getFixedValue
    ^42.
!

" Method with statements before return "
!calculateAndReturn
    | intermediate |
    Transcript show: 'Calculating...'; cr.
    intermediate := self complexCalculation. " Assuming complexCalculation is defined "
    ^intermediate * 2.
!
!complexCalculation
    ^10. " Placeholder "
!

" Method without an explicit return (implicitly returns self) "
!doSomething
    Transcript show: 'Doing something with self: '; print: self; cr.
    " No explicit ^, so self is returned. "
!

" Method with instance variable access (assuming 'ivar1' was declared) "
!getIvar1
    ^ivar1.
!
!setIvar1: aValue
    ivar1 := aValue.
!

" End of Basic Method Definitions and Statements Test "
" The '!' is a chunk separator in file-out format. "
!
