"
    File: 14_gst_specific_binding_compile_time_constants.st
    Purpose: Test cases for GNU Smalltalk specific syntax:
             - Binding Constants #{...}
             - Compile-Time Constants ##(...)
"

" --- Binding Constants #{...} --- "
" Binding to a global variable/class "
| bindingToTranscript bindingToArray |
bindingToTranscript := #{Transcript}.
bindingToArray := #{Array}.

bindingToTranscript value show: 'Accessed via binding: '; show: bindingToArray value name; cr.

" Binding to a namespaced class "
Namespace current: #TestBindings [
    Object subclass: #MyBoundClass []
].

| bindingToNamespacedClass |
bindingToNamespacedClass := #{TestBindings::MyBoundClass}.
bindingToNamespacedClass value new printNl. " Should print 'a MyBoundClass'"


" --- Compile-Time Constants ##(...) --- "
" Simple compile-time constant "
| compileTimeNow compileTimeCalculation |
compileTimeNow := ##( Time now ).
'Compile time was: ' print. compileTimeNow printNl.
" Note: The value is fixed when the code is compiled. "

" Compile-time constant with temporaries and statements "
compileTimeCalculation := ##(
    | a b product |
    a := 5.
    b := 10.
    product := a * b.
    product + Date today dayOfYear. " Add something dynamic at compile time "
).
'Compile-time calculation result: ' print. compileTimeCalculation printNl.

" Compile-time constant with no statements (result is nil) "
| emptyCompileTime |
emptyCompileTime := ##().
emptyCompileTime isNil ifTrue: [ 'Empty compile-time constant is nil.' printNl ].

" Compile-time constant with only temporaries (result is nil) "
| tempsOnlyCompileTime |
tempsOnlyCompileTime := ##( | x y | ).
tempsOnlyCompileTime isNil ifTrue: [ 'Temps-only compile-time constant is nil.' printNl ].


" Using compile-time constants in expressions "
| message |
message := 'Program started at: ', ##( Date today printFormat: #(1 2 3 $  $- 1900 1 1 $m $- $d $- $Y ) ).
message printNl.


" End of Binding and Compile-Time Constants Test "
!
