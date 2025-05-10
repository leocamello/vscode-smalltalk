"
    File: 05_core_identifiers_pseudo_vars.st
    Purpose: Test cases for identifiers and pseudo-variables.
"

" --- Identifiers --- "
" Used as variable names "
| anIdentifier anotherIdentifier variable123 with_underscore |
anIdentifier := 1.
anotherIdentifier := 'hello'.
variable123 := anIdentifier + 2.
with_underscore := Object new.

" Used as class names (conventionally starting with uppercase) "
Array.
String.
Object.
UndefinedObject.
MyCustomClass. " Assuming MyCustomClass is defined "

" Used as unary selectors "
1 class.
'hello' size.
Object new yourself.


" --- Pseudo-variables --- "
" true "
true ifTrue: [ 'Is true' printNl ].

" false "
false ifFalse: [ 'Is false' printNl ].

" nil "
nil isNil ifTrue: [ 'Is nil' printNl ].
(anIdentifier isNil) ifFalse: [ anIdentifier := nil ].

" self "
Object subclass: #MyExampleClassForSelf
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Test'.

MyExampleClassForSelf methodsFor: 'testing'
stamp: 'tester'.
!
getValue
    ^self. " self refers to the instance of MyExampleClassForSelf "
!
printSelf
    Transcript show: self printString; cr.
!

" super "
MyExampleClassForSelf subclass: #MySubclassForSuper
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Test'.

MySubclassForSuper methodsFor: 'testing'
stamp: 'tester'.
!
printSuperclass
    " super refers to MyExampleClassForSelf in this context "
    Transcript show: super class name; cr.
!
getValue
    ^super getValue. " calls getValue in MyExampleClassForSelf "
!

" thisContext (less common in typical code, more for debugging/reflection) "
[ | currentContext |
  currentContext := thisContext.
  Transcript show: 'Current context: '; show: currentContext printString; cr.
] value.


" End of Identifiers and Pseudo-variables Test "
!
