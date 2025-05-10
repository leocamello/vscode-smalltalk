"
    File: 12_gst_specific_extend_scoped_methods.st
    Purpose: Test cases for GNU Smalltalk specific syntax:
             - object extend [...]
             - Scoped method definitions (Class >> selector [...] and Class class >> selector [...])
"

" --- object extend [...] --- "
| myInstance myExtendedInstance |
myInstance := Object new.

myInstance extend [
    " Adding methods to a specific instance "
    | extensionVar | " Instance variable for this extension only "

    initializeExtension [
        extensionVar := 'Extended!'.
        Transcript show: 'Instance extended and initialized.'; cr.
    ]

    greetFromExtension [
        ^'Hello from extended instance: ', extensionVar.
    ]
].

" Initialize and use the extended methods "
myInstance initializeExtension.
(myInstance greetFromExtension) printNl.

" Another instance of Object won't have these methods "
myExtendedInstance := myInstance. " Keep a reference "
myInstance := Object new.
" myInstance greetFromExtension. " " This would cause a doesNotUnderstand error "


" --- Scoped Method Definitions (Class >> selector [...] and Class class >> selector [...]) --- "
" These are typically used within a Class subclass: ... [...] block,
  but can also be used outside to add methods to existing classes. "

" Pre-requisite: Define a class to add methods to, if not using an existing one. "
Object subclass: #MyScopedMethodDemo [
    | data |
    MyScopedMethodDemo >> initializeWith: aValue [ data := aValue ]
].

" Adding an instance method using OwningClass >> selector [...] "
MyScopedMethodDemo >> printData [
    " This method is added to MyScopedMethodDemo "
    Transcript show: 'Data: '; print: data; cr.
].

" Adding a class method using OwningClass class >> selector [...] "
MyScopedMethodDemo class >> classSideUtility [
    " This method is added to the class side of MyScopedMethodDemo "
    ^'Utility method on class side'.
].

" Test the added methods "
| demoObj |
demoObj := MyScopedMethodDemo new.
demoObj initializeWith: 'Test Data'.
demoObj printData.
(MyScopedMethodDemo classSideUtility) printNl.


" Scoped method definition with temporaries "
MyScopedMethodDemo >> processData [
    | temp |
    temp := data reversed. " Assuming data is a string "
    ^'Processed: ', temp.
].
(demoObj processData) printNl.


" Scoped method definition with return statement "
MyScopedMethodDemo >> getData [
    ^data.
].
(demoObj getData) printNl.


" Scoped binary method "
MyScopedMethodDemo >> + anotherDemoObject [
    " Concatenate data from two MyScopedMethodDemo objects "
    ^self getData, anotherDemoObject getData.
].

| demoObj2 result |
demoObj2 := MyScopedMethodDemo new.
demoObj2 initializeWith: 'More Data'.
result := demoObj + demoObj2.
result printNl.


" Scoped keyword method "
MyScopedMethodDemo >> update: newData ifOld: oldDataBlock [
    data = newData ifTrue: [ ^oldDataBlock value ].
    data := newData.
    ^'Data updated'.
].

(demoObj update: 'New Data' ifOld: [ 'Data was already new!' ]) printNl.
(demoObj update: 'New Data' ifOld: [ 'Data was already new!' ]) printNl. " Call again "


" End of object extend and Scoped Method Definition Test "
!
