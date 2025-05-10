"
    File: 11_gst_specific_namespace_class_def.st
    Purpose: Test cases for GNU Smalltalk specific syntax:
             - Namespace current: ... [...]
             - Class subclass: ... [...] (scoped definition)
"

" --- Namespace current: ... [...] --- "
Namespace current: #TestSpace1 [
    " Code within TestSpace1 namespace "
    Transcript show: 'Inside TestSpace1.'; cr.

    Object subclass: #MyClassInTestSpace1 [
        " Class definition within the namespace "
        | instVarInNamespace |

        MyClassInTestSpace1 class >> new [
            ^super new initializeInNamespace
        ]

        initializeInNamespace [
            instVarInNamespace := 'Initialized in TestSpace1'.
            Transcript show: 'MyClassInTestSpace1 instance created.'; cr.
        ]

        printVar [
            instVarInNamespace printNl.
        ]
    ]
].

" Accessing class from namespace (assuming TestSpace1 is now known) "
" | obj |
obj := TestSpace1::MyClassInTestSpace1 new.
obj printVar. "

" --- Class subclass: ... [...] (Scoped Definition) --- "

" Simple class definition "
Object subclass: #MySimpleClass [
    " No instance variables, class variables, or methods defined here yet "
    " This block can be empty "
    Transcript show: 'MySimpleClass definition processed.'; cr.
].

" Class definition with instance variables "
Object subclass: #MyClassWithIvars [
    | instVar1 instVar2 | " Instance variable declarations "

    " Methods can be defined here using OwningClass >> selector [...] "
    MyClassWithIvars >> initialize [
        instVar1 := 10.
        instVar2 := 'hello'.
    ]

    MyClassWithIvars >> printIvars [
        Transcript show: 'instVar1: '; print: instVar1; cr.
        Transcript show: 'instVar2: '; print: instVar2; cr.
    ]

    MyClassWithIvars class >> greeting [
        ^'Hello from MyClassWithIvars class side!'.
    ]
].

" Create instance and test "
" | myObj |
myObj := MyClassWithIvars new.
myObj initialize.
myObj printIvars.
MyClassWithIvars greeting printNl. "


" Class definition with class variable assignments "
" Note: True class variable *declarations* are part of the older
  subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
  syntax. In the scoped `[...]` syntax, you typically assign to them.
  They must exist or be created by assignment.
"
Object subclass: #MyClassWithClassVars [
    MyClassWithClassVars class [
        " This nested 'class' scope is for class-side definitions "
        ClassVar1 := 100. " Assigning to a class variable "
        AnotherClassVar := 'shared'.
    ]

    MyClassWithClassVars class >> getClassVar1Value [
        ^ClassVar1.
    ]

    MyClassWithClassVars >> printShared [
        AnotherClassVar printNl.
    ]
].
" (MyClassWithClassVars new) printShared. "
" MyClassWithClassVars getClassVar1Value printNl. "


" Class definition inheriting from a namespaced superclass "
Namespace current: #ParentNamespace [
    Object subclass: #BaseClass [
        BaseClass >> baseMethod [ ^'Base method result' ]
    ]
].

" Assuming ParentNamespace is accessible "
ParentNamespace::BaseClass subclass: #MyDerivedClass [
    " MyDerivedClass inherits from ParentNamespace::BaseClass "
    MyDerivedClass >> derivedMethod [
        Transcript show: 'Derived method calls: '; print: super baseMethod; cr.
        ^'Derived method result'.
    ]
].
" | derivedObj |
derivedObj := MyDerivedClass new.
derivedObj derivedMethod printNl. "


" Nested class-side definitions "
Object subclass: #OuterClass [
    OuterClass class [
        " Class side of OuterClass "
        OuterClass class >> outerClassMethod [ ^'Outer class method' ]

        " Defining an inner class on the class side of OuterClass (less common) "
        Object subclass: #InnerClassOnClassSide [
            InnerClassOnClassSide >> innerInstanceMethod [ ^'Inner instance on class side' ]
        ]
    ]

    OuterClass >> outerInstanceMethod [ ^'Outer instance method' ]
].
" OuterClass outerClassMethod printNl. "
" (OuterClass::InnerClassOnClassSide new) innerInstanceMethod printNl. " " If accessible "


" Class definition with attributes "
Object subclass: #MyClassWithAttributes [
    <gst.classCategory: 'Test-Attributes'> " Attribute for the class "

    | ivar |

    <gst.methodCategory: 'accessing'>
    MyClassWithAttributes >> getIvar [
        <gst.methodComment: 'Returns the value of ivar'>
        ^ivar
    ]

    MyClassWithAttributes >> setIvar: newValue [
        <gst.methodCategory: 'mutating'>
        ivar := newValue.
    ]
].

" End of Namespace and Scoped Class Definition Test "
!

" --- Additional GST Specific Definitions (from parser tests) --- "

" --- Namespace new name: ... ; yourself --- "
Namespace new name: #MyFixtureNamespace; import: Smalltalk; yourself.

" --- Class definition within a namespace (traditional) --- "
MyFixtureNamespace.MyTraditionalClass subclass: #Object
	instanceVariableNames: 'ivarA ivarB'
	classVariableNames: 'ClassVarA ClassVarB'
	poolDictionaries: ''
	category: 'MyCategory-MyFixtureNamespace'.

MyFixtureNamespace.MyTraditionalClass compile: 'myMethod ^ivarA'.
" (MyFixtureNamespace.MyTraditionalClass new) myMethod. "

" --- Class definition in the default namespace (Smalltalk) --- "
Object subclass: #MyParserSimpleClass
	instanceVariableNames: 'instVarOne instVarTwo'
	classVariableNames: 'ClassVarOne ClassVarTwo'
	poolDictionaries: ''
	category: 'MyCategory-ParserExamples'.

MyParserSimpleClass class compile: 'initialize ClassVarOne := 10. ClassVarTwo := ''hello''.'.
" MyParserSimpleClass initialize. "

" --- Instance variable declarations with | | (GST specific clarity) --- "
Object subclass: #MyParserDetailedClass
	instanceVariableNames: '| explicitIvar1 explicitIvar2 | anotherIvar' " GST allows | | for clarity "
	classVariableNames: 'MyClassVar'
	poolDictionaries: ''
	category: 'MyCategory-ParserDetailed'.

MyParserDetailedClass class compile: 'initializeClassVars MyClassVar := nil.'.
" MyParserDetailedClass initializeClassVars. "


" --- Nested Receiver [...] style definition (Namespace and Class) --- "
Smalltalk.Namespace definition: [
    name: #OuterParserSpace.
    import: Smalltalk.

    OuterParserSpace.Object definition: [
        name: #InnerParserClass.
        superclass: Smalltalk.Object.
        instanceVariables: 'innerIvar'.
        classVariables: 'InnerParserClassVar'.
        poolDictionaries: ''.
        category: 'OuterParserSpace-InnerClasses'.

        InnerParserClass compile: 'getInnerIvar ^innerIvar'.
        InnerParserClass class compile: 'initializeInner InnerParserClassVar := 100.'.
    ].
].

" Test accessing the nested class (requires OuterParserSpace to be processed) "
" | objInParsedNamespace |
objInParsedNamespace := OuterParserSpace.InnerParserClass new.
objInParsedNamespace getInnerIvar.
OuterParserSpace.InnerParserClass initializeInner. "

"EOF"
