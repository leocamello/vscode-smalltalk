"
    File: 13_gst_specific_attributes_array_constructor.st
    Purpose: Test cases for GNU Smalltalk specific syntax:
             - Attributes (<...>)
             - Array Constructor ({...})
"

" --- Attributes (<...>) --- "
Object subclass: #MyClassWithAttrs [
    <gst.classCategory: 'Test-Attributes'> " Attribute for the class "

    | myVar |

    <gst.methodCategory: 'accessing'>
    MyClassWithAttrs >> getMyVar [
        <gst.methodComment: 'Returns the value of myVar'>
        ^myVar
    ]

    MyClassWithAttrs >> setMyVar: newValue [
        <gst.methodCategory: 'mutating'>
        myVar := newValue.
    ]
].

" Inspecting attributes (programmatically, not for syntax highlighting directly) "
" (MyClassWithAttrs methodNamed: #getMyVar) attributes printNl. "
" (MyClassWithAttrs attributes) printNl. "


" --- Array Constructor {...} --- "
| constructedArray anotherArray |

" Simple array constructor with literals "
constructedArray := { 1. 'two'. $c. #four }.
constructedArray printNl.

" Array constructor with expressions "
anotherArray := { 1 + 2. Date today. Smalltalk version. (Array new add: 5; yourself) }.
anotherArray printNl.

" Empty array constructor (results in an empty array) "
{}.

" Array constructor with a single element "
{ 'lonely' }.

" Array constructor with a block that is evaluated "
{ [ 3 factorial ] value. 'after block' }.

" Array constructor with nested structures "
{ 1. { 'nested'. true }. #( 'literal array' ) }.

" Array constructor with statements separated by periods "
constructedArray := {
    | temp |
    temp := 10.
    Transcript show: 'First element construction'.
    temp * temp. " First element "

    Transcript show: 'Second element construction'.
    'String element'. " Second element "

    (1 to: 3) collect: [ :i | i squared ]. " Third element (an array) "
}.
constructedArray printNl.


" End of Attributes and Array Constructor Test "
!