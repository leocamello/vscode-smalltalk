"Navigation.st — manual-QA fixture for code navigation (US-412) and navigation
 polish (US-417). Three classes (one a subclass), instance variables, a class-side
 constructor, blocks, and a selector (#describe) implemented by TWO classes and
 sent from a third — so outline, workspace symbols, go-to-definition (plural),
 folding, and scope-aware highlight all have something to bite on. Must parse with
 0 diagnostics."

Object subclass: Shape [
    | name |

    Shape class >> named: aString [ ^self new setName: aString ]

    setName: aString [ name := aString. ^self ]

    name [ ^name ]

    describe [ ^'a shape named ', name ]

    area [ ^self subclassResponsibility ]
]

Shape subclass: Circle [
    | radius |

    radius: aNumber [ radius := aNumber. ^self ]

    describe [ ^'a circle named ', name, ' radius ', radius printString ]

    area [ ^radius * radius * 355 / 113 ]
]

Object subclass: Gallery [
    | shapes |

    init [ shapes := OrderedCollection new. ^self ]

    add: aShape [ shapes add: aShape ]

    describeAll [
        shapes do: [:each | Transcript showCr: each describe ]
    ]
]

Gallery new init
    add: (Circle new radius: 5);
    add: (Shape named: 'blob');
    describeAll
