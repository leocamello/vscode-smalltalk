"Shape-Draw.st — references `Shape` in a SECOND file, in several forms that must all
update on rename: an `extend` receiver, a `subclass:` superclass position, a
`class`-side extend receiver, and message-receiver sends (`Shape new`). `Circle` is
a NEW class introduced here (its own name is never renamed when renaming `Shape`)."

Shape extend [
    describe [ ^'a ', name printString ]
]

Shape subclass: Circle [
    | radius |

    area [ ^radius * radius * 3 ]
]

Shape class extend [
    default [ ^Shape new ]
]
