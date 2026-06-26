"Theatre.st — US-423 manual-QA fixture for References + Senders/Implementors.

   Two classes implement #greet (a workspace implementor union); several methods
   SEND #greet (senders); methods also send kernel selectors (#printString, #do:,
   #new) so the cartridge tier joins the union. A class-side method and a
   top-level send round out the coordinates. Must parse with 0 diagnostics."

Object subclass: Speaker [
    greet [ ^'hello' ]
    announce: aLine [ ^Transcript showCr: aLine printString ]
]

Object subclass: Robot [
    greet [ ^'beep boop' ]
]

Object subclass: Stage [
    | cast |

    Stage class >> new [ ^super new init ]

    init [ cast := OrderedCollection new. ^self ]

    add: aPerformer [ cast add: aPerformer ]

    "Sends #greet to a dynamic receiver (who) — a possible, not provable, responder."
    run: who [ ^who greet ]

    "Sends #greet inside a block, and #do: to the kernel collection."
    announceAll [ cast do: [:each | each greet ] ]
]

"Top-level sends with no enclosing method: senders of #new (receivers Stage,
 Speaker, Robot — all known classes), #add: and #announceAll."
Stage new add: Speaker new; add: Robot new; announceAll
