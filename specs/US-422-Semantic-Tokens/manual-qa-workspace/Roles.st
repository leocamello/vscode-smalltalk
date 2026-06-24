"US-422 manual QA fixture. Valid GST — parses with 0 diagnostics, so any red
 squiggle here is a bug. See README.md for the token-by-token color script.
 Verify with: Developer: Inspect Editor Tokens and Scopes (puts the cursor on a
 token and shows its `semantic token type` + modifiers, below the TextMate scopes)."

Object subclass: Vault [
    | balance entries |

    <comment: 'A tiny vault for the US-422 semantic-token tests.'>

    Vault class [
        | DefaultLimit |

        "ROW classvar: DefaultLimit -> property + `static` modifier."
        limit [ ^DefaultLimit ]
        setLimit: n [ DefaultLimit := n ]
    ]

    "ROW ivar: balance / entries -> property.
     ROW class (cartridge): OrderedCollection -> class + `defaultLibrary`."
    init [
        balance := 0.
        entries := OrderedCollection new.
        ^self
    ]

    "ROW param: amount -> parameter.   ROW temp: receipt -> variable.
     ROW selector: deposit: / add: / + -> method (keyword / unary / binary)."
    deposit: amount [
        | receipt |
        receipt := amount * 2.
        entries add: receipt.
        balance := balance + amount.
        ^receipt
    ]

    "ROW block param: each -> parameter.
     ROW global (known, not a class): Transcript -> variable, NOT class.
     ROW selector: do: / showCr: / printString -> method."
    report [
        entries do: [ :each | Transcript showCr: each printString ].
        ^self
    ]

    "ROW pseudo-variables: self / super / nil / true / false / thisContext -> keyword."
    printOn: aStream [
        super printOn: aStream.
        balance isNil ifTrue: [ ^nil ].
        (true | false) ifTrue: [ thisContext ].
        ^self
    ]
]

"Use site.
 ROW class (workspace): Vault -> class, NO defaultLibrary modifier.
 ROW global / AC4: Zork is unknown. With a cartridge loaded (auto/bundled) it is a
 `variable` (unknown global); with `off` (no cartridge) it falls back to `class`."
Vault new deposit: 16rFF.
Zork new frobnicate.
