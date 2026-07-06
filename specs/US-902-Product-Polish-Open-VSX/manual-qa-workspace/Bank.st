"Bank.st — aggregates accounts. The richest file for language intelligence:

 - keyword messages (`at:put:`, `detect:ifNone:`, `inject:into:`) -> completion + signature help
 - kernel classes (`OrderedCollection`, `Dictionary`) -> semantic tokens (known class) + hover
 - cross-file sends (`Account for:`, `SavingsAccount for:rate:`, `each balance`, `each owner`)
   -> references / senders / implementors / call hierarchy
 - a cascade on `Transcript` -> formatting cascade-align
 - namespaced/binding references to `Account` (`#{Account}`, `Smalltalk.Account`)
   -> class-rename resolution-gating (the class segment only is rewritten)

 Parses with 0 diagnostics."

Object subclass: Bank [
    | name accounts |

    name [ ^name ]

    accounts [ ^accounts ]

    initName: aString [
        name := aString.
        accounts := OrderedCollection new
    ]

    open: anOwner [
        | acct |
        acct := Account for: anOwner.
        accounts add: acct.
        ^acct
    ]

    openSavings: anOwner rate: aRate [
        | acct |
        acct := SavingsAccount for: anOwner rate: aRate.
        accounts add: acct.
        ^acct
    ]

    totalBalance [ ^accounts inject: 0 into: [:sum :each | sum + each balance] ]

    accountFor: anOwner [ ^accounts detect: [:each | each owner = anOwner] ifNone: [ nil ] ]

    index [
        | byOwner |
        byOwner := Dictionary new.
        accounts do: [:each | byOwner at: each owner put: each].
        ^byOwner
    ]

    report [
        Transcript
            showCr: name;
            showCr: '----------------'.
        accounts do: [:each | Transcript showCr: each printString].
        Transcript showCr: 'Total: ', self totalBalance printString
    ]

    "Namespaced / binding references to Account (class-rename resolution-gating)."
    accountClass [ ^#{Account} ]

    defaultAccount [ ^Smalltalk.Account for: 'default' ]
]

Bank class extend [
    named: aString [ ^self new initName: aString ]
]
