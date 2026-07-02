"US-901 manual-QA — a normal, clean class. Every feature (completion, hover,
 rename, references, formatting, semantic tokens) must keep working while the
 pathological fixture is also open. This file parses with 0 diagnostics."
Object subclass: Account [
    | balance owner |
    Account class >> for: aName [ ^self new initFor: aName ]
    initFor: aName [ balance := 0. owner := aName. ^self ]
    deposit: anAmount [ balance := balance + anAmount. ^balance ]
    withdraw: anAmount [
        anAmount > balance ifTrue: [ ^self error: 'insufficient funds' ].
        balance := balance - anAmount.
        ^balance
    ]
    balance [ ^balance ]
    owner [ ^owner ]
]
