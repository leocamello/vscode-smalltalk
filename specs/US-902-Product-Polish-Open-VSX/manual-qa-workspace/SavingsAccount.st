"SavingsAccount.st — a SUBCLASS of the workspace class Account (not a kernel class).

 Good for: hover on `SavingsAccount` showing the superclass chain
 (SavingsAccount -> Account -> Object); go-to-definition on the `Account` superclass
 jumping to Account.st; `super`/inherited sends (`self balance`, `self deposit:`)
 in references / call hierarchy; and class rename (renaming `Account` rewrites the
 superclass position here). Parses with 0 diagnostics."

Account subclass: SavingsAccount [
    | rate |

    rate [ ^rate ]

    rate: aNumber [ rate := aNumber ]

    "Interest = balance * rate, truncated; deposited via the inherited deposit:."
    addInterest [ ^self deposit: (self balance * rate) truncated ]

    printOn: aStream [
        super printOn: aStream.
        aStream nextPutAll: ' @'; print: rate
    ]
]

SavingsAccount class extend [
    for: aName rate: aNumber [ ^(self for: aName) rate: aNumber; yourself ]
]
