"Account.st — the base class of the Bank domain.

 The instance variables `balance` and `owner` are declared HERE and also used in
 Account-Reporting.st and Shadowing.st (separate `extend` files), so renaming an
 ivar (F2) must rewrite every file. The class `Account` is referenced across the
 whole workspace (SavingsAccount.st, Bank.st, Demo comments, the `extend` files),
 so renaming the class must rewrite every resolved reference — and never a local
 that merely shares the name, nor a kernel class, nor text in a comment/string.

 This file parses with 0 diagnostics — any red squiggle is a bug."

Object subclass: Account [
    | balance owner history |

    "Answer the current balance."
    balance [ ^balance ]

    owner [ ^owner ]

    history [ ^history ]

    initOwner: aName [
        owner := aName.
        balance := 0.
        history := OrderedCollection new
    ]

    "Add amount to the balance and log a Transaction. Answer the new balance."
    deposit: amount [
        balance := balance + amount.
        history add: (Transaction kind: #deposit amount: amount).
        ^balance
    ]

    withdraw: amount [
        amount > balance ifTrue: [ ^self error: 'insufficient funds' ].
        balance := balance - amount.
        history add: (Transaction kind: #withdraw amount: amount).
        ^balance
    ]

    printOn: aStream [
        aStream
            nextPutAll: 'Account(';
            nextPutAll: owner;
            nextPutAll: ': ';
            print: balance;
            nextPut: $)
    ]
]

Account class extend [
    "Create an account owned by aName with a zero balance."
    for: aName [ ^self new initOwner: aName ]
]
