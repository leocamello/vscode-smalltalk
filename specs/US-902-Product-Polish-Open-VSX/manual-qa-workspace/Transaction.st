"Transaction.st — a small value class logged by Account. Referenced from Account.st
 (`Transaction kind:amount:`), so it takes part in references / senders / class rename.
 Parses with 0 diagnostics."

Object subclass: Transaction [
    | kind amount |

    kind [ ^kind ]

    amount [ ^amount ]

    setKind: aSymbol amount: aNumber [
        kind := aSymbol.
        amount := aNumber
    ]

    printOn: aStream [
        aStream nextPutAll: kind; nextPut: $ ; print: amount
    ]
]

Transaction class extend [
    kind: aSymbol amount: aNumber [ ^self new setKind: aSymbol amount: aNumber ]
]
