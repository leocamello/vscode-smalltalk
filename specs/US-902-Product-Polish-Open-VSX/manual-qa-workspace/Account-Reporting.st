"Account-Reporting.st — reopens Account in a SECOND file with `extend`. It uses the
 SAME `balance` and `owner` instance variables declared in Account.st, so renaming
 either ivar (F2) must rewrite BOTH files. Also proves `extend` parsing + that the
 outline lists these added methods under Account. Parses with 0 diagnostics."

Account extend [
    summary [ ^owner, ': ', balance printString ]

    isOverdrawn [ ^balance < 0 ]
]
