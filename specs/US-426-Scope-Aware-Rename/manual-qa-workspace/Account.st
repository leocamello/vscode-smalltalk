"Account.st — the primary class definition. The instance variable `balance` is
declared here and referenced in methods both here AND in Account-Report.st (a
separate `extend` file). Renaming `balance` (F2) must update BOTH files."

Object subclass: Account [
    | balance owner |

    init [ balance := 0 ]

    deposit: amount [ balance := balance + amount. ^balance ]

    balance [ ^balance ]
]
