"Account-Report.st — extends Account in a SECOND file. `balance` here is the same
instance variable declared in Account.st; renaming it must rewrite this file too."

Account extend [
    report [ Transcript showCr: 'balance = ', balance printString ]
]
