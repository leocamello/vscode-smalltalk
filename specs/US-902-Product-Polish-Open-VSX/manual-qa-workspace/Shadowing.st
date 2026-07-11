"Shadowing.st — the scope-safety cases that rename, highlight, and semantic tokens
 must all respect. Parses with 0 diagnostics.

 - `scaleBy: balance` — the ARGUMENT `balance` shadows the Account instance variable.
   Renaming the ivar (from Account.st) must NOT touch this method; renaming this local
   (F2 on the argument) must NOT touch the ivar. Highlight-occurrences on this `balance`
   marks only the local uses.
 - `demo` — a TEMPORARY named `Account` shadows the class. Renaming the class `Account`
   must NOT rewrite this local, and semantic highlighting colors this `Account` as a
   variable, not a class."

Account extend [
    scaleBy: balance [
        | doubled |
        doubled := balance * 2.
        ^doubled
    ]

    demo [
        | Account |
        Account := 7.
        ^Account + 1
    ]
]
