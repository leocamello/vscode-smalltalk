"US-415 manual QA fixture. Valid GST — parses with 0 diagnostics, so any red
 squiggle here is a bug. See README.md for the row-by-row hover script.
 Hover = mouse over the token (or put the cursor on it and press the hover key)."

Object subclass: Bank [
    | accounts ledger |

    <comment: 'A tiny bank for the US-415 hover tests. Holds accounts and a running ledger.'>

    Bank class [
        | DefaultRate |

        "ROW 3d (class variable): hover  DefaultRate  below -> kind 'class variable'
         + its declaration site (the line of the | DefaultRate | declaration above)."
        setRate: r [
            "Set the shared default rate."
            DefaultRate := r
        ]
    ]

    "ROW 3a (instance variable): hover  accounts  -> kind 'instance variable' + decl line.
     ROW 2b (workspace class): hover  OrderedCollection  -> chain to Object (+ comment on
     the installed tier; none on the bundled reference)."
    init [
        "Set up an empty bank."
        accounts := OrderedCollection new.
        ledger := Dictionary new.
        ^self
    ]

    "ROW 3b (parameter): hover  amount  or  name  -> kind 'parameter' + decl line.
     ROW 3c (temporary): hover  entry  -> kind 'temporary' + decl line.
     ROW 1b (keyword selector): hover  at:put:  in  ledger at: name put: entry  ->
     signature  at: a1 put: a2  + implementors (Dictionary, …)."
    deposit: amount into: name [
        "Record a deposit of amount under name; answer the new ledger entry."
        | entry |
        entry := amount * 2.
        ledger at: name put: entry.
        ^entry
    ]

    "ROW 1c (workspace selector + comment): hover  rate  in the use site at the bottom ->
     signature + implementor Bank + THIS method comment (workspace prose, always shown)."
    rate [
        "Answer the default interest rate."
        ^DefaultRate
    ]
]

"Use site — ROW 1a (kernel selector): hover  printString  -> signature + implementor Object
 (+ Object's method comment on the installed tier; none on the bundled reference).
 ROW 2a (workspace class): hover  Bank  -> chain  Bank -> Object  + the <comment:> above.
 ROW 4 (literal): hover  16rFF  -> 16rFF = 255 (base 16)."
Bank new deposit: 16rFF into: 'alice'.
Bank new rate printString
