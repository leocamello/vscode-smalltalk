"US-430 manual QA fixture. See README.md for the row-by-row script.
 This file is valid GST and parses with 0 diagnostics — any red squiggle here is a bug."

Object subclass: Account [
    | balance owner history |

    init [
        balance := 0.
        history := OrderedCollection new.
        ^self
    ]

    "ROW 1 (selector context): on a blank line in this method type  amount print
     then trigger completion. Expect kernel selectors printString, printNl, printOn:.
     Select one and read the greyed detail: kernel (reference) on the floor,
     kernel (installed) when auto resolves to the installed kernel (ROW 6)."
    deposit: amount [
        history add: amount.
        balance := balance + amount.
        ^balance
    ]

    "ROW 3 (keyword snippet): type  history at  and accept  at:put:
     It must insert  at:${1} put:${2}  with two Tab stops."
    record: key value: aValue [
        ^history
    ]

    "ROW 2 (head context): on the blank line type  ba  -> expect the variable
     'balance' (Field icon) ranked ABOVE class names; then type  OC  -> expect
     OrderedCollection by camel-hump. ROW 9: also try  Err  -> expect Error,
     and  Inv  -> InvalidArgument (the broader frozen-floor base image)."
    describe [
        | summary |
        summary := owner.
        ^summary
    ]
]
