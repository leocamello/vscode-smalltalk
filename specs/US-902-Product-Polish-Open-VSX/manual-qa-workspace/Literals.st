"Literals.st — numeric and other literals for HOVER decode and syntax highlighting.
 Hover each returned literal to see its decoded value (e.g. 16rFF -> 255, 2r1010 -> 10,
 1.50s2 -> a ScaledDecimal). The `radixThenPeriod` method demonstrates the radix-integer
 coloring fix: `16rFF.` colors `16rFF` as ONE number and the trailing `.` separately (a
 statement terminator) — the period is NOT swallowed into the literal.
 Parses with 0 diagnostics."

Object subclass: Literals [
    hexByte     [ ^16rFF ]
    binary      [ ^2r1010 ]
    million     [ ^1000000 ]
    pi          [ ^3.14159 ]
    scaled      [ ^1.50s2 ]
    exponent    [ ^1e6 ]
    character   [ ^$A ]
    symbol      [ ^#deposit ]
    string      [ ^'hello, world' ]
    array       [ ^#(1 2 3 $a #sym 'str') ]
    negative    [ ^-42 ]

    "The `.` after 16rFF terminates the statement — it must color separately,
     not as part of the number (the radix-integer coloring fix)."
    radixThenPeriod [
        | x |
        x := 16rFF.
        ^x
    ]
]
