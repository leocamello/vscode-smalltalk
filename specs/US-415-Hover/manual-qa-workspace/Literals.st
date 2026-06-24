"US-415 manual QA — numeric literals (AC4). Valid GST, 0 diagnostics.
 Hover each literal in the array; expect the decoded value in the hover."

Object subclass: Literals [
    "ROW 4 (numeric literals): hover each element below. Expect:
       16rFF   -> 16rFF = 255 (base 16)
       2r1010  -> 2r1010 = 10 (base 2)
       8r17    -> 8r17 = 15 (base 8)
       3.14s2  -> 3.14s2 = 3.14 (scale 2)
       100s    -> 100s = 100 (scale default)
       255     -> 255 = 255          (a plain decimal still gets a value line)
       3.5     -> 3.5 = 3.5          (float)
     ROW 5 (no hover): hover the STRING 'hi' or the symbol #tag below -> NO hover
     (strings/symbols/chars carry no extra facts today; this is expected)."
    samples [
        ^{ 16rFF. 2r1010. 8r17. 3.14s2. 100s. 255. 3.5 }
    ]

    nonNumeric [
        ^{ 'hi'. #tag. $a }
    ]
]
