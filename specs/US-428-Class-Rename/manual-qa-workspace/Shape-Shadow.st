"Shape-Shadow.st — the `Shape extend [` receiver IS the class and must be renamed,
but the method `demo` has a TEMPORARY named `Shape` (a shadow). Renaming the class
`Shape` must rewrite the receiver on line 4 and leave the local `Shape` inside
`demo` untouched (scope-aware; a same-named local is a different symbol)."

Shape extend [
    demo [ | Shape | Shape := 1. ^Shape ]
]
