"=============================================================================
  US-417 manual spot-check — open THIS file in the Extension Development Host
  (F5 -> [Extension Development Host] -> open this file).
  Follow the S1/S2/S3 instructions inline. Every construct here is valid
  GNU Smalltalk and parses with zero diagnostics, so any odd behaviour is the
  feature under test, not a parse error.
============================================================================="

"-----------------------------------------------------------------------------
  S1 - CODE FOLDING
  Hover the LEFT GUTTER (between line numbers and code) next to each marked
  construct; click the chevron to fold, click again to expand.
  Quick way to see ALL fold points: Ctrl+K Ctrl+0 (fold all) then
  Ctrl+K Ctrl+J (unfold all).
  Expect a fold for: (a) this multi-line comment, (b) the class body,
  (c) the multi-line method `sum:`, (d) the inner block `[ :each | ... ]`.
-----------------------------------------------------------------------------"

Object subclass: SpotCheck [
    | total |

    "S1(c)+(d): fold this whole method, and the inner block separately."
    sum: aCollection [
        | running |
        running := 0.
        aCollection do: [ :each |
            running := running + each
        ].
        ^running
    ]

    "-------------------------------------------------------------------------
      S2 - HIGHLIGHT A SELECTOR
      Click on the word `printNl` below -> BOTH sends highlight.
      Click on `at:` (or `put:`) -> BOTH `at:put:` sends highlight, each part.
      (Note: a method *definition* of a selector is not highlighted, only sends.)
    -------------------------------------------------------------------------"
    demo [
        1 printNl.
        2 printNl.
        Smalltalk at: #A put: 1.
        Smalltalk at: #B put: 2
    ]

    "-------------------------------------------------------------------------
      S3 - HIGHLIGHT A SCOPED VARIABLE
      Click on `x` inside `methodA` -> ONLY methodA's x highlights
        (its declaration and both uses).
      Click on `x` inside `methodB` -> ONLY methodB's x highlights.
      The two `x` are independent locals: highlighting one must NOT light up
      the other method's x. (This is the scope-aware behaviour, not text match.)
    -------------------------------------------------------------------------"
    methodA [
        | x |
        x := 1.
        ^x + x
    ]

    methodB [
        | x |
        x := 99.
        ^x
    ]
]
