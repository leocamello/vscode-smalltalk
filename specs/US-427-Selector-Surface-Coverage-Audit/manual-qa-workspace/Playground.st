"US-427 — snippet playground.

Each method below is a realistic spot to exercise one new block-bearing snippet.
Put the cursor on the line marked  >>> <prefix>  (delete the comment first if you
like), type the prefix, accept the snippet from the suggestion widget (Tab/Enter),
and Tab through the placeholders. The receiver is already there so the expansion
reads as valid GST.

These new snippets are MESSAGE FRAGMENTS — they attach to the receiver to their
left. Existing prefixes (ift, do, col, to, ifn, ifnn, rep) must still expand
unchanged; that is the no-collision check (Part B in the README)."

Object subclass: SnippetDemo [
    | bins log |

    loopWhile [
        "A receiver block is in place; add the loop message after it."
        | i |
        i := 0.
        [ i < 10 ] >>> wt        "[ i < 10 ] whileTrue: [ ... ]"
        [ i >= 10 ] >>> wf       "[ i >= 10 ] whileFalse: [ ... ]"
    ]

    guarded [
        "Exception / unwind idioms — type the prefix after the receiver block."
        [ self risky ] >>> ond   "[ ... ] on: Error do: [ :ex | ... ]"
        [ self risky ] >>> ens   "[ ... ] ensure: [ ... ]"
        [ self risky ] >>> ifc   "[ ... ] ifCurtailed: [ ... ]"
    ]

    nilGuards: anObject [
        "Two-arm nil guards. Note ifn / ifnn (existing) vs ifninn / ifnnin (new)."
        anObject >>> ifninn      "anObject ifNil: [ ... ] ifNotNil: [ :arg | ... ]"
        anObject >>> ifnnin      "anObject ifNotNil: [ :arg | ... ] ifNil: [ ... ]"
    ]

    dictionaryAccess: aKey [
        "Dictionary lookup / memoize / iteration."
        bins >>> atia            "bins at: key ifAbsent: [ ... ]"
        bins >>> atiap           "bins at: key ifAbsentPut: [ ... ]"
        bins >>> kvd             "bins keysAndValuesDo: [ :key :value | ... ]"
        bins >>> kd              "bins keysDo: [ :key | ... ]"
    ]

    folding [
        "Folding / indexing iteration."
        bins >>> inj             "bins inject: 0 into: [ :acc :each | ... ]"
        bins >>> dwi             "bins doWithIndex: [ :each :index | ... ]"
        bins >>> detin           "bins detect: [ :each | ... ] ifNone: [ ... ]"
    ]

    regressionCheck [
        "Part B — existing prefixes must be unchanged."
        bins >>> do              "bins do: [ :each | ... ]"
        bins >>> col             "bins collect: [ :each | ... ]"
        true >>> ift             "true ifTrue: [ ... ]"
        1 >>> to                 "1 to: count do: [ :i | ... ]"
    ]
]
