"=============================================================================
  Clean.st  --  US-414 manual QA baseline (see README.md).
  Every construct here is valid GNU Smalltalk: it must parse with ZERO
  diagnostics, and 'Validate with gst' (or useGst on save) must report nothing.
  Use it for the no-noise rows and the live-typing / flicker row: type a partial
  send like  total prin  inside a method and watch the squiggle settle (no
  flicker) ~250 ms after you stop -- then finish it to  total printNl.
  NOTE: comments use only ASCII and contain no double-quote characters.
============================================================================="

Object subclass: Ledger [
    | entries total |

    init [
        entries := OrderedCollection new.
        total := 0
    ]

    add: anAmount [
        entries add: anAmount.
        total := total + anAmount.
        ^total
    ]

    describe [
        Transcript showCr: 'entries: ', entries size printString.
        Transcript showCr: 'total: ', total printString
    ]
]
