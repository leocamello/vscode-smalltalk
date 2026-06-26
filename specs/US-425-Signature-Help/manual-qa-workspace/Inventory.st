"US-425 manual-QA fixture — keyword-message signature help.
 Valid GST 3.2.5; must parse with 0 diagnostics. The class defines a workspace
 keyword method (#stock:at:) so signature help shows a `workspace` signature, and
 the methods send kernel keyword messages (#at:put:, #at:ifAbsent:, #do:separatedBy:,
 #inject:into:) so the bundled cartridge supplies kernel signatures — all no gst."

Object subclass: Inventory [
    | bins |

    init [ bins := Dictionary new ]

    "A workspace keyword method — signature help offers `stock:at:` with provenance `workspace`."
    stock: aProduct at: aLocation [
        bins at: aLocation put: aProduct.
        ^aProduct
    ]

    "Kernel keyword sends — type each one and watch the popup track the active keyword."
    demo [
        | total |
        bins at: #widget put: 10.
        bins at: #gadget ifAbsent: [ 0 ].
        total := bins values inject: 0 into: [ :sum :each | sum + each ].
        bins keys asSortedCollection do: [ :k | Transcript show: k ] separatedBy: [ Transcript show: ', ' ].
        self stock: #sprocket at: #binA.
        ^total
    ]
]
