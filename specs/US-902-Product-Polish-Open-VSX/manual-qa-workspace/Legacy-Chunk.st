"Legacy-Chunk.st — the classic chunk / bang (!) file-out format, as opposed to the
 bracket syntax used by every other file here. This is the format the GNU Smalltalk
 kernel itself ships in. It proves the parser + Outline handle BOTH container formats:
 the Outline for this file should list `Ledger` with its `entries` and `add:` methods.
 Parses with 0 diagnostics."

Object subclass: #Ledger
    instanceVariableNames: 'entries'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Bank-QA'!

!Ledger methodsFor: 'accessing'!
entries
    "Answer the entries collection, lazily created."
    entries isNil ifTrue: [ entries := OrderedCollection new ].
    ^entries! !

!Ledger methodsFor: 'adding'!
add: anAmount
    self entries add: anAmount.
    ^anAmount! !
