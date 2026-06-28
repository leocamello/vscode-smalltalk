"Messy.st — VALID GNU Smalltalk that parses with 0 diagnostics, but is
deliberately badly spaced / indented / un-aligned. Format Document should
normalize it (and a second format must be a no-op). No code is changed — only
whitespace. See README.md for the expected result."

Object subclass: Account [
| balance owner |



init [balance:=0]

deposit: amount [balance:=balance+amount.   ^self]

report [Transcript showCr:'opening';showCr:'closing';nl]

transfer: amount to: other [self balance < amount ifTrue:[^self error:'insufficient']. other deposit:amount. ^self]

describe [^SystemExceptions.NotYetImplemented]
]
