"Demo.st — a SELF-CONTAINED GNU Smalltalk script for the Run Current File test.
 It defines its own `Greeter` class (no dependency on the Bank files) so it runs
 standalone with `gst Demo.st` and prints two lines.

 Run it with 'Smalltalk: Run Current File' (right-click, or the Command Palette):
   - In an UNTRUSTED workspace it is REFUSED with a 'Manage Workspace Trust' prompt —
     gst is never spawned from an untrusted folder.
   - Trust the workspace and run again: it prints to the Smalltalk terminal (needs gst).

 The Bank/Account/etc. classes need NO gst — they power the offline language
 intelligence you exercise in the other files. Parses with 0 diagnostics."

Object subclass: Greeter [
    | who |

    who: aName [ who := aName ]

    greet [ ^'Hello, ', who, '!' ]
]

| greeter total |
greeter := Greeter new.
greeter who: 'Smalltalk'.
Transcript showCr: greeter greet.

total := (1 to: 100) inject: 0 into: [:acc :each | acc + each].
Transcript showCr: 'Sum 1 to 100 = ', total printString.
