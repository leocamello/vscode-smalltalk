"Diagnostics-Playground.st — starts CLEAN (0 diagnostics). This file is where you
 INTRODUCE errors to test live diagnostics + quick fixes, then undo (Ctrl/Cmd+Z).

 Test A — insert-closer quick fix:
   Delete the closing `]` at the end of the `broken` method below. A red
   `smalltalk(parse)` squiggle appears as you type. Hover it, open the lightbulb
   (Ctrl/Cmd+.), and apply 'Insert missing ]'. The squiggle clears and the file
   re-parses clean. Undo.

 Test B — close-string quick fix:
   In `aString`, delete the CLOSING quote of the string (the single quote right
   before the line break). The string is now unterminated; a diagnostic appears.
   Open the lightbulb and apply the insert-missing-quote fix. The quote is restored
   at the end of the string's line and the file re-parses clean. Undo.
   (The string is on its OWN line so the fix has a clean place to close it — if
   code follows the opener on the same line, the fix closes at end-of-line.)

 The squiggle and the quick-fix edit should land on the DEFECT line, not drift."

Object subclass: Playground [
    broken [ ^42 ]

    aString [
        ^'delete my closing quote to test the close-string quick fix'
    ]
]
