"Formatting-Playground.st — deliberately LOOSELY SPACED but valid (0 diagnostics).
 Use it for the formatting checks (formatting is always available as of 1.0 — there is
 no enable switch):

   - Format Document (Shift+Alt+F / Shift+Opt+F): normalizes `:=`, binary-operator, and
     keyword spacing, aligns the cascade, collapses blank-line runs — everywhere.
   - Format Selection: select the loosely-spaced TOP-LEVEL statements at the bottom of
     this file (the `| x y | ... ` script block) and format only those lines.
   - On-type: inside a block, type a newline / a closing `]` and watch the indent adjust.
   - blockStyle: set `smalltalk.format.blockStyle` to `expand` and re-format — method and
     multi-statement block bodies reflow one statement per line.

 Formatting is whitespace-only + idempotent: a SECOND Format Document changes nothing,
 and comments/strings are preserved verbatim.

 NOTE on Format Selection: the formatter formats the selected span as a STANDALONE unit,
 so a selection must parse on its own. Selecting a single method line INSIDE a class
 (e.g. just `sum [ ^a+b ]`) yields no edits — a bare `^` isn't valid at the top level —
 so use Format Document for in-class touch-ups, or select a whole class / top-level
 statements for Format Selection."

Object subclass: Messy [
    | a b |

    setUp [ a:=1. b:=2 ]

    sum [ ^a+b ]

    "This comment must survive formatting unchanged."
    cascade [ Transcript show:'a=';show:a printString;show:' b=';show:b printString;cr ]

    keywordCall [ ^Dictionary new at:#one put:1;at:#two put:2;yourself ]
]

"Top-level statements (a standalone span) — good for the Format Selection check.
 Select these three lines and run Format Selection: spacing normalizes, only here."
| x y |
x:=1+2.
y:=x*3.

