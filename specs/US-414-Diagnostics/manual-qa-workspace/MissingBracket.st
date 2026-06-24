"=============================================================================
  MissingBracket.st  --  US-414 manual QA (see README.md).
  INTENTIONAL DEFECT: the class body is missing its closing ']' (the `add:`
  method below is never closed, so the class definition runs off the end).

  Expect (parser tier, always on):
    * a red squiggle near the end, badged  smalltalk(parse)
    * message like  Expected \ to close definition  (an Expected ] message)
  Expect (gst tier, when useGst is true and you SAVE -- see README rows 7/8):
    * a second diagnostic badged  gst(compile)  e.g.  parse error, expected ]

  AC4 quick fix: put the caret on the squiggle, press Ctrl/Cmd + .  ->
    'Insert missing \]'  (an Insert missing ] action). Applying it appends ] and
    clears the parser squiggle. Undo (Ctrl/Cmd+Z) to re-run the row.
  Comments contain no double-quote characters (ASCII only).
============================================================================="

Object subclass: Ledger [
    | total |

    init [ total := 0 ]

    add: anAmount [
        total := total + anAmount.
        ^total
