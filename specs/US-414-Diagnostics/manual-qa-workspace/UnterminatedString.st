"=============================================================================
  UnterminatedString.st  --  US-414 manual QA (see README.md).
  INTENTIONAL DEFECT: the string literal on the last line is never closed.

  Expect (parser tier): a squiggle badged  smalltalk(parse)  with an
  Unterminated string literal  message.
  AC4 quick fix: caret on the squiggle, Ctrl/Cmd + .  ->  the close-quote action
  inserts a ' at the END of the line where the string opened, so the squiggle
  clears. (For a string that ran over later lines, closing at end-of-open-line
  preserves that following code instead of swallowing it.) Undo to re-run.
  Keep the unterminated string as the LAST thing in the file (it runs to EOF).
  Comments contain no double-quote characters (ASCII only).
============================================================================="

Transcript showCr: 'this string literal is never closed
