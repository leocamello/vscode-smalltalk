"=============================================================================
  UnterminatedString.st  --  US-414 manual QA, AC4 NEGATIVE case (see README.md).
  INTENTIONAL DEFECT: the string literal on the last line is never closed.
  This is a parse error, but NOT a missing-bracket one.

  Expect (parser tier): a squiggle badged  smalltalk(parse)  with an
  Unterminated string literal message.
  Expect (AC4): NO bracket quick fix is offered here -- the insert-missing
  ]/) actions must appear only for the bracket diagnostics, not for every
  parse error.
  Keep the unterminated string as the LAST thing in the file (it runs to EOF).
  Comments contain no double-quote characters (ASCII only).
============================================================================="

Transcript showCr: 'this string literal is never closed
