"=============================================================================
  NoQuickFix.st  --  US-414 manual QA, AC4 NEGATIVE case (see README.md).
  INTENTIONAL DEFECT: a truncated expression (binary send with no right operand).
  This is a parse error, but there is nothing trivial to insert.

  Expect (parser tier): a squiggle badged  smalltalk(parse)  (an Unexpected EOF
  message).
  Expect (AC4): NO quick fix is offered -- the insert-missing actions only apply
  to missing closers (] ) } >) and unterminated strings, not to every parse error.
  Comments contain no double-quote characters (ASCII only).
============================================================================="

| x |
x := 1 +
