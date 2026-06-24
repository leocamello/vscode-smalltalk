"=============================================================================
  MissingParen.st  --  US-414 manual QA (see README.md).
  INTENTIONAL DEFECT: the parenthesised expression in `compute` is missing its
  closing ')'.

  Expect (parser tier): a squiggle badged  smalltalk(parse)  with an Expected )
  message.
  AC4 quick fix: caret on the squiggle, Ctrl/Cmd + .  ->  'Insert missing )'.
  Applying it inserts ) and clears the squiggle. Undo to re-run.
  Comments contain no double-quote characters (ASCII only).
============================================================================="

Object subclass: Calc [
    compute [
        | x |
        x := (1 + 2 * 3.
        ^x
    ]
]
