"=============================================================================
  MissingBrace.st  --  US-414 manual QA (see README.md).
  INTENTIONAL DEFECT: the dynamic-array literal is missing its closing '}'.

  Expect (parser tier): a squiggle badged  smalltalk(parse)  with an Expected }
  message.
  AC4 quick fix: caret on the squiggle, Ctrl/Cmd + .  ->  'Insert missing }'.
  Applying it inserts } and the squiggle clears. Undo to re-run.
  Comments contain no double-quote characters (ASCII only).
============================================================================="

| nums |
nums := { 1. 2. 3
