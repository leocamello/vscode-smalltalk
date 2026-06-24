"US-415 manual QA - ROW R1 (robustness / live edit).
 UNLIKE the other files, the method below is intentionally incomplete: the line
 total foo:  is a half-typed keyword message with no argument, so this file WILL
 show a parse squiggle near the end of the method - that is expected HERE only.
 This is exactly the mid-typing state where hover still has to work. The point:
 hovering the GOOD tokens must still work and the server must NEVER throw.

 Try it (hover the USE sites, not the | declarations |):
   - hover  count  in  count := 0           -> instance variable + declaration site
   - hover  total  in  total := count        -> temporary + declaration site
   - hover  16rFF                            -> 16rFF = 255 (base 16)
   - hover  Scratch (the class name)         -> class, chain  Scratch -> Object
   - hover the half-typed  foo:              -> keyword selector (no implementors yet)

 ROW R2 (live edit): finish the method (replace  total foo:  with  ^total ), then
 add a new method whose body opens with a leading double-quoted comment, add a
 send  Scratch new <yourSelector>  and hover that selector: within ~a second
 (debounced re-index) the hover shows its facts plus your comment text."

Object subclass: Scratch [
    | count |

    broken [
        | total |
        count := 0.
        total := count + 16rFF.
        total foo:
    ]
]
