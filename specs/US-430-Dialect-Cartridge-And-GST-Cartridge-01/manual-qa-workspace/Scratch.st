"US-430 manual QA — ROW 10 (robustness / live edit).
 The method below is intentionally half-written. Put the cursor after  x  on the
 incomplete line and type a selector (e.g.  x print ): completions must still
 appear and the server must NEVER throw. Then add a brand-new method under this
 class and confirm its selector becomes completable within a moment (~debounce)."

Object subclass: Scratch [
    broken [
        | x |
        x := 1.
        "type below this line, e.g.  x print  — the statement is deliberately incomplete"
        x
    ]
]
