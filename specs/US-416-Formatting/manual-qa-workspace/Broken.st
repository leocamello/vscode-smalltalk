"Broken.st — deliberately MALFORMED (the method body block is never closed).
The parser reports a diagnostic (you'll see a red squiggle). Formatting must be
NON-DESTRUCTIVE: Format Document on this file produces NO edits and changes
NOTHING (whole-file skip on any parse error). This is the data-loss safety gate."

Object subclass: Broken [
    oops [ ^self missingCloser
]
