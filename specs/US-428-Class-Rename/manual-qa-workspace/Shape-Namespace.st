"Shape-Namespace.st — namespaced + binding-constant references. `#{Shape}` (a binding
constant) and the default-namespace `Smalltalk.Shape` both resolve to the class and
are rewritten — the CLASS SEGMENT only, preserving `#{…}`/`Smalltalk.`. A same-named
class living in an unrelated namespace would be left untouched (resolution-gated).
`Painter` is a new class; its name is never renamed when renaming `Shape`."

Object subclass: Painter [
    paintOne [ ^#{Shape} new ]

    paintTwo [ ^Smalltalk.Shape new ]
]
