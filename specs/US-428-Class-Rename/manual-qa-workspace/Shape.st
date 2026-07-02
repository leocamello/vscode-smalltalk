"Shape.st — the primary class. The class `Shape` is declared here and referenced
across this workspace (Shape-Draw.st, Shape-Shadow.st, Shape-Namespace.st).
Renaming `Shape` (F2) must rewrite EVERY resolved reference in ALL files — and
never touch a kernel class, a same-named local, or a same-named class in another
namespace. `OrderedCollection` is a kernel class: F2 on it must be REJECTED, and
renaming `Shape` INTO `OrderedCollection` must be REFUSED (collision)."

Object subclass: Shape [
    | name sides |

    name [ ^name ]

    sides [ ^sides ]

    setName: aName [ name := aName ]

    parts [ ^OrderedCollection new ]
]
