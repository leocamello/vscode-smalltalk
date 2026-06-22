"============================================================================
 export-gst-cartridge.st  -  Cartridge #01 exporter for GNU Smalltalk 3.2.5
 ----------------------------------------------------------------------------
 Reflectively walks the running image's Smalltalk namespace and emits a
 DialectCartridge JSON file matching server/src/types/knowledge-base.ts.

 WHY REFLECTION (not static parsing)?
   The image is ground truth: it knows the real method tables, arities,
   class/instance split, categories and superclass chains - no parser ambiguity.
   This is the ADR-0002 'image-export' source adapter, realised for GST.

 ZERO-RUNTIME CONTRACT (ADR-0001) IS NOT VIOLATED:
   gst is required only here, at BUILD time, to generate a committed JSON
   artifact. The shipped extension loads that frozen JSON and needs no gst.

 USAGE:
   gst -f scripts/export-gst-cartridge.st
   (writes server/data/cartridges/gst-3.2.5-cartridge.json - create the dir first)

 TEMPLATE FOR CONTRIBUTORS (future Pharo/Squeak exporters):
   - Keep JsonWriter verbatim - it is dialect-neutral.
   - Reimplement GstCartridgeExporter per dialect: the reflection selectors
     differ (Pharo uses classVariableNames, traitComposition, package), but the
     OUTPUT shape (the Dictionaries you build) must stay identical.
   - Populate traitComposition and documentation where the dialect/licence
     allows (GST is LGPL-2.1 facts-only, so we emit NO prose; Pharo is MIT).

 LEXER NOTE (GST 3.2.5): this file is deliberately ASCII-only and avoids the
 dollar-quote and dollar-backslash character literals, which the 3.2.5 lexer
 mis-handles. All JSON punctuation is emitted via single-quoted string literals;
 GST strings do NOT process escape sequences, so a two-character literal of
 backslash then t stays those two bytes - exactly what JSON wants. (Comments
 here must avoid the double-quote character, since comments are quote-delimited.)
 ============================================================================"


"--------------------------------------------------------------------------
 JsonWriter - a tiny, dependency-free JSON serialiser. Accepts nested
 Dictionary (=> object), Collection (=> array), String/Symbol (=> string),
 Integer/Number, Boolean and nil. Object keys are SORTED for stable diffs.
 --------------------------------------------------------------------------"
Object subclass: JsonWriter [

    JsonWriter class >> write: anObject on: aStream [
        anObject isNil ifTrue: [ ^aStream nextPutAll: 'null' ].
        anObject == true ifTrue: [ ^aStream nextPutAll: 'true' ].
        anObject == false ifTrue: [ ^aStream nextPutAll: 'false' ].
        (anObject isKindOf: Integer) ifTrue: [ ^aStream nextPutAll: anObject printString ].
        (anObject isKindOf: Number)  ifTrue: [ ^aStream nextPutAll: anObject printString ].
        (anObject isString or: [ anObject isSymbol ])
            ifTrue: [ ^self writeString: anObject asString on: aStream ].
        (anObject isKindOf: Dictionary)
            ifTrue: [ ^self writeObject: anObject on: aStream ].
        (anObject isKindOf: Collection)
            ifTrue: [ ^self writeArray: anObject on: aStream ].
        "Fallback: anything exotic is stringified, never crashes the export."
        ^self writeString: anObject printString on: aStream
    ]

    JsonWriter class >> writeObject: aDictionary on: aStream [
        "Emit a JSON object with keys sorted for deterministic output."
        | first keys |
        aStream nextPutAll: '{'.
        keys := aDictionary keys asSortedCollection: [ :a :b | a asString <= b asString ].
        first := true.
        keys do: [ :k |
            first ifFalse: [ aStream nextPutAll: ',' ].
            first := false.
            self writeString: k asString on: aStream.
            aStream nextPutAll: ':'.
            self write: (aDictionary at: k) on: aStream ].
        aStream nextPutAll: '}'
    ]

    JsonWriter class >> writeArray: aCollection on: aStream [
        "Emit a JSON array, preserving the collection's iteration order."
        | first |
        aStream nextPutAll: '['.
        first := true.
        aCollection do: [ :each |
            first ifFalse: [ aStream nextPutAll: ',' ].
            first := false.
            self write: each on: aStream ].
        aStream nextPutAll: ']'
    ]

    JsonWriter class >> writeString: aString on: aStream [
        "Emit a quoted, escaped JSON string."
        aStream nextPutAll: '"'.
        aString do: [ :c | self writeChar: c on: aStream ].
        aStream nextPutAll: '"'
    ]

    JsonWriter class >> writeChar: c on: aStream [
        "Escape one character. Remember: GST strings are literal, so '\t' below is
         the two bytes backslash+t, and '\\' is the two bytes backslash+backslash."
        c = (Character value: 34) ifTrue: [ ^aStream nextPutAll: '\"' ].  "double quote"
        c = (Character value: 92) ifTrue: [ ^aStream nextPutAll: '\\' ].  "backslash"
        c = Character tab ifTrue: [ ^aStream nextPutAll: '\t' ].
        c = Character lf  ifTrue: [ ^aStream nextPutAll: '\n' ].
        c = Character cr  ifTrue: [ ^aStream nextPutAll: '\r' ].
        c value < 32 ifTrue: [ ^aStream nextPutAll: '\u', (self hex4: c value) ].
        aStream nextPut: c
    ]

    JsonWriter class >> hex4: anInteger [
        "Zero-padded 4-digit hex for \uXXXX escapes of control characters."
        | hex |
        hex := anInteger printString: 16.
        ^(String new: (4 - hex size max: 0) withAll: $0) , hex
    ]
]


"--------------------------------------------------------------------------
 GstCartridgeExporter - the dialect-specific reflection. Everything that
 touches GST-only selectors lives here; swap this class out to build a
 Pharo/Squeak cartridge while keeping JsonWriter and the output shape.
 --------------------------------------------------------------------------"
Object subclass: GstCartridgeExporter [

    "Default output path. The directory must already exist."
    GstCartridgeExporter class >> outputPath [
        ^'server/data/cartridges/gst-3.2.5-cartridge.json'
    ]

    "Namespaces to scan. GST kernel classes live under Smalltalk; extend this
     list (or recurse sub-namespaces) for images that spread across namespaces."
    GstCartridgeExporter class >> namespacesToScan [
        ^Array with: Smalltalk
    ]

    "------------------------- entry point -------------------------"
    GstCartridgeExporter class >> run [
        | cartridge stream |
        cartridge := self buildCartridge.
        stream := WriteStream on: (String new: 1024 * 1024).
        JsonWriter write: cartridge on: stream.
        self writeString: stream contents toFile: self outputPath.
        Transcript showCr: 'Wrote ', self outputPath,
            ' (', (cartridge at: 'classes') size printString, ' classes).'
    ]

    GstCartridgeExporter class >> writeString: aString toFile: aPath [
        "Persist the assembled JSON."
        | file |
        file := FileStream open: aPath mode: 'w'.
        file nextPutAll: aString.
        file close
    ]

    "------------------------- assembly -------------------------"
    GstCartridgeExporter class >> buildCartridge [
        | classes xref cartridge |
        classes := Dictionary new.
        xref := self newCrossReference.

        "Single traversal: build each class's facts AND feed the call graph."
        self allKernelClassesDo: [ :cls |
            [ | id |
              id := self idOf: cls.
              classes at: id put: (self factForClass: cls).
              self indexBehavior: cls      side: 'instance' into: xref.
              self indexBehavior: cls class side: 'class'    into: xref ]
            on: Error
            do: [ :e | Transcript showCr: 'skip ', cls printString, ': ', e messageText ] ].

        cartridge := Dictionary new.
        cartridge at: 'header'         put: (self headerFor: classes).
        cartridge at: 'classes'        put: classes.
        cartridge at: 'crossReference' put: (self crossReferenceToArrays: xref).
        ^cartridge
    ]

    GstCartridgeExporter class >> headerFor: aClassDictionary [
        "Cartridge header. contentHash is left 'pending' - the TypeScript build
         step stamps the real SHA-256. builtAt is the one volatile field; pin it
         in CI for byte-identical rebuilds."
        | header |
        header := Dictionary new.
        header at: 'schema'        put: 1.
        header at: 'dialect'       put: 'gnu-smalltalk'.
        header at: 'dialectLabel'  put: 'GNU Smalltalk'.
        "'base' = the full loaded base image (~496 classes via the subclass tree),
         which is richer than the 122 kernel/*.st source files (~250 classes) the
         static indexer parses. See US-430 reconciliation."
        header at: 'library'       put: 'base'.
        header at: 'version'       put: '3.2.5'.
        header at: 'source'        put: 'image-export'.
        header at: 'sourceLicense' put: 'LGPL-2.1-only'.
        "GST kernel comments are LGPL prose - facts only, so carriesProse=false
         and no documentation fields are ever emitted."
        header at: 'carriesProse'  put: false.
        header at: 'tiers'         put: (Array with: 'classes' with: 'crossReference').
        header at: 'builtAt'       put: self isoNow.
        header at: 'contentHash'   put: 'pending'.
        ^header
    ]

    "------------------------- per-class facts -------------------------"
    GstCartridgeExporter class >> factForClass: cls [
        | fact |
        fact := Dictionary new.
        fact at: 'id'   put: (self idOf: cls).
        fact at: 'name' put: cls name asString.
        fact at: 'kind' put: 'class'.   "GST has no traits; always a plain class."
        fact at: 'superclass'
             put: (cls superclass isNil ifTrue: [ nil ] ifFalse: [ self idOf: cls superclass ]).
        fact at: 'instanceVariables'      put: (self instVarNamesOf: cls).
        fact at: 'classVariables'         put: (self classVarNamesOf: cls).
        fact at: 'classInstanceVariables' put: (self instVarNamesOf: cls class).
        fact at: 'instanceMethods'        put: (self signaturesFor: cls).
        fact at: 'classMethods'           put: (self signaturesFor: cls class).
        fact at: 'taxonomy'               put: (self taxonomyOf: cls).
        ^fact
    ]

    GstCartridgeExporter class >> signaturesFor: aBehavior [
        "All LOCALLY-defined selectors of aBehavior (inherited members are
         resolved later, at index-build time - we never denormalise them here)."
        | sigs |
        sigs := OrderedCollection new.
        aBehavior selectors asSortedCollection do: [ :sel |
            sigs add: (self signatureFor: sel in: aBehavior) ].
        ^sigs asArray
    ]

    GstCartridgeExporter class >> signatureFor: aSelector in: aBehavior [
        | sig method category |
        sig := Dictionary new.
        sig at: 'selector' put: aSelector asString.
        sig at: 'arity'    put: aSelector numArgs.
        sig at: 'keywords' put: (self keywordsOf: aSelector).

        method := aBehavior compiledMethodAt: aSelector ifAbsent: [ nil ].
        method notNil ifTrue: [
            category := self categoryOf: aSelector in: aBehavior.
            category notNil ifTrue: [ sig at: 'protocol' put: category asString ].
            "Only the primitive pragma is cheap to recover reflectively."
            (self primitiveOf: method) > 0 ifTrue: [
                sig at: 'pragmas'
                    put: (Array with: '<primitive: ', (self primitiveOf: method) printString, '>') ] ].
        ^sig
    ]

    "------------------ cross-reference (senders/implementors) ------------------"
    GstCartridgeExporter class >> newCrossReference [
        | xref |
        xref := Dictionary new.
        xref at: 'implementors' put: Dictionary new.
        xref at: 'senders'      put: Dictionary new.
        ^xref
    ]

    GstCartridgeExporter class >> indexBehavior: aBehavior side: sideString into: xref [
        "For every local method: record it as an implementor of its own selector,
         and scan its literal frame for the selectors it SENDS."
        | implementors senders base |
        implementors := xref at: 'implementors'.
        senders := xref at: 'senders'.
        base := (aBehavior isKindOf: Metaclass)
                    ifTrue: [ aBehavior instanceClass ] ifFalse: [ aBehavior ].

        aBehavior selectors do: [ :sel |
            | method |
            (implementors at: sel asString ifAbsentPut: [ OrderedCollection new ])
                add: (self implementorRefFor: base side: sideString).

            method := aBehavior compiledMethodAt: sel ifAbsent: [ nil ].
            method notNil ifTrue: [
                (self sentSelectorsOf: method) do: [ :sentSel |
                    (senders at: sentSel asString ifAbsentPut: [ OrderedCollection new ])
                        add: (self sendSiteFor: base side: sideString inSelector: sel) ] ] ]
    ]

    GstCartridgeExporter class >> sentSelectorsOf: aCompiledMethod [
        "Heuristic: the symbols in a method's literal frame are the selectors it
         sends (and the symbol literals it merely uses as data - an accepted
         over-count). CAVEAT: special selectors compiled to dedicated bytecodes
         (+ - = == at: at:put: do: when inlined) are NOT in the literal frame, so
         they are under-counted. A bytecode pass can refine this later."
        | result lits |
        result := Set new.
        lits := self literalsOf: aCompiledMethod.
        lits do: [ :lit | lit isSymbol ifTrue: [ result add: lit ] ].
        ^result
    ]

    GstCartridgeExporter class >> implementorRefFor: aClass side: sideString [
        | ref |
        ref := Dictionary new.
        ref at: 'inClass' put: (self idOf: aClass).
        ref at: 'side'    put: sideString.
        ^ref
    ]

    GstCartridgeExporter class >> sendSiteFor: aClass side: sideString inSelector: aSelector [
        "Repeated sends of the same selector within one method collapse to a single
         site (line 0). receiverHint is omitted: reflective literal scanning cannot
         resolve a send's receiver - the workspace AST path does."
        | site |
        site := Dictionary new.
        site at: 'inClass'    put: (self idOf: aClass).
        site at: 'side'       put: sideString.
        site at: 'inSelector' put: aSelector asString.
        site at: 'line'       put: 0.
        ^site
    ]

    GstCartridgeExporter class >> crossReferenceToArrays: xref [
        "Freeze each selector's bucket from OrderedCollection to Array."
        | out |
        out := Dictionary new.
        out at: 'implementors' put: (self valuesAsArrays: (xref at: 'implementors')).
        out at: 'senders'      put: (self valuesAsArrays: (xref at: 'senders')).
        ^out
    ]

    GstCartridgeExporter class >> valuesAsArrays: aDictionary [
        | out |
        out := Dictionary new.
        aDictionary keysAndValuesDo: [ :k :v | out at: k put: v asArray ].
        ^out
    ]

    "------------------------- guarded GST reflection helpers -------------------------
     Each wrapper isolates one possibly-version-specific selector so a single
     missing API thins one field instead of dropping a whole class."

    GstCartridgeExporter class >> allKernelClassesDo: aBlock [
        "Yield every class in the loaded image, walking the subclass tree so that
         classes in sub-namespaces (SystemExceptions, NetClients, Kernel, VFS, ...)
         are included - not just the Smalltalk namespace's direct bindings.
         Metaclasses are reached separately via 'cls class', so they are not
         yielded here. (For a namespace-restricted export, iterate
         namespacesToScan with associationsDo: instead.)"
        | all |
        all := IdentitySet new.
        all add: Object.
        Object allSubclasses do: [ :c | (c isKindOf: Class) ifTrue: [ all add: c ] ].
        all do: [ :c | aBlock value: c ]
    ]

    GstCartridgeExporter class >> idOf: aClass [
        "ClassId = '<Namespace>.<Name>'. The root kernel namespace is 'Smalltalk'."
        ^(self namespaceNameOf: aClass) , '.' , aClass name asString
    ]

    GstCartridgeExporter class >> namespaceNameOf: aClass [
        ^[ aClass environment isNil
              ifTrue: [ 'Smalltalk' ]
              ifFalse: [ aClass environment name asString ] ]
            on: Error do: [ :e | 'Smalltalk' ]
    ]

    GstCartridgeExporter class >> instVarNamesOf: aBehavior [
        ^self namesArray: ([ aBehavior instanceVariableNames ] on: Error do: [ :e | #() ])
    ]

    GstCartridgeExporter class >> classVarNamesOf: aClass [
        ^self namesArray: ([ aClass classVarNames ] on: Error do: [ :e | #() ])
    ]

    GstCartridgeExporter class >> namesArray: aCollection [
        "Normalise a names collection (Symbols/Strings/nil) to an Array of Strings."
        aCollection isNil ifTrue: [ ^Array new ].
        ^aCollection asArray collect: [ :each | each asString ]
    ]

    GstCartridgeExporter class >> taxonomyOf: aClass [
        "GST's organizational vocabulary -> the neutral taxonomy bag. A Pharo
         exporter fills {package, tag} into the SAME field with no schema change."
        | tax category |
        tax := Dictionary new.
        tax at: 'namespace' put: (self namespaceNameOf: aClass).
        category := [ aClass category ] on: Error do: [ :e | nil ].
        category notNil ifTrue: [ tax at: 'category' put: category asString ].
        ^tax
    ]

    GstCartridgeExporter class >> categoryOf: aSelector in: aBehavior [
        "Method category (protocol), if the class organisation knows it."
        ^[ aBehavior organization categoryOfElement: aSelector ]
            on: Error do: [ :e | nil ]
    ]

    GstCartridgeExporter class >> primitiveOf: aCompiledMethod [
        "Primitive number, or 0 if none / not introspectable on this build."
        ^[ aCompiledMethod primitive ] on: Error do: [ :e | 0 ]
    ]

    GstCartridgeExporter class >> literalsOf: aCompiledMethod [
        "The method's literal frame, or empty if not introspectable on this build."
        ^[ aCompiledMethod literals ] on: Error do: [ :e | #() ]
    ]

    GstCartridgeExporter class >> keywordsOf: aSelector [
        "Snippet parts. Unary/binary -> [selector]; keyword 'at:put:' -> ['at:','put:']."
        | str |
        str := aSelector asString.
        (str isEmpty or: [ str last ~= $: ])
            ifTrue: [ ^Array with: str ].                "unary or binary"
        ^(str subStrings: ':') collect: [ :part | part , ':' ]   "keyword parts"
    ]

    GstCartridgeExporter class >> isoNow [
        "Best-effort ISO-8601 UTC timestamp for the header builtAt."
        ^[ DateTime now asUTC printString trimSeparators ]
            on: Error do: [ :e | Date today printString trimSeparators ]
    ]
]


"---------------------------------------------------------------------------
 Kick off the export. (Top-level statements are evaluated by gst -f.)
 ---------------------------------------------------------------------------"
GstCartridgeExporter run.
