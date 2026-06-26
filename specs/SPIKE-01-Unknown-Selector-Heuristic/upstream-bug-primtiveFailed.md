# Upstream bug report — GNU Smalltalk: misspelled selector `primtiveFailed` in `Object>>checkIndexableBounds:put:`

Surfaced by SPIKE-01 (the unknown-selector heuristic) while validating against the GST 3.2.5 kernel
corpus. Confirmed still present in the current Savannah git `master`
(`cgit.git.savannah.gnu.org/cgit/smalltalk.git/plain/kernel/Object.st`), so this is a live defect, not a
3.2.5-only artifact.

---

## Submit to (pick one)

- **Savannah bug tracker (preferred — trackable):** https://savannah.gnu.org/bugs/?group=smalltalk
  (use "Submit a new item"; a Savannah account may be required).
- **Mailing list:** `help-smalltalk@gnu.org`
  (subscribe/info: https://lists.gnu.org/mailman/listinfo/help-smalltalk).

> Note: GNU Smalltalk is largely dormant (3.2.5 was the last release, 2012), so a reply may be slow.

---

## Report (ready to paste)

**Title:** Typo `primtiveFailed` → `primitiveFailed` in `Object>>checkIndexableBounds:put:`

**Component:** kernel — `kernel/Object.st`

**Version:** 3.2.5 (and current git `master`).

**Description:**

`Object>>checkIndexableBounds:put:` raises a misspelled selector on its
"unrecognized shape" error path:

```smalltalk
checkIndexableBounds: anIndex put: object
    ...
    shape == #uint64 ifTrue: [size := 64].
    shape == #int64 ifTrue: [size := 63].
    size isNil ifTrue: [^self primtiveFailed].        "<-- typo: primtiveFailed"
    ^SystemExceptions.ArgumentOutOfRange
        signalOn: object
        mustBeBetween: (size odd ifTrue: [-1 bitShift: size] ifFalse: [0])
        and: (1 bitShift: size) - 1
```

`primtiveFailed` (note the missing `i`) is not a defined method anywhere in the
image. The correct selector, `primitiveFailed`, is defined in the same file:

```smalltalk
primitiveFailed [
    "Called when a VM primitive fails"
    <category: 'built ins'>
    SystemExceptions.PrimitiveFailed signal
]
```

So when `shape` is none of the handled values (`size` stays `nil`), the method
intends to raise `SystemExceptions.PrimitiveFailed` but instead sends the unknown
message `#primtiveFailed`, producing a `doesNotUnderstand:` /
`MessageNotUnderstood` — i.e. the "primitive failed" handler itself fails, with the
wrong error.

**Impact:** Low severity (an error path, reached only for an unexpected indexable
shape), but the error reported is misleading (`doesNotUnderstand: #primtiveFailed`
instead of a `PrimitiveFailed`).

**Fix:** one-character correction.

```diff
-    size isNil ifTrue: [^self primtiveFailed].
+    size isNil ifTrue: [^self primitiveFailed].
```

**How it was found:** a static unknown-selector heuristic resolving `self`-sends
against the closed-world `Object` method table flagged `#primtiveFailed` as a
selector implemented nowhere in the kernel (it occurs exactly once in
`kernel/Object.st`, only at this call site).
