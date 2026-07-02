// Unit tests for workspace-wide class rename (US-428). Runs in Node via tsx.
//
// Acceptance Harness — written BEFORE the resolver: RED until
// server/src/xref/classRefs.ts ships and providers/rename.ts grows a `class`
// classification. Covers (spec.md §4):
//   - prepareRename: accept a workspace class; reject a kernel class, a selector,
//     an unknown global, and a local temp/arg that merely shares the name (AC1/AC3),
//   - classOccurrences rewrites every RESOLVED reference form — declaration,
//     receiver/superclass Variables, `class`/`extend` receivers, `#{Foo}`, and
//     the qualified `A.B`/`A::B` (class segment only) — and nothing else (AC2),
//   - resolution-gating: an unrelated-namespace `Q::Foo` and a shadowing local are
//     skipped (AC3),
//   - new-name validation: kernel + workspace collision and non-class identifier
//     are refused (AC4),
//   - the edit stays within resolved references; comments/strings untouched (AC5).
import assert from 'node:assert/strict';
import { parse } from '../src/parser/parser.ts';
import { tokenize } from '../src/parser/lexer.ts';
import { buildSymbolTable } from '../src/parser/symbols.ts';
import { classOccurrences, type ClassWorld } from '../src/xref/classRefs.ts';
import { prepareRenameAt, renameAt } from '../src/providers/rename.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

const isReject = (r: unknown): r is { reject: string } =>
  typeof r === 'object' && r !== null && 'reject' in r;

/** Byte offset just inside the `occurrence`-th appearance of `sub` in `src`. */
function at(src: string, sub: string, occurrence = 1): number {
  let idx = -1;
  for (let i = 0; i < occurrence; i++) idx = src.indexOf(sub, idx + 1);
  assert.ok(idx >= 0, `fixture must contain occurrence ${occurrence} of ${JSON.stringify(sub)}`);
  return idx + 1;
}

/** A test ClassWorld: workspace class names, kernel class names, namespace map. */
function world(workspace: string[], kernel: string[] = [], ns: Record<string, string> = {}): ClassWorld {
  const ws = new Set(workspace);
  const k = new Set(kernel);
  return {
    isKnownWorkspaceClass: (n) => ws.has(n),
    isKernelClass: (n) => k.has(n),
    namespaceOf: (n) => ns[n],
  };
}

const files = (m: Record<string, string>) => Object.entries(m).map(([uri, text]) => ({ uri, text }));

/** Apply a WorkspaceEdit's per-file edits to the originals (offset-descending). */
function applied(originals: Record<string, string>, changes: Record<string, { range: { start: { line: number; character: number }; end: { line: number; character: number } }; newText: string }[]>): Record<string, string> {
  const offsetOf = (text: string, p: { line: number; character: number }): number => {
    let line = 0, i = 0;
    while (line < p.line && i < text.length) { if (text.charCodeAt(i) === 10) line += 1; i += 1; }
    return i + p.character;
  };
  const out: Record<string, string> = {};
  for (const [uri, text] of Object.entries(originals)) {
    const edits = [...(changes[uri] ?? [])].sort((a, b) => offsetOf(text, b.range.start) - offsetOf(text, a.range.start));
    let s = text;
    for (const e of edits) s = s.slice(0, offsetOf(text, e.range.start)) + e.newText + s.slice(offsetOf(text, e.range.end));
    out[uri] = s;
  }
  return out;
}

function prep(src: string, offset: number, w: ClassWorld, fs = files({ 'file:///a.st': src })) {
  const ast = parse(src).ast;
  return prepareRenameAt(ast, tokenize(src).tokens, buildSymbolTable(ast), offset, fs, w);
}

// ---------------------------------------------------------------------------
// prepareRename classification (AC1/AC3)
// ---------------------------------------------------------------------------
test('prepareRename accepts a workspace class reference', () => {
  const src = 'Object subclass: Shape [\n  make [ ^Shape new ] ]';
  const r = prep(src, at(src, 'Shape', 2), world(['Shape'], ['Object']));
  assert.ok(!isReject(r), 'a workspace class is renameable');
});

test('prepareRename rejects a kernel class with a reason', () => {
  const src = 'Object subclass: Shape [\n  make [ ^OrderedCollection new ] ]';
  const r = prep(src, at(src, 'OrderedCollection'), world(['Shape'], ['Object', 'OrderedCollection']));
  assert.ok(isReject(r) && /kernel/i.test(r.reject), 'a kernel class is rejected with a kernel reason');
});

test('prepareRename rejects an unknown capitalized global', () => {
  const src = 'Object subclass: Shape [\n  make [ ^SomethingUnknown new ] ]';
  const r = prep(src, at(src, 'SomethingUnknown'), world(['Shape'], ['Object']));
  assert.ok(isReject(r), 'an unknown global is not renameable');
});

test('prepareRename treats a local temp sharing a class name as a local (not a class)', () => {
  // `Shape` is also a workspace class, but here it is a local temporary — still
  // renameable, but as a LOCAL (the class resolver skips it; see below).
  const src = 'Object subclass: Foo [\n  bar [ | Shape | Shape := 1. ^Shape ] ]';
  const r = prep(src, at(src, 'Shape', 2), world(['Shape', 'Foo'], ['Object']));
  assert.ok(!isReject(r), 'a local named like a class is still renameable as a local');
});

test('prepareRename accepts a qualified class segment (Smalltalk.Shape)', () => {
  const src = 'Object subclass: Foo [\n  make [ ^Smalltalk.Shape new ] ]';
  const r = prep(src, at(src, 'Shape', 1), world(['Shape', 'Foo'], ['Object']));
  assert.ok(!isReject(r), 'the class segment of a qualified name is renameable');
});

// ---------------------------------------------------------------------------
// classOccurrences — every reference form (AC2)
// ---------------------------------------------------------------------------
test('classOccurrences finds the declaration (bare-id subclass) + receiver + superclass', () => {
  const src = [
    'Object subclass: Shape [ ]',       // declaration (Variable arg `Shape`)
    'Shape extend [ describe [ ^1 ] ]', // extend receiver
    'Shape subclass: Circle [ ]',       // superclass position
    'Shape class extend [ d [ ^Shape new ] ]', // class receiver + `Shape new`
  ].join('\n');
  const occ = classOccurrences('Shape', world(['Shape', 'Circle'], ['Object']), files({ 'file:///a.st': src }));
  const ranges = occ.get('file:///a.st') ?? [];
  // 5 Shape references; `Object`/`Circle`/`describe` untouched.
  assert.equal(ranges.length, 5, `expected 5 Shape occurrences, got ${ranges.length}`);
});

test('classOccurrences rewrites #{Foo} and Smalltalk.Foo class segments only', () => {
  const src = 'Object subclass: Painter [\n  a [ ^#{Shape} new ]\n  b [ ^Smalltalk.Shape new ] ]';
  const changes: Record<string, { range: { start: { line: number; character: number }; end: { line: number; character: number } }; newText: string }[]> = {};
  const occ = classOccurrences('Shape', world(['Shape', 'Painter'], ['Object']), files({ 'file:///a.st': src }));
  changes['file:///a.st'] = (occ.get('file:///a.st') ?? []).map((r) => ({ range: { start: r.startPos, end: r.endPos }, newText: 'Polygon' }));
  const out = applied({ 'file:///a.st': src }, changes)['file:///a.st'];
  assert.match(out, /#\{Polygon\}/, 'binding-constant class segment rewritten, #{} preserved');
  assert.match(out, /Smalltalk\.Polygon/, 'qualified class segment rewritten, Smalltalk. preserved');
});

test('classOccurrences skips a local temp shadowing the class name', () => {
  const src = 'Shape extend [\n  demo [ | Shape | Shape := 1. ^Shape ] ]';
  const occ = classOccurrences('Shape', world(['Shape'], []), files({ 'file:///a.st': src }));
  const ranges = occ.get('file:///a.st') ?? [];
  // Only the `Shape extend` receiver — the three local occurrences are skipped.
  assert.equal(ranges.length, 1, `expected only the extend receiver, got ${ranges.length}`);
});

test('classOccurrences leaves an unrelated-namespace same-name class untouched', () => {
  // Target Shape is top-level (namespace undefined); `App::Shape` is a different class.
  const src = 'Object subclass: Foo [\n  a [ ^Smalltalk.Shape new ]\n  b [ ^App::Shape new ] ]';
  const occ = classOccurrences('Shape', world(['Shape', 'Foo'], ['Object'], {}), files({ 'file:///a.st': src }));
  const ranges = occ.get('file:///a.st') ?? [];
  // Only Smalltalk.Shape resolves (default namespace); App::Shape does not.
  assert.equal(ranges.length, 1, `expected only the Smalltalk-qualified ref, got ${ranges.length}`);
});

test('classOccurrences never touches comments or strings', () => {
  const src = 'Object subclass: Shape [\n  "Shape is nice" a [ ^\'Shape here\' ] ]';
  const occ = classOccurrences('Shape', world(['Shape'], ['Object']), files({ 'file:///a.st': src }));
  const ranges = occ.get('file:///a.st') ?? [];
  assert.equal(ranges.length, 1, 'only the declaration identifier; comment/string Shape untouched');
});

test('classOccurrences spans multiple files', () => {
  const a = 'Object subclass: Shape [ ]';
  const b = 'Shape extend [ f [ ^Shape new ] ]';
  const occ = classOccurrences('Shape', world(['Shape'], ['Object']), files({ 'file:///a.st': a, 'file:///b.st': b }));
  assert.ok((occ.get('file:///a.st') ?? []).length === 1, 'declaration in file a');
  assert.equal((occ.get('file:///b.st') ?? []).length, 2, 'extend receiver + Shape new in file b');
});

// ---------------------------------------------------------------------------
// renameAt — class branch, validation, kernel boundary (AC2/AC4/AC5)
// ---------------------------------------------------------------------------
test('renameAt renames a class across files (AC2)', () => {
  const a = 'Object subclass: Shape [ ]';
  const b = 'Shape extend [ f [ ^Shape new ] ]';
  const fs = files({ 'file:///a.st': a, 'file:///b.st': b });
  const r = renameAt('file:///a.st', at(a, 'Shape'), 'Polygon', fs, world(['Shape'], ['Object']));
  assert.ok(!isReject(r), 'class rename produces an edit');
  const out = applied({ 'file:///a.st': a, 'file:///b.st': b }, (r as { changes: Record<string, any[]> }).changes);
  assert.equal(out['file:///a.st'], 'Object subclass: Polygon [ ]');
  assert.equal(out['file:///b.st'], 'Polygon extend [ f [ ^Polygon new ] ]');
});

test('renameAt refuses a new name colliding with a kernel class (AC4)', () => {
  const a = 'Object subclass: Shape [ ]';
  const r = renameAt('file:///a.st', at(a, 'Shape'), 'OrderedCollection', files({ 'file:///a.st': a }), world(['Shape'], ['Object', 'OrderedCollection']));
  assert.ok(isReject(r) && /kernel|exist|collid/i.test(r.reject), 'kernel collision refused');
});

test('renameAt refuses a new name colliding with a workspace class (AC4)', () => {
  const a = 'Object subclass: Shape [ ]\nObject subclass: Circle [ ]';
  const r = renameAt('file:///a.st', at(a, 'Shape'), 'Circle', files({ 'file:///a.st': a }), world(['Shape', 'Circle'], ['Object']));
  assert.ok(isReject(r) && /exist|collid|workspace/i.test(r.reject), 'workspace collision refused');
});

test('renameAt refuses a non-class (lower-case) identifier (AC4)', () => {
  const a = 'Object subclass: Shape [ ]';
  const r = renameAt('file:///a.st', at(a, 'Shape'), 'polygon', files({ 'file:///a.st': a }), world(['Shape'], ['Object']));
  assert.ok(isReject(r) && /identifier|valid|class/i.test(r.reject), 'a lower-case class name is refused');
});

test('renameAt on a class is idempotent — rename then rename back is identity (AC5)', () => {
  const a = 'Object subclass: Shape [ ]\nShape extend [ f [ ^Shape new ] ]';
  const fs1 = files({ 'file:///a.st': a });
  const r1 = renameAt('file:///a.st', at(a, 'Shape'), 'Polygon', fs1, world(['Shape'], ['Object']));
  const mid = applied({ 'file:///a.st': a }, (r1 as { changes: Record<string, any[]> }).changes)['file:///a.st'];
  const fs2 = files({ 'file:///a.st': mid });
  const r2 = renameAt('file:///a.st', at(mid, 'Polygon'), 'Shape', fs2, world(['Polygon'], ['Object']));
  const back = applied({ 'file:///a.st': mid }, (r2 as { changes: Record<string, any[]> }).changes)['file:///a.st'];
  assert.equal(back, a, 'round-trips to the original text');
});

console.log(`\nclassRename.test: ${passed} passed.`);
