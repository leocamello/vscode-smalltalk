// Unit tests for scope-aware rename (US-426). Runs in Node via tsx.
//
// Acceptance Harness — written BEFORE the provider: RED until
// server/src/providers/rename.ts (+ xref/ivarRefs.ts) ship. Covers:
//   - prepareRename accept (temp/arg/ivar) / reject (selector, class/kernel,
//     global, self/super, literal) — each rejection carries a reason (AC1),
//   - temp/arg rename rewrites exactly its in-scope occurrences (AC2),
//   - new-name validation: invalid identifier + collision/shadow refused (AC4),
//   - workspace-wide ivar rename across a split class, skipping a shadow (AC3),
//   - the edit never targets kernel/cartridge and stays within scope (AC5).
import assert from 'node:assert/strict';
import { parse } from '../src/parser/parser.ts';
import { tokenize } from '../src/parser/lexer.ts';
import { buildSymbolTable } from '../src/parser/symbols.ts';
import { prepareRenameAt, renameAt, withMultiFileConfirmation } from '../src/providers/rename.ts';

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

function prep(src: string, offset: number) {
  const ast = parse(src).ast;
  return prepareRenameAt(ast, tokenize(src).tokens, buildSymbolTable(ast), offset);
}

// --- prepareRename: accepts (AC1) ---
test('prepareRename accepts a temporary', () => {
  const src = 'Object subclass: Foo [\n  bar [ | acc | acc := 1. ^acc ] ]';
  assert.ok(!isReject(prep(src, at(src, 'acc', 2))), 'a temporary is renameable');
});

test('prepareRename accepts a method argument', () => {
  const src = 'Object subclass: Foo [\n  bar: x [ ^x + 1 ] ]';
  assert.ok(!isReject(prep(src, at(src, 'x', 2))), 'a method argument is renameable');
});

test('prepareRename accepts an instance variable', () => {
  const src = 'Object subclass: Foo [\n  | balance |\n  bal [ ^balance ] ]';
  assert.ok(!isReject(prep(src, at(src, 'balance', 2))), 'an instance variable is renameable');
});

// --- prepareRename: rejects with a reason (AC1) ---
test('prepareRename rejects a message selector', () => {
  const src = 'Object subclass: Foo [\n  bar [ ^self frobnicate ] ]';
  const r = prep(src, at(src, 'frobnicate'));
  assert.ok(isReject(r) && /selector/i.test(r.reject), 'selector rename is rejected with a reason');
});

test('prepareRename rejects a class / kernel global', () => {
  const src = 'Object subclass: Foo [\n  bar [ ^OrderedCollection new ] ]';
  assert.ok(isReject(prep(src, at(src, 'OrderedCollection'))), 'a class reference is not renameable');
});

test('prepareRename rejects a pseudo-variable (self)', () => {
  const src = 'Object subclass: Foo [\n  bar [ ^self ] ]';
  assert.ok(isReject(prep(src, at(src, 'self'))), 'self is not renameable');
});

test('prepareRename rejects a literal', () => {
  const src = 'Object subclass: Foo [\n  bar [ ^42 ] ]';
  assert.ok(isReject(prep(src, at(src, '42'))), 'a literal is not renameable');
});

test('prepareRename rejects an unresolved global', () => {
  const src = 'Object subclass: Foo [\n  bar [ ^undeclaredThing ] ]';
  assert.ok(isReject(prep(src, at(src, 'undeclaredThing'))), 'an unresolved name is not renameable');
});

// --- temp/arg rename: exact scope, no bleed (AC2) ---
const editsFor = (edit: { changes?: Record<string, unknown[]> }, uri: string): unknown[] =>
  edit.changes?.[uri] ?? [];

test('renaming a temporary rewrites exactly its method occurrences', () => {
  const uri = 'file:///t.st';
  const src = 'Object subclass: Foo [\n  bar [ | acc | acc := 1. ^acc ]\n  baz [ | acc | ^acc ] ]';
  const r = renameAt(uri, at(src, 'acc', 2), 'total', [{ uri, text: src }]);
  assert.ok(!isReject(r), 'rename should succeed');
  const edits = editsFor(r as { changes?: Record<string, unknown[]> }, uri);
  // bar's three `acc` (decl + write + read) only; baz's `acc` untouched.
  assert.equal(edits.length, 3, `expected 3 edits in bar, got ${edits.length}`);
});

// --- new-name validation (AC4) ---
test('rename refuses an invalid identifier', () => {
  const uri = 'file:///t.st';
  const src = 'Object subclass: Foo [\n  bar [ | acc | ^acc ] ]';
  const r = renameAt(uri, at(src, 'acc', 2), '1bad', [{ uri, text: src }]);
  assert.ok(isReject(r) && /identifier|valid/i.test(r.reject), 'invalid identifier refused');
});

test('rename refuses a colliding new name (no cross-symbol bleed)', () => {
  const uri = 'file:///t.st';
  const src = 'Object subclass: Foo [\n  bar [ | acc total | acc := total ] ]';
  const r = renameAt(uri, at(src, 'acc', 2), 'total', [{ uri, text: src }]);
  assert.ok(isReject(r) && /collid|exist|conflict/i.test(r.reject), 'collision refused');
});

// --- workspace-wide ivar rename (AC3) ---
test('renaming an instance variable edits every file that defines/extends the class', () => {
  const a = 'file:///account.st';
  const b = 'file:///account-extra.st';
  const srcA = 'Object subclass: Account [\n  | balance |\n  deposit: n [ balance := balance + n ] ]';
  const srcB = 'Account extend [\n  report [ ^balance ] ]';
  const r = renameAt(a, at(srcA, 'balance', 2), 'funds', [
    { uri: a, text: srcA },
    { uri: b, text: srcB },
  ]);
  assert.ok(!isReject(r), 'ivar rename should succeed');
  const changes = (r as { changes?: Record<string, unknown[]> }).changes ?? {};
  assert.ok(editsFor({ changes }, a).length >= 2, 'file A (decl + uses) edited');
  assert.ok(editsFor({ changes }, b).length >= 1, 'file B (extend method ref) edited');
});

test('ivar rename skips a method where a local temp shadows the name', () => {
  const a = 'file:///account.st';
  const b = 'file:///shadow.st';
  const srcA = 'Object subclass: Account [\n  | balance |\n  deposit: n [ balance := n ] ]';
  const srcB = 'Account extend [\n  weird [ | balance | ^balance ] ]';
  const r = renameAt(a, at(srcA, 'balance', 2), 'funds', [
    { uri: a, text: srcA },
    { uri: b, text: srcB },
  ]);
  assert.ok(!isReject(r), 'rename should succeed');
  const changes = (r as { changes?: Record<string, unknown[]> }).changes ?? {};
  assert.equal(editsFor({ changes }, b).length, 0, 'the shadowing method must NOT be edited');
});

// --- multi-file preview confirmation (needsConfirmation) ---
const te = () => ({ range: { start: { line: 0, character: 0 }, end: { line: 0, character: 1 } }, newText: 'x' });

test('a multi-file rename is annotated needsConfirmation (routes through Refactor Preview)', () => {
  const edit = { changes: { 'file:///a.st': [te()], 'file:///b.st': [te()] } };
  const r = withMultiFileConfirmation(edit, true, () => 1);
  assert.ok(r.documentChanges && !r.changes, 'multi-file → documentChanges, no plain changes');
  const ann = Object.values(r.changeAnnotations ?? {})[0] as { needsConfirmation?: boolean } | undefined;
  assert.equal(ann?.needsConfirmation, true, 'annotation requires confirmation');
});

test('a single-file rename stays instant (no confirmation)', () => {
  const edit = { changes: { 'file:///a.st': [te()] } };
  const r = withMultiFileConfirmation(edit, true, () => 1);
  assert.ok(r.changes && !r.documentChanges, 'single file is applied directly');
});

test('multi-file confirmation falls back to plain changes without client support', () => {
  const edit = { changes: { 'file:///a.st': [te()], 'file:///b.st': [te()] } };
  const r = withMultiFileConfirmation(edit, false, () => 1);
  assert.ok(r.changes && !r.documentChanges, 'unsupported client → plain changes');
});

console.log(`rename.test: ${passed} passed`);
