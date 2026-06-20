// Unit + snapshot tests for the symbol table (US-411, slice 4 / AC5).
// Runs in Node via tsx.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { parse } from '../src/parser/parser.ts';
import { buildSymbolTable, SymbolKind, type SymbolNode } from '../src/parser/symbols.ts';

const UPDATE = process.argv.includes('--update');
const ROOT = process.cwd();
const FIXTURE_DIR = path.join(ROOT, 'docs/research/gst-syntax/test-cases');
const SNAPSHOT_DIR = path.join(ROOT, 'server/test/__snapshots__');

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

const symbols = (src: string): SymbolNode[] => buildSymbolTable(parse(src).ast);
const find = (list: SymbolNode[], name: string): SymbolNode | undefined => list.find((s) => s.name === name);
const childrenOfKind = (s: SymbolNode, kind: SymbolKind): SymbolNode[] => s.children.filter((c) => c.kind === kind);

// --- Classes, methods, variables (AC5) --------------------------------------
test('class records superclass, instance variables, and methods with arity', () => {
  const top = symbols('Object subclass: Foo [ | a b | bar [ ^1 ] Foo class >> make: x [ ^x ] ]');
  const foo = find(top, 'Foo');
  assert.ok(foo, 'expected class Foo');
  assert.equal(foo?.kind, SymbolKind.Class);
  assert.equal(foo?.detail, 'Object');
  assert.deepEqual(childrenOfKind(foo as SymbolNode, SymbolKind.InstanceVariable).map((v) => v.name), ['a', 'b']);
  const methods = childrenOfKind(foo as SymbolNode, SymbolKind.Method);
  const bar = methods.find((m) => m.selector === 'bar');
  assert.equal(bar?.arity, 0);
  assert.equal(bar?.classSide, false);
  const make = methods.find((m) => m.selector === 'make:');
  assert.equal(make?.arity, 1);
  assert.equal(make?.classSide, true);
});

test('definitions of the same class merge into one symbol', () => {
  const top = symbols('Object subclass: Foo [ a [ ^1 ] ] Foo >> b [ ^2 ] Foo extend [ c [ ^3 ] ]');
  assert.equal(top.filter((s) => s.name === 'Foo').length, 1);
  const foo = find(top, 'Foo') as SymbolNode;
  assert.deepEqual(
    childrenOfKind(foo, SymbolKind.Method).map((m) => m.selector).sort(),
    ['a', 'b', 'c'],
  );
});

test('namespace contains its classes', () => {
  const top = symbols('Namespace current: Kernel [ Object subclass: Bar [ go [ ^1 ] ] ]');
  const ns = find(top, 'Kernel');
  assert.equal(ns?.kind, SymbolKind.Namespace);
  const bar = find((ns as SymbolNode).children, 'Bar');
  assert.equal(bar?.kind, SymbolKind.Class);
  assert.equal(childrenOfKind(bar as SymbolNode, SymbolKind.Method)[0]?.selector, 'go');
});

test('class-side scope yields class variables and class-side methods', () => {
  const top = symbols('Object subclass: C [ C class [ Total := 0 ] C class >> util [ ^Total ] ]');
  const c = find(top, 'C') as SymbolNode;
  assert.deepEqual(childrenOfKind(c, SymbolKind.ClassVariable).map((v) => v.name), ['Total']);
  assert.equal(childrenOfKind(c, SymbolKind.Method).find((m) => m.selector === 'util')?.classSide, true);
});

test('methodsFor: chunk methods are recorded on their class', () => {
  const top = symbols("Foo methodsFor: 'cat'! bar ^1 ! baz: x ^x ! !");
  const foo = find(top, 'Foo') as SymbolNode;
  assert.deepEqual(childrenOfKind(foo, SymbolKind.Method).map((m) => m.selector), ['bar', 'baz:']);
});

test('method temporaries and parameters are recorded under the method', () => {
  const top = symbols('Object subclass: Foo [ run: x [ | t | t := x. ^t ] ]');
  const run = childrenOfKind(find(top, 'Foo') as SymbolNode, SymbolKind.Method)[0] as SymbolNode;
  assert.deepEqual(run.children.map((c) => c.name), ['x', 't']);
  assert.ok(run.children.every((c) => c.kind === SymbolKind.Temporary));
});

// --- Snapshot over a brace-format fixture ------------------------------------
function dumpSymbols(list: SymbolNode[], indent = ''): string[] {
  const out: string[] = [];
  for (const s of list) {
    const extra = s.detail ? ` (${s.detail})` : '';
    out.push(`${indent}${s.kind} ${s.name}${extra} @${s.range.start}..${s.range.end}`);
    out.push(...dumpSymbols(s.children, indent + '  '));
  }
  return out;
}

for (const name of ['12_gst_specific_extend_scoped_methods']) {
  test(`symbol snapshot: ${name}`, () => {
    const src = fs.readFileSync(path.join(FIXTURE_DIR, `${name}.st`), 'utf8');
    const actual = dumpSymbols(symbols(src)).join('\n') + '\n';
    const snapPath = path.join(SNAPSHOT_DIR, `${name}.symbols.txt`);
    if (UPDATE) {
      fs.mkdirSync(SNAPSHOT_DIR, { recursive: true });
      fs.writeFileSync(snapPath, actual);
      console.log(`    (updated ${path.relative(ROOT, snapPath)})`);
      return;
    }
    if (!fs.existsSync(snapPath)) {
      throw new Error(`Missing snapshot ${path.relative(ROOT, snapPath)}; run: npm run test:parser -- --update`);
    }
    assert.equal(actual, fs.readFileSync(snapPath, 'utf8'), `symbol snapshot drift for ${name}`);
  });
}

console.log(`symbols: ${passed} tests passed.`);
