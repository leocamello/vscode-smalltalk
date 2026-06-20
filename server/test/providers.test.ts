// Unit tests for the LSP provider mappings (US-412). Runs in Node via tsx —
// uses only vscode-languageserver-types (no server runtime, no VS Code).
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { SymbolKind as LspSymbolKind, type DocumentSymbol } from 'vscode-languageserver-types';
import { parse } from '../src/parser/parser.ts';
import { buildSymbolTable, SymbolKind } from '../src/parser/symbols.ts';
import { toDocumentSymbols } from '../src/providers/documentSymbol.ts';
import { WorkspaceIndex, defaultExclude, excludeFromConfig } from '../src/providers/workspaceIndex.ts';
import { toWorkspaceSymbols } from '../src/providers/workspaceSymbol.ts';
import { findDefinitions, resolveDefinitionQuery } from '../src/providers/definition.ts';

const FIXTURE_DIR = path.join(process.cwd(), 'docs/research/gst-syntax/test-cases');

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

const outline = (src: string): DocumentSymbol[] => toDocumentSymbols(buildSymbolTable(parse(src).ast));
const find = (list: DocumentSymbol[], name: string): DocumentSymbol | undefined => list.find((s) => s.name === name);

function assertContained(s: DocumentSymbol): void {
  // LSP requires selectionRange ⊆ range.
  const within =
    (s.selectionRange.start.line > s.range.start.line ||
      (s.selectionRange.start.line === s.range.start.line &&
        s.selectionRange.start.character >= s.range.start.character)) &&
    (s.selectionRange.end.line < s.range.end.line ||
      (s.selectionRange.end.line === s.range.end.line &&
        s.selectionRange.end.character <= s.range.end.character));
  assert.ok(within, `selectionRange must be within range for ${s.name}`);
  s.children?.forEach(assertContained);
}

test('brace class maps to a class symbol with field/method children', () => {
  const top = outline('Object subclass: Foo [ | a b | bar [ ^1 ] Foo class >> make [ ^self ] ]');
  const foo = find(top, 'Foo');
  assert.ok(foo);
  assert.equal(foo?.kind, LspSymbolKind.Class);
  assert.equal(foo?.detail, 'Object');
  const kids = foo?.children ?? [];
  assert.deepEqual(kids.filter((c) => c.kind === LspSymbolKind.Field).map((c) => c.name), ['a', 'b']);
  assert.deepEqual(
    kids.filter((c) => c.kind === LspSymbolKind.Method).map((c) => c.name).sort(),
    ['bar', 'make'],
  );
});

test('method-local temporaries/parameters are not in the outline', () => {
  const top = outline('Object subclass: Foo [ run: x [ | t | t := x. ^t ] ]');
  const run = (find(top, 'Foo')?.children ?? []).find((c) => c.name === 'run:');
  assert.equal(run?.kind, LspSymbolKind.Method);
  assert.deepEqual(run?.children ?? [], []); // x and t are dropped
});

test('chunk-format methods appear under their class', () => {
  const top = outline("Foo methodsFor: 'cat'! bar ^1 ! baz: x ^x ! !");
  const foo = find(top, 'Foo');
  assert.deepEqual((foo?.children ?? []).map((c) => c.name), ['bar', 'baz:']);
});

test('namespace maps to a Namespace symbol containing its classes', () => {
  const top = outline('Namespace current: Kernel [ Object subclass: Bar [ go [ ^1 ] ] ]');
  const ns = find(top, 'Kernel');
  assert.equal(ns?.kind, LspSymbolKind.Namespace);
  assert.equal(find(ns?.children ?? [], 'Bar')?.kind, LspSymbolKind.Class);
});

test('selectionRange is always within range', () => {
  outline('Object subclass: Foo [ | a | bar [ ^1 ] ]').forEach(assertContained);
});

// --- Workspace index + workspace/symbol (AC2) -------------------------------
test('index.setFile + query finds classes and selectors', () => {
  const idx = new WorkspaceIndex();
  idx.setFile('file:///a.st', 'Object subclass: Foo [ bar [ ^1 ] at: k put: v [ ^k ] ]');
  assert.deepEqual(idx.query('Foo').map((e) => e.name), ['Foo']);
  const atput = idx.query('at:put:');
  assert.equal(atput[0]?.kind, SymbolKind.Method);
  assert.equal(atput[0]?.containerName, 'Foo'); // method's enclosing class
  assert.equal(idx.query('').length, 3); // Foo + bar + at:put:
});

test('indexFolder scans real .st files and records locations', () => {
  const idx = new WorkspaceIndex();
  idx.indexFolder(FIXTURE_DIR);
  assert.ok(idx.size >= 10, `expected the 15 fixtures indexed, got ${idx.size}`);
  const simple = idx.query('MySimpleClass'); // defined in fixture 11
  assert.ok(simple.length >= 1, 'expected MySimpleClass from fixture 11');
  assert.match(simple[0]?.uri ?? '', /^file:\/\/.*11_/);
  assert.ok((simple[0]?.range.start.line ?? -1) >= 0);
});

test('toWorkspaceSymbols maps kind, location, and container', () => {
  const idx = new WorkspaceIndex();
  idx.setFile('file:///a.st', 'Object subclass: Foo [ bar [ ^1 ] ]');
  const syms = toWorkspaceSymbols(idx.query(''));
  const foo = syms.find((s) => s.name === 'Foo');
  assert.equal(foo?.kind, LspSymbolKind.Class);
  const bar = syms.find((s) => s.name === 'bar');
  assert.equal(bar?.kind, LspSymbolKind.Method);
  assert.equal(bar?.containerName, 'Foo');
  assert.equal(bar?.location.uri, 'file:///a.st');
});

test('exclude predicates skip build/VCS dirs and files.exclude entries', () => {
  assert.equal(defaultExclude('/x/node_modules', 'node_modules', true), true);
  assert.equal(defaultExclude('/x/.git', '.git', true), true);
  assert.equal(defaultExclude('/x/src', 'src', true), false);
  const fromCfg = excludeFromConfig({ '**/vendored': true, '**/keepme': false });
  assert.equal(fromCfg('/x/vendored', 'vendored', true), true);
  assert.equal(fromCfg('/x/keepme', 'keepme', true), false);
});

// --- Go-to-definition (AC3) -------------------------------------------------
const at = (src: string, sub: string) => resolveDefinitionQuery(src, src.indexOf(sub) + 1);

test('resolveDefinitionQuery distinguishes class refs from message selectors', () => {
  assert.deepEqual(at('Array new', 'Array'), { target: 'class', name: 'Array' });
  assert.deepEqual(at('obj printNl', 'printNl'), { target: 'selector', name: 'printNl' });
  assert.deepEqual(at('a foo: b', 'foo:'), { target: 'selector', name: 'foo:' });
  assert.deepEqual(at('a foo: b', 'b'), { target: 'class', name: 'b' });
  assert.deepEqual(at('a foo: b', 'a'), { target: 'class', name: 'a' });
  // Innermost send wins.
  assert.deepEqual(at('a foo bar', 'bar'), { target: 'selector', name: 'bar' });
  assert.deepEqual(at('a foo bar', 'foo'), { target: 'selector', name: 'foo' });
});

test('findDefinitions returns class definitions and all selector implementors', () => {
  const idx = new WorkspaceIndex();
  idx.setFile('file:///a.st', 'Object subclass: Foo [ greet [ ^1 ] ]');
  idx.setFile('file:///b.st', 'Object subclass: Bar [ greet [ ^2 ] ]'); // another `greet` implementor

  const classQ = at('Foo new', 'Foo');
  assert.ok(classQ);
  const classLocs = findDefinitions(idx, classQ!, 'file:///other.st');
  assert.deepEqual(classLocs.map((l) => l.uri), ['file:///a.st']);

  const selQ = at('x greet', 'greet');
  assert.ok(selQ);
  const selLocs = findDefinitions(idx, selQ!, 'file:///b.st');
  assert.deepEqual(selLocs.map((l) => l.uri).sort(), ['file:///a.st', 'file:///b.st']);
  // Same-file first: querying from b.st puts b.st's implementor first.
  assert.equal(selLocs[0]?.uri, 'file:///b.st');
});

console.log(`providers: ${passed} tests passed.`);
