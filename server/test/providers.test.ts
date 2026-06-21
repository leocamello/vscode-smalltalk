// Unit tests for the LSP provider mappings (US-412). Runs in Node via tsx —
// uses only vscode-languageserver-types (no server runtime, no VS Code).
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import {
  CompletionItemKind,
  InsertTextFormat,
  SymbolKind as LspSymbolKind,
  type CompletionItem,
  type DocumentSymbol,
} from 'vscode-languageserver-types';
import { parse } from '../src/parser/parser.ts';
import { buildSymbolTable, SymbolKind } from '../src/parser/symbols.ts';
import { toDocumentSymbols } from '../src/providers/documentSymbol.ts';
import { WorkspaceIndex, defaultExclude, excludeFromConfig } from '../src/providers/workspaceIndex.ts';
import { toWorkspaceSymbols } from '../src/providers/workspaceSymbol.ts';
import { findDefinitions, resolveDefinitionQuery } from '../src/providers/definition.ts';
import { tokenize } from '../src/parser/lexer.ts';
import { toFoldingRanges } from '../src/providers/foldingRange.ts';
import { documentHighlightsAt } from '../src/providers/documentHighlight.ts';
import { completionsAt } from '../src/providers/completion.ts';
import { Provenance } from '../src/kernel/model.ts';

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

// --- Folding ranges (AC1) ---------------------------------------------------
const folds = (src: string) => toFoldingRanges(parse(src).ast, tokenize(src).tokens);
const hasFold = (
  ranges: ReturnType<typeof folds>,
  startLine: number,
  endLine: number,
  kind?: string,
) => ranges.some((r) => r.startLine === startLine && r.endLine === endLine && (kind === undefined || r.kind === kind));

test('folds class bodies and method bodies with correct line ranges', () => {
  const src = ['Object subclass: Foo [', '    greet [', '        ^1', '    ]', ']'].join('\n');
  const ranges = folds(src);
  assert.ok(hasFold(ranges, 0, 4), 'class Foo folds lines 0..4');
  assert.ok(hasFold(ranges, 1, 3), 'method greet folds lines 1..3');
});

test('folds a multi-line block and skips single-line constructs', () => {
  const block = ['x do: [ :each |', '    each printNl', ']'].join('\n');
  assert.ok(hasFold(folds(block), 0, 2), 'block folds 0..2');
  // A whole one-line method produces no fold (nothing to collapse).
  assert.equal(folds('Object subclass: Foo [ bar [ ^1 ] ]').length, 0);
});

test('folds multi-line comments as comment ranges', () => {
  const src = ['"', 'multi', 'line', '"', '1 + 1'].join('\n');
  assert.ok(hasFold(folds(src), 0, 3, 'comment'), 'comment folds 0..3 with comment kind');
  // A single-line comment does not fold.
  assert.equal(folds('"one line" 1 + 1').length, 0);
});

// --- Document highlight (AC2) -----------------------------------------------
const highlightsAt = (src: string, sub: string, occurrence = 0) => {
  let idx = -1;
  for (let i = 0; i <= occurrence; i++) {
    idx = src.indexOf(sub, idx + 1);
  }
  return documentHighlightsAt(parse(src).ast, tokenize(src).tokens, idx + 1);
};

test('highlight on a unary selector marks every send of it', () => {
  const hs = highlightsAt('obj printNl. zed printNl', 'printNl');
  assert.equal(hs.length, 2);
});

test('highlight on a keyword selector marks each part of every matching send', () => {
  const hs = highlightsAt('a at: 1 put: 2. b at: 3 put: 4', 'at:');
  assert.equal(hs.length, 4); // 2 sends × (at: + put:)
});

test('highlight on a variable is scoped to its method — no cross-scope bleed', () => {
  // method m and method n both declare `x`; highlighting m's x must not touch n's.
  const src = 'Object subclass: C [ m [ | x | ^x ] n [ | x | ^x ] ]';
  const hs = highlightsAt(src, '^x'); // the use in method m
  assert.equal(hs.length, 2); // the declaration + the `^x` use, in m only
});

test('highlight on a variable includes its declaration and assignment writes', () => {
  const src = 'Object subclass: C [ m [ | x y | x := 1. ^x + y ] ]';
  const hs = highlightsAt(src, '^x'); // a use of x
  // declaration x, assignment target x, and ^x — but not y.
  assert.equal(hs.length, 3);
});

// --- completion (US-413 slice C) -------------------------------------------
const complete = (
  src: string,
  offset: number,
  selectors: { selector: string; provenance: Provenance }[],
  classes: { name: string; provenance: Provenance }[],
): CompletionItem[] =>
  completionsAt(offset, src, tokenize(src).tokens, parse(src).ast, buildSymbolTable(parse(src).ast), selectors, classes);

test('completion: selector context after a receiver offers selectors + keyword snippet', () => {
  const src = 'x at';
  const items = complete(
    src,
    src.length,
    [
      { selector: 'at:put:', provenance: Provenance.BundledKernel },
      { selector: 'at:', provenance: Provenance.BundledKernel },
      { selector: 'add:', provenance: Provenance.Workspace }, // doesn't match "at"
    ],
    [],
  );
  const labels = items.map((i) => i.label);
  assert.ok(labels.includes('at:put:') && labels.includes('at:'), 'should offer at:put: and at:');
  assert.ok(!labels.includes('add:'), 'add: does not match prefix "at"');
  const atput = items.find((i) => i.label === 'at:put:');
  assert.equal(atput?.insertTextFormat, InsertTextFormat.Snippet);
  assert.equal(atput?.insertText, 'at:${1} put:${2}', 'multi-part keyword inserts as a snippet');
});

test('completion: workspace selectors rank above kernel selectors', () => {
  const src = 'x pr';
  const items = complete(
    src,
    src.length,
    [
      { selector: 'printString', provenance: Provenance.BundledKernel },
      { selector: 'printOn:', provenance: Provenance.Workspace },
    ],
    [],
  );
  const ws = items.find((i) => i.label === 'printOn:');
  const kn = items.find((i) => i.label === 'printString');
  assert.ok(ws && kn);
  assert.ok((ws.sortText ?? '') < (kn.sortText ?? ''), 'workspace selector sorts before kernel selector');
});

test('completion: head context offers in-scope variables (instance vars)', () => {
  const src = 'Object subclass: Foo [ | count | bar [ | total | ^c ] ]';
  const offset = src.indexOf('^c') + 2; // just after the `c`
  const items = complete(src, offset, [], [{ name: 'Collection', provenance: Provenance.Workspace }]);
  const count = items.find((i) => i.label === 'count');
  assert.ok(count, 'should offer the in-scope instance variable count');
  assert.equal(count?.detail, 'instanceVariable');
  assert.ok(!items.some((i) => i.label === 'total'), 'total does not match prefix "c"');
});

test('completion: head context offers class names; camel-hump matches', () => {
  const src = 'OC';
  const items = complete(src, src.length, [], [{ name: 'OrderedCollection', provenance: Provenance.Workspace }]);
  const oc = items.find((i) => i.label === 'OrderedCollection');
  assert.ok(oc, 'camel-hump prefix OC should match OrderedCollection');
  assert.equal(oc?.kind, CompletionItemKind.Class);
});

test('completion: variables rank above classes in head context', () => {
  const src = 'Object subclass: Foo [ bar [ | value | ^v ] ]';
  const offset = src.indexOf('^v') + 2;
  const items = complete(src, offset, [], [{ name: 'Value', provenance: Provenance.Workspace }]);
  const v = items.find((i) => i.label === 'value');
  const cls = items.find((i) => i.label === 'Value');
  assert.ok(v && cls);
  assert.ok((v.sortText ?? '') < (cls.sortText ?? ''), 'in-scope variable sorts before a class');
});

console.log(`providers: ${passed} tests passed.`);
