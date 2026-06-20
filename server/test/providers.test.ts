// Unit tests for the LSP provider mappings (US-412). Runs in Node via tsx —
// uses only vscode-languageserver-types (no server runtime, no VS Code).
import assert from 'node:assert/strict';
import { SymbolKind as LspSymbolKind, type DocumentSymbol } from 'vscode-languageserver-types';
import { parse } from '../src/parser/parser.ts';
import { buildSymbolTable } from '../src/parser/symbols.ts';
import { toDocumentSymbols } from '../src/providers/documentSymbol.ts';

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

console.log(`providers: ${passed} tests passed.`);
