// Unit tests for the hover provider (US-415). Runs in Node via tsx — uses only
// vscode-languageserver-types (no server runtime, no VS Code). Written BEFORE the
// provider (Acceptance Harness phase): RED until server/src/providers/hover.ts ships.
//
// Pins user-observable hover CONTENT per symbol kind (US-414 lesson: assert on the
// rendered strings, not just shape).
import assert from 'node:assert/strict';
import { MarkupKind, type Hover, type MarkupContent } from 'vscode-languageserver-types';
import { parse } from '../src/parser/parser.ts';
import { tokenize } from '../src/parser/lexer.ts';
import { buildSymbolTable } from '../src/parser/symbols.ts';
import { Provenance } from '../src/kernel/model.ts';
import { hoverAt, type HoverContext } from '../src/providers/hover.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

/** A tiny kernel-ish context: a superclass map + per-selector implementors. */
function ctxFrom(
  supers: Record<string, string | undefined>,
  implementors: Record<string, { className: string; provenance: Provenance; comment?: string }[]>,
): HoverContext {
  return {
    isClass: (name) => name in supers,
    superclassOf: (name) => supers[name],
    implementorsOf: (selector) => implementors[selector] ?? [],
  };
}

/** Run hoverAt over `text` with the cursor at the first `▮`, return the markdown. */
function md(text: string, ctx: HoverContext): string {
  const offset = text.indexOf('▮');
  assert.ok(offset >= 0, 'mark the cursor with ▮');
  const src = text.replace('▮', '');
  const ast = parse(src).ast;
  const hover = hoverAt(offset, src, tokenize(src).tokens, ast, buildSymbolTable(ast), ctx);
  assert.ok(hover, `expected a hover for: ${text}`);
  const c = (hover as Hover).contents as MarkupContent;
  assert.equal(c.kind, MarkupKind.Markdown, 'hover must be Markdown (AC5)');
  return c.value;
}

const KERNEL = ctxFrom(
  {
    Object: undefined,
    Collection: 'Iterable',
    Iterable: 'Object',
    SequenceableCollection: 'Collection',
    OrderedCollection: 'SequenceableCollection',
  },
  {
    printString: [{ className: 'Object', provenance: Provenance.BundledKernel }],
    'at:put:': [{ className: 'Dictionary', provenance: Provenance.BundledKernel }],
  },
);

// --- AC1: selector signature + implementors ---
test('AC1 unary selector shows signature + implementor', () => {
  const out = md('x printStr▮ing', KERNEL);
  assert.match(out, /printString/, 'names the selector');
  assert.match(out, /Object/, 'lists the implementor');
  assert.match(out, /```/, 'has a code fence (AC5)');
});

test('AC1 keyword selector renders a keyword signature', () => {
  const out = md('d at: 1 put▮: 2', KERNEL);
  assert.match(out, /at:/, 'keyword part at:');
  assert.match(out, /put:/, 'keyword part put:');
  assert.match(out, /Dictionary/, 'lists the implementor');
});

test('AC1 facts-only kernel carries NO comment prose', () => {
  const out = md('x printStr▮ing', KERNEL);
  assert.ok(!/comment/i.test(out), 'bundled/reference selector hover must not show prose');
});

test('AC1 installed-kernel comment IS shown (provenance allows)', () => {
  const ctx = ctxFrom(
    { Object: undefined },
    { printString: [{ className: 'Object', provenance: Provenance.InstalledKernel, comment: 'Answer a String.' }] },
  );
  const out = md('x printStr▮ing', ctx);
  assert.match(out, /Answer a String\./, 'installed-kernel method comment is surfaced');
});

// --- AC2: class superclass chain ---
test('AC2 class shows its full superclass chain', () => {
  const out = md('Ordered▮Collection', KERNEL);
  assert.match(out, /OrderedCollection/, 'chain starts at the class');
  assert.match(out, /SequenceableCollection/, 'includes the immediate superclass');
  assert.match(out, /Collection/, 'includes a transitive ancestor');
  assert.match(out, /Object/, 'reaches the root');
  assert.match(out, /→/, 'rendered as a chain');
});

test('AC2 chain stops at the dialect root (nil sentinel is not displayed)', () => {
  // The installed `.st` source defines `nil subclass: Object`, so Object's
  // superclass is the string "nil" — the chain must end at Object.
  const ctx = ctxFrom({ Object: 'nil', Foo: 'Object' }, {});
  const out = md('F▮oo', ctx);
  assert.match(out, /Foo → Object/, 'chain reaches Object');
  assert.ok(!/nil/.test(out), 'the nil sentinel must not appear in the chain');
});

// --- AC3: variable kind + declaration site ---
test('AC3 instance variable shows kind + declaration site', () => {
  const out = md('Object subclass: Foo [ | count | bar [ ^cou▮nt ] ]', KERNEL);
  assert.match(out, /instance variable/i, 'reports the kind');
  assert.match(out, /line/i, 'reports a declaration site');
});

test('AC3 block parameter is reported as a parameter', () => {
  const out = md('Object subclass: Foo [ bar: x [ ^x▮ ] ]', KERNEL);
  assert.match(out, /parameter/i, 'reports the parameter kind');
});

// --- AC4: numeric literal decode ---
test('AC4 radix integer decodes to decimal', () => {
  assert.match(md('16rF▮F', KERNEL), /255/, '16rFF = 255');
  assert.match(md('2r101▮0', KERNEL), /10\b/, '2r1010 = 10');
});

test('AC4 scaled decimal shows its value and scale', () => {
  const out = md('3.14s▮2', KERNEL);
  assert.match(out, /3\.14/, 'shows the value');
  assert.match(out, /scale|s2/i, 'notes the scale');
});

// --- AC5 / miss handling ---
test('hover returns null when nothing is under the cursor', () => {
  const src = '  ';
  const ast = parse(src).ast;
  assert.equal(hoverAt(0, src, tokenize(src).tokens, ast, buildSymbolTable(ast), KERNEL), null);
});

console.log(`hover.test: ${passed} passed`);
