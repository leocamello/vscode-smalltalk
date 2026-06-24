// Unit tests for the semantic-tokens provider (US-422). Runs in Node via tsx —
// uses only the pure front end + the provider (no server runtime, no VS Code).
// Written BEFORE the provider (Acceptance Harness phase): RED until
// server/src/providers/semanticTokens.ts ships.
//
// Pins user-observable CLASSIFICATION per role (US-414 lesson: assert the
// observable result, not just shape). The one differentiator is AC2 — a
// capitalized name is `class` iff it resolves in workspace ∪ cartridge, else a
// global — and AC4, the capitalization fallback when no cartridge is loaded.
import assert from 'node:assert/strict';
import { parse } from '../src/parser/parser.ts';
import { tokenize } from '../src/parser/lexer.ts';
import { buildSymbolTable } from '../src/parser/symbols.ts';
import {
  collectSemanticTokens,
  encodeSemanticTokens,
  SEMANTIC_TOKEN_TYPES,
  SEMANTIC_TOKEN_MODIFIERS,
  type RawSemanticToken,
  type SemanticTokenContext,
} from '../src/providers/semanticTokens.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

/** A context whose known classes come from a fixed origin map; everything else
 *  is unknown. `hasCartridge` toggles the AC4 capitalization fallback. */
function ctxOf(
  classes: Record<string, 'workspace' | 'cartridge'>,
  hasCartridge = true,
): SemanticTokenContext {
  return { hasCartridge, classOrigin: (name) => classes[name] };
}

/** Byte offset → {line, character} (0-based, matching the token positions). */
function posAt(src: string, offset: number): { line: number; character: number } {
  let line = 0;
  let last = -1;
  for (let i = 0; i < offset; i++) {
    if (src[i] === '\n') {
      line += 1;
      last = i;
    }
  }
  return { line, character: offset - last - 1 };
}

/** Collect tokens for `src` under `ctx`. */
function tokensFor(src: string, ctx: SemanticTokenContext): RawSemanticToken[] {
  const ast = parse(src).ast;
  return collectSemanticTokens(ast, buildSymbolTable(ast), tokenize(src).tokens, ctx);
}

/** The semantic token whose span (on its line) covers the n-th occurrence of
 *  `needle` in `src`, or undefined. */
function tokenForNth(
  tokens: RawSemanticToken[],
  src: string,
  needle: string,
  occurrence = 0,
): RawSemanticToken | undefined {
  let from = -1;
  for (let i = 0; i <= occurrence; i++) {
    from = src.indexOf(needle, from + 1);
    if (from < 0) return undefined;
  }
  const p = posAt(src, from);
  return tokens.find(
    (t) => t.line === p.line && t.char <= p.character && p.character < t.char + t.length,
  );
}

// --- AC1: role classification from the AST + symbol scopes -------------------

test('AC1 instance variable is a property', () => {
  const src = 'Object subclass: Foo [ | count | bar [ ^count ] ]';
  const toks = tokensFor(src, ctxOf({ Object: 'cartridge', Foo: 'workspace' }));
  const ref = tokenForNth(toks, src, 'count', 1); // the use in `^count`
  assert.ok(ref, 'the ivar use is tokenized');
  assert.equal(ref.type, 'property', 'an instance variable is a property');
});

test('AC1 temporary is a variable', () => {
  const src = 'Object subclass: Foo [ bar [ | tmp | ^tmp ] ]';
  const toks = tokensFor(src, ctxOf({ Object: 'cartridge', Foo: 'workspace' }));
  const tok = tokenForNth(toks, src, 'tmp', 1); // the use in `^tmp`
  assert.ok(tok, 'the temp use is tokenized');
  assert.equal(tok.type, 'variable', 'a temporary is a variable');
});

test('AC1 method parameter is a parameter', () => {
  const src = 'Object subclass: Foo [ bar: x [ ^x ] ]';
  const toks = tokensFor(src, ctxOf({ Object: 'cartridge', Foo: 'workspace' }));
  const ref = tokenForNth(toks, src, 'x', 1); // the use in `^x`
  assert.ok(ref, 'the parameter use is tokenized');
  assert.equal(ref.type, 'parameter', 'a method argument is a parameter');
});

test('AC1 block parameter is a parameter', () => {
  const src = 'Object subclass: Foo [ bar [ ^[ :y | y ] ] ]';
  const toks = tokensFor(src, ctxOf({ Object: 'cartridge', Foo: 'workspace' }));
  const ref = tokenForNth(toks, src, 'y', 1); // the use inside the block body
  assert.ok(ref, 'the block parameter use is tokenized');
  assert.equal(ref.type, 'parameter', 'a block argument is a parameter');
});

// --- AC2: cartridge-driven known-class vs unknown-global --------------------

test('AC2 a known kernel class is a class, with defaultLibrary', () => {
  const src = 'OrderedCollection new';
  const toks = tokensFor(src, ctxOf({ OrderedCollection: 'cartridge' }));
  const tok = tokenForNth(toks, src, 'OrderedCollection');
  assert.ok(tok, 'the class reference is tokenized');
  assert.equal(tok.type, 'class', 'a known kernel class is a class (the cartridge consumer)');
  assert.ok(tok.modifiers.includes('defaultLibrary'), 'a cartridge class carries defaultLibrary');
});

test('AC2 a workspace class is a class, without defaultLibrary', () => {
  const src = 'Object subclass: Widget [ ]\nWidget new';
  const toks = tokensFor(src, ctxOf({ Object: 'cartridge', Widget: 'workspace' }));
  const tok = tokenForNth(toks, src, 'Widget', 1); // the use on line 2
  assert.ok(tok, 'the workspace class reference is tokenized');
  assert.equal(tok.type, 'class', 'a known workspace class is a class');
  assert.ok(!tok.modifiers.includes('defaultLibrary'), 'a workspace class is not defaultLibrary');
});

test('AC2 an unknown capitalized name is a global variable, not a class', () => {
  const src = 'Zork new';
  const toks = tokensFor(src, ctxOf({ OrderedCollection: 'cartridge' })); // Zork is unknown
  const tok = tokenForNth(toks, src, 'Zork');
  assert.ok(tok, 'the unknown global is tokenized');
  assert.equal(tok.type, 'variable', 'an unknown capitalized name is a global variable, not a class');
});

// --- AC3: keyword-message parts + pseudo-variables --------------------------

test('AC3 keyword-message parts are methods', () => {
  const src = 'd at: 1 put: 2';
  const toks = tokensFor(src, ctxOf({}));
  const at = tokenForNth(toks, src, 'at:');
  const put = tokenForNth(toks, src, 'put:');
  assert.ok(at && put, 'both keyword parts are tokenized');
  assert.equal(at.type, 'method', 'at: is a method (selector part)');
  assert.equal(put.type, 'method', 'put: is a method (selector part)');
});

test('AC3 pseudo-variables are keywords, distinct from selectors', () => {
  const src = 'Object subclass: Foo [ bar [ ^self ] baz [ ^super ] qux [ ^thisContext ] ]';
  const toks = tokensFor(src, ctxOf({ Object: 'cartridge', Foo: 'workspace' }));
  for (const pv of ['self', 'super', 'thisContext']) {
    const tok = tokenForNth(toks, src, pv);
    assert.ok(tok, `${pv} is tokenized`);
    assert.equal(tok.type, 'keyword', `${pv} is a pseudo-variable (keyword)`);
  }
});

test('AC3 nil/true/false are keywords', () => {
  const src = '{ nil. true. false }';
  const toks = tokensFor(src, ctxOf({}));
  for (const pv of ['nil', 'true', 'false']) {
    const tok = tokenForNth(toks, src, pv);
    assert.ok(tok, `${pv} is tokenized`);
    assert.equal(tok.type, 'keyword', `${pv} is a reserved literal (keyword)`);
  }
});

// --- AC4: no-cartridge capitalization fallback ------------------------------

test('AC4 with no cartridge, a capitalized name falls back to class', () => {
  const src = 'Zork new';
  const toks = tokensFor(src, ctxOf({}, /* hasCartridge */ false));
  const tok = tokenForNth(toks, src, 'Zork');
  assert.ok(tok, 'the capitalized name is tokenized');
  assert.equal(tok.type, 'class', 'with no cartridge, capitalized ⇒ class (AC4 fallback)');
});

test('AC4 empty input yields no tokens and never throws', () => {
  assert.deepEqual(tokensFor('   ', ctxOf({})), [], 'no tokens for whitespace-only input');
});

// --- Encoding: LSP delta-encoded integer stream -----------------------------

test('encodeSemanticTokens emits 5 integers per token, sorted + delta-encoded', () => {
  const src = 'Object subclass: Foo [ bar: x [ ^x ] ]';
  const raw = tokensFor(src, ctxOf({ Object: 'cartridge', Foo: 'workspace' }));
  const data = encodeSemanticTokens(raw);
  assert.equal(data.length % 5, 0, 'data is a flat run of 5-tuples');
  assert.equal(data.length / 5, raw.length, 'one 5-tuple per raw token');
  // First tuple: deltaLine is absolute for the first token (prev line 0).
  assert.ok(data[0] >= 0 && data[1] >= 0, 'first token has non-negative deltas');
  // Every type index is within the legend.
  for (let i = 3; i < data.length; i += 5) {
    assert.ok(data[i] >= 0 && data[i] < SEMANTIC_TOKEN_TYPES.length, 'type index within legend');
  }
});

test('legend modifiers include static + defaultLibrary', () => {
  assert.ok(SEMANTIC_TOKEN_MODIFIERS.includes('static'), 'static modifier (class variable)');
  assert.ok(SEMANTIC_TOKEN_MODIFIERS.includes('defaultLibrary'), 'defaultLibrary modifier (cartridge class)');
});

console.log(`semanticTokens.test: ${passed} passed`);
