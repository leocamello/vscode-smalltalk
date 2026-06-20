// Unit + snapshot tests for the GNU Smalltalk lexer (US-411, slice 1).
// Runs in Node via tsx — no VS Code required. Same idiom as
// client/test/gstLocator.test.ts.
//
//   npm run test:parser                # run
//   npm run test:parser -- --update    # regenerate the token snapshots
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { tokenize } from '../src/parser/lexer.ts';
import { TokenKind, type Token } from '../src/parser/token.ts';

const UPDATE = process.argv.includes('--update');
const ROOT = process.cwd(); // npm runs scripts from the repo root
const FIXTURE_DIR = path.join(ROOT, 'docs/research/gst-syntax/test-cases');
const SNAPSHOT_DIR = path.join(ROOT, 'server/test/__snapshots__');

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

/** Tokens with the trailing EOF stripped, for compact assertions. */
function lex(src: string): Token[] {
  const { tokens } = tokenize(src);
  assert.equal(tokens.at(-1)?.kind, TokenKind.EOF, 'token stream must end with EOF');
  return tokens.slice(0, -1);
}
const kinds = (src: string): TokenKind[] => lex(src).map((t) => t.kind);
/** Assert the single (non-EOF) token's kind and exact text. */
function one(src: string, kind: TokenKind, text = src): void {
  const toks = lex(src);
  assert.equal(toks.length, 1, `expected one token for ${JSON.stringify(src)}, got ${toks.length}`);
  assert.equal(toks[0]?.kind, kind);
  assert.equal(toks[0]?.text, text);
}

// --- Numbers (AC1.3) ---------------------------------------------------------
test('integers, radix, and underscores', () => {
  one('123', TokenKind.Integer);
  one('0', TokenKind.Integer);
  one('1_000_000', TokenKind.Integer);
  one('16rFF', TokenKind.Integer);
  one('16rff', TokenKind.Integer);
  one('2r1011', TokenKind.Integer);
  one('36rSMALLTALK', TokenKind.Integer);
  one('16rCAFE_BABE', TokenKind.Integer);
});

test('leading minus is a binary selector, not part of the number', () => {
  assert.deepEqual(kinds('-456'), [TokenKind.BinarySelector, TokenKind.Integer]);
  assert.deepEqual(lex('-456').map((t) => t.text), ['-', '456']);
});

test('floats incl. radix and exponent suffixes', () => {
  one('1.0', TokenKind.Float);
  one('123.456', TokenKind.Float);
  one('1.23456789d', TokenKind.Float);
  one('12.34E5', TokenKind.Float);
  one('1.0q2', TokenKind.Float);
  one('16rF.Fd', TokenKind.Float);
  one('123e2', TokenKind.Float);
  one('123e', TokenKind.Float); // bare suffix, no exponent digits
  one('1.0e2', TokenKind.Float);
  assert.deepEqual(lex('1.0e-2').map((t) => t.text), ['1.0e-2']); // exponent sign consumed
});

test('the tricky boundaries: 123e. and 1. keep the period separate', () => {
  assert.deepEqual(kinds('123e.'), [TokenKind.Float, TokenKind.Period]);
  assert.deepEqual(lex('123e.').map((t) => t.text), ['123e', '.']);
  assert.deepEqual(kinds('1.'), [TokenKind.Integer, TokenKind.Period]);
  assert.deepEqual(kinds('0.'), [TokenKind.Integer, TokenKind.Period]);
});

test('scaled decimals', () => {
  one('1s', TokenKind.ScaledDecimal);
  one('1.2s2', TokenKind.ScaledDecimal);
  one('123.456s', TokenKind.ScaledDecimal);
  one('16rA.Bs2', TokenKind.ScaledDecimal);
});

// --- Strings (AC1.4) ---------------------------------------------------------
test('strings with doubled-quote escape', () => {
  one("'hello'", TokenKind.String);
  one("''", TokenKind.String); // empty
  one("'a''b'", TokenKind.String); // '' is one escaped quote, not a close+reopen
});

test('multi-line string is a single token', () => {
  one("'multi\nline'", TokenKind.String);
});

// --- Chars (AC1.5) -----------------------------------------------------------
test('character literals incl. code points', () => {
  one('$a', TokenKind.Char);
  one('$$', TokenKind.Char);
  one('$Z', TokenKind.Char);
  one('$[', TokenKind.Char);
  one('$<65>', TokenKind.Char);
  one('$<16r42>', TokenKind.Char);
});

test('$< followed by a non-digit is the character "<"', () => {
  assert.deepEqual(kinds('$<.'), [TokenKind.Char, TokenKind.Period]);
  assert.deepEqual(lex('$<.').map((t) => t.text), ['$<', '.']);
});

// --- Symbols (AC1.4) ---------------------------------------------------------
test('symbol forms: identifier, keyword, quoted, binary', () => {
  one('#foo', TokenKind.Symbol);
  one('#at:put:', TokenKind.Symbol);
  one('#value:value:', TokenKind.Symbol);
  one("#'a string symbol'", TokenKind.Symbol);
  one("#''", TokenKind.Symbol);
  one('#+', TokenKind.Symbol);
  one('#<=', TokenKind.Symbol);
  one('#--', TokenKind.Symbol);
  one('#|', TokenKind.Symbol);
});

// --- Identifiers, keywords, scope, assignment (AC1.1, AC1.6) -----------------
test('identifiers and pseudo-variables are Identifier', () => {
  one('foo', TokenKind.Identifier);
  one('with_underscore', TokenKind.Identifier);
  one('self', TokenKind.Identifier);
  one('super', TokenKind.Identifier);
  one('thisContext', TokenKind.Identifier);
});

test('keyword vs assignment vs scope at a colon', () => {
  one('ifTrue:', TokenKind.Keyword);
  assert.deepEqual(kinds('x:=1'), [TokenKind.Identifier, TokenKind.Assign, TokenKind.Integer]);
  assert.deepEqual(lex('x:=1').map((t) => t.text), ['x', ':=', '1']);
  assert.deepEqual(kinds('Smalltalk::Foo'), [TokenKind.Identifier, TokenKind.Scope, TokenKind.Identifier]);
  one('_', TokenKind.Assign); // legacy assignment
  assert.deepEqual(kinds('a := b'), [TokenKind.Identifier, TokenKind.Assign, TokenKind.Identifier]);
});

// --- Operators & structural tokens (AC1.6) ----------------------------------
test('literal-collection starts are distinct combined tokens', () => {
  assert.deepEqual(kinds('#(1 2)'), [
    TokenKind.HashParen,
    TokenKind.Integer,
    TokenKind.Integer,
    TokenKind.RParen,
  ]);
  assert.deepEqual(kinds('#[1 2]'), [
    TokenKind.HashBracket,
    TokenKind.Integer,
    TokenKind.Integer,
    TokenKind.RBracket,
  ]);
  assert.deepEqual(kinds('#{Foo}'), [TokenKind.HashBrace, TokenKind.Identifier, TokenKind.RBrace]);
  assert.deepEqual(kinds('{1. 2}'), [
    TokenKind.LBrace,
    TokenKind.Integer,
    TokenKind.Period,
    TokenKind.Integer,
    TokenKind.RBrace,
  ]);
});

test('binary selectors run; a lone pipe is its own token', () => {
  one('>>', TokenKind.BinarySelector);
  one('->', TokenKind.BinarySelector);
  one('<=', TokenKind.BinarySelector);
  one('|', TokenKind.Pipe);
  assert.deepEqual(kinds('||'), [TokenKind.Pipe, TokenKind.Pipe]);
  assert.deepEqual(kinds('|x|'), [TokenKind.Pipe, TokenKind.Identifier, TokenKind.Pipe]);
  one('^', TokenKind.Caret);
  one(';', TokenKind.Semicolon);
});

// --- Comments (AC1.7) --------------------------------------------------------
test('comments are trivia tokens; "" is an escaped quote', () => {
  one('"a comment"', TokenKind.Comment);
  one('"a ""quoted"" word"', TokenKind.Comment); // single token, not two comments
  one('"multi\nline"', TokenKind.Comment);
});

// --- Shebang & bang (AC1.1, AC1.6) ------------------------------------------
test('shebang only at file start; bang otherwise', () => {
  const toks = lex('#! /usr/bin/env gst -f\n42');
  assert.equal(toks[0]?.kind, TokenKind.Shebang);
  assert.equal(toks[0]?.text, '#! /usr/bin/env gst -f');
  assert.equal(toks[1]?.kind, TokenKind.Integer);
  one('!', TokenKind.Bang);
  // Mid-file `#!` is not a shebang: it is a bare Hash then Bang.
  assert.deepEqual(kinds('a#!'), [TokenKind.Identifier, TokenKind.Hash, TokenKind.Bang]);
});

// --- Error tolerance (AC1.1, AC1.7) -----------------------------------------
test('never throws; always ends in EOF; emits diagnostics not exceptions', () => {
  const empty = tokenize('');
  assert.deepEqual(empty.tokens.map((t) => t.kind), [TokenKind.EOF]);
  assert.equal(empty.diagnostics.length, 0);

  const stray = tokenize('`');
  assert.equal(stray.tokens[0]?.kind, TokenKind.Error);
  assert.equal(stray.tokens.at(-1)?.kind, TokenKind.EOF);
  assert.equal(stray.diagnostics.length, 1);
});

test('unterminated string and comment recover with a diagnostic', () => {
  const s = tokenize("'no end");
  assert.equal(s.tokens[0]?.kind, TokenKind.String);
  assert.equal(s.tokens[0]?.text, "'no end");
  assert.equal(s.tokens.at(-1)?.kind, TokenKind.EOF);
  assert.equal(s.diagnostics.length, 1);

  const c = tokenize('"no end');
  assert.equal(c.tokens[0]?.kind, TokenKind.Comment);
  assert.equal(c.diagnostics.length, 1);
});

test('garbage input terminates and never throws', () => {
  let garbage = '';
  for (let i = 0; i < 512; i++) {
    garbage += String.fromCharCode((i * 37 + 1) % 256);
  }
  const result = tokenize(garbage);
  assert.equal(result.tokens.at(-1)?.kind, TokenKind.EOF);
  // Cursor reached the end: the last real token (or EOF) ends at source length.
  assert.equal(result.tokens.at(-1)?.end, garbage.length);
});

// --- Positions (AC1.2) -------------------------------------------------------
test('positions track CRLF newlines and UTF-16 characters', () => {
  const crlf = lex('1\r\n2');
  assert.deepEqual(crlf[0]?.startPos, { line: 0, character: 0 });
  assert.deepEqual(crlf[1]?.startPos, { line: 1, character: 0 });

  // After "'é' " the next token starts at character 4 (offsets are UTF-16 units).
  const toks = lex("'é' x");
  const x = toks.find((t) => t.text === 'x');
  assert.ok(x, 'expected an identifier x');
  assert.deepEqual(x?.startPos, { line: 0, character: 4 });
  assert.equal(x?.start, 4);
});

// --- Snapshots over the lexer-relevant fixtures (AC1) ------------------------
const SNAPSHOT_FIXTURES = [
  '01_literals_numbers',
  '02_literals_strings_chars_symbols',
  '03_literals_arrays',
  '04_comments',
  '05_core_identifiers_pseudo_vars',
  '15_edge_cases_shebang_whitespace',
];

function serialize(src: string): string {
  const { tokens, diagnostics } = tokenize(src);
  const lines = tokens.map(
    (t) => `${t.kind} ${JSON.stringify(t.text)} @${t.start}..${t.end} (${t.startPos.line}:${t.startPos.character})`,
  );
  const diag = diagnostics.map(
    (d) => `${d.severity} ${JSON.stringify(d.message)} @${d.start}..${d.end}`,
  );
  return [...lines, '', '--- diagnostics ---', ...diag, ''].join('\n');
}

for (const name of SNAPSHOT_FIXTURES) {
  test(`snapshot: ${name}`, () => {
    const src = fs.readFileSync(path.join(FIXTURE_DIR, `${name}.st`), 'utf8');
    const actual = serialize(src);
    const snapPath = path.join(SNAPSHOT_DIR, `${name}.tokens.txt`);
    if (UPDATE) {
      fs.mkdirSync(SNAPSHOT_DIR, { recursive: true });
      fs.writeFileSync(snapPath, actual);
      console.log(`    (updated ${path.relative(ROOT, snapPath)})`);
      return;
    }
    if (!fs.existsSync(snapPath)) {
      throw new Error(`Missing snapshot ${path.relative(ROOT, snapPath)}; run: npm run test:parser -- --update`);
    }
    const expected = fs.readFileSync(snapPath, 'utf8');
    assert.equal(actual, expected, `token snapshot drift for ${name}; review, then: npm run test:parser -- --update`);
  });
}

console.log(`lexer: ${passed} tests passed.`);
