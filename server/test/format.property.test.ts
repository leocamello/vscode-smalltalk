// Property tests for the formatter (US-416, AC3 + AC5). Runs in Node via tsx.
//
// Acceptance Harness — written BEFORE the formatter: RED until
// server/src/format/formatter.ts ships. These pin the two hard guarantees that
// make formatting safe (the story's "data loss = trust loss" mandate, ADR-0005):
//
//   1. IDEMPOTENCE         — format(format(x)) === format(x)
//   2. TOKEN-STREAM INVARIANCE — the significant token stream (kind + text,
//                            comments included, EOF/whitespace excluded) is
//                            identical before and after formatting. No token may
//                            be added, dropped, split, or merged.
//   3. NON-DESTRUCTIVE ON ERROR — a file that does not parse cleanly is returned
//                            unchanged (the formatter never reformats around an
//                            Error node).
//
// Run over a hand-picked corpus AND, when present, the 122-file GNU Smalltalk
// kernel (outside the repo — skipped gracefully in CI, runs as a local gate).
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { tokenize } from '../src/parser/lexer.ts';
import { parse } from '../src/parser/parser.ts';
import { TokenKind } from '../src/parser/token.ts';
// RED: this module does not exist yet — implementation (Phase 3) creates it.
import { formatSource, DEFAULT_FORMAT_OPTIONS } from '../src/format/formatter.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

/** Significant tokens as (kind, text) pairs — comments kept, EOF dropped. */
function sigTokens(src: string): Array<{ kind: TokenKind; text: string }> {
  return tokenize(src)
    .tokens.filter((t) => t.kind !== TokenKind.EOF)
    .map((t) => ({ kind: t.kind, text: t.text }));
}

const fmt = (src: string): string => formatSource(src, DEFAULT_FORMAT_OPTIONS);
// The same invariants must hold under blockStyle=expand (structural forced breaks).
const EXPAND_OPTIONS = { ...DEFAULT_FORMAT_OPTIONS, blockStyle: 'expand' as const };
const fmtExpand = (src: string): string => formatSource(src, EXPAND_OPTIONS);

/** Inputs that parse cleanly — the invariants must hold on every one. */
const CLEAN_SAMPLES: Record<string, string> = {
  assignment: 'foo:=Account new.',
  cascade: 'Transcript show: 1;show: 2;nl.',
  keywordSend: 'dict at: #a put: 1.',
  block: 'coll do:[:each|Transcript showCr: each printString].',
  nestedBlocks: '[:x|[:y|x+y]value: 2]value: 1.',
  classDef: "Object subclass: Foo [\n| a b |\nbar [ \"doc\" ^a+b ]\n]",
  methodCascade: "Object subclass: Acct [\nlog [ self a;b;c ]\n]",
  comment: 'foo bar. "trailing" baz qux.',
  blankRuns: 'a := 1.\n\n\n\nb := 2.',
  literals: 'x := #(1 2 3). y := #[1 2 3]. z := {1. 2. 3}.',
};

/** Inputs that do NOT parse cleanly — must come back byte-for-byte unchanged. */
const MALFORMED_SAMPLES: Record<string, string> = {
  unclosedBlock: 'Object subclass: Foo [\n  bar [ ^1 \n',
  unterminatedString: "x := 'oops.",
  strayBracket: 'foo bar ] baz.',
};

for (const [name, src] of Object.entries(CLEAN_SAMPLES)) {
  test(`idempotent: ${name}`, () => {
    const once = fmt(src);
    const twice = fmt(once);
    assert.equal(twice, once, 'format(format(x)) must equal format(x)');
  });

  test(`token-stream invariant: ${name}`, () => {
    assert.deepEqual(
      sigTokens(fmt(src)),
      sigTokens(src),
      'formatting must not add, drop, split, or merge any significant token',
    );
  });

  test(`expand idempotent + token-invariant: ${name}`, () => {
    const once = fmtExpand(src);
    assert.equal(fmtExpand(once), once, 'blockStyle=expand must be idempotent');
    assert.deepEqual(sigTokens(once), sigTokens(src), 'blockStyle=expand must not change the token stream');
  });
}

for (const [name, src] of Object.entries(MALFORMED_SAMPLES)) {
  test(`non-destructive on parse error: ${name}`, () => {
    assert.ok(parse(src).diagnostics.length > 0, 'sample must actually be malformed');
    assert.equal(fmt(src), src, 'an unparseable file must be returned unchanged (no edits)');
  });
}

// A couple of concrete spacing goldens so "idempotent" can't pass by being a no-op.
test('normalizes assignment spacing', () => {
  assert.equal(fmt('foo:=Account new.'), 'foo := Account new.');
});

test('empty input yields empty output', () => {
  assert.equal(fmt(''), '');
});

// Local-only gate: the invariants must also hold across the whole kernel corpus.
const KERNEL_DIR = path.join(process.cwd(), '../smalltalk-3.2.5/kernel');
if (!fs.existsSync(KERNEL_DIR)) {
  console.log(`format: kernel corpus skipped (not found at ${path.relative(process.cwd(), KERNEL_DIR)}).`);
} else {
  const files = fs.readdirSync(KERNEL_DIR).filter((f) => f.endsWith('.st'));
  test(`idempotence + token invariance across all ${files.length} kernel files (both block styles)`, () => {
    for (const file of files) {
      const src = fs.readFileSync(path.join(KERNEL_DIR, file), 'utf8');
      for (const f of [fmt, fmtExpand]) {
        const once = f(src);
        assert.equal(f(once), once, `idempotence failed on ${file}`);
        assert.deepEqual(sigTokens(once), sigTokens(src), `token stream changed on ${file}`);
      }
    }
  });
}

console.log(`format property tests: ${passed} passed.`);
