// Unit tests for the signature-help provider (US-425 / AC1, AC2). Pure: the
// lexer token stream + a hand-built SignatureCandidate set drive `signatureHelpAt`.
// No server runtime, no VS Code. Covers the backward keyword-scan cursor analysis,
// the prefix-union matching, active-parameter tracking, and the null cases.
import assert from 'node:assert/strict';
import { tokenize } from '../src/parser/lexer.ts';
import { Provenance } from '../src/kernel/model.ts';
import { signatureHelpAt, type SignatureCandidate } from '../src/providers/signatureHelp.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

/** Build a candidate from a selector, deriving keyword parts as the provider does. */
function cand(selector: string, provenance = Provenance.BundledKernel): SignatureCandidate {
  const keywords = selector.match(/[^:]+:/g) ?? [selector];
  return { selector, keywords, provenance };
}

const KERNEL: SignatureCandidate[] = [
  cand('at:put:'),
  cand('at:'),
  cand('at:ifAbsent:'),
  cand('at:put:ifAbsent:'),
  cand('do:'),
  cand('do:separatedBy:'),
  cand('printString'), // unary — never a signature-help candidate
  cand('+'), // binary — never a candidate
];

/** Cursor at the first occurrence of `marker` removed from `src` (▮ convention). */
function helpAt(src: string, signatures: SignatureCandidate[] = KERNEL) {
  const offset = src.indexOf('▮');
  assert.ok(offset >= 0, 'test source must mark the cursor with ▮');
  const text = src.replace('▮', '');
  return signatureHelpAt(offset, text, tokenize(text).tokens, signatures);
}

// --- AC1: active-parameter tracking over a keyword send -------------------------

test('first keyword: active parameter 0, at:put: among the prefix union', () => {
  const help = helpAt('dict at: ▮');
  assert.ok(help, 'a keyword send yields signature help');
  assert.equal(help.activeParameter, 0, 'after the first keyword, parameter 0 is active');
  assert.ok(help.signatures.some((s) => s.selector === 'at:put:'), 'at:put: is in the union');
  assert.ok(help.signatures.some((s) => s.selector === 'at:'), 'the bare at: selector is in the union');
});

test('second keyword: active parameter 1', () => {
  const help = helpAt('dict at: 1 put: ▮');
  assert.ok(help);
  assert.equal(help.activeParameter, 1, 'after the second keyword, parameter 1 is active');
  assert.ok(
    help.signatures.some((s) => s.label.includes('at:') && s.label.includes('put:')),
    'at:put: still matches the two typed keywords',
  );
});

test('prefix union narrows: at:put: prefix excludes at:ifAbsent:', () => {
  const help = helpAt('dict at: 1 put: ▮');
  assert.ok(help);
  const selectors = new Set(help.signatures.map((s) => s.selector));
  assert.ok(!selectors.has('at:ifAbsent:'), 'at:ifAbsent: drops out once put: is typed');
  // at:put:ifAbsent: stays — its first two keywords still match.
  assert.ok(selectors.has('at:put:ifAbsent:'), 'longer matching selector remains');
});

test('a matching signature exposes one parameter per keyword part, with the active one bolded', () => {
  const help = helpAt('dict at: ▮');
  const sig = help?.signatures.find((s) => s.selector === 'at:put:');
  assert.ok(sig, 'at:put: signature present');
  assert.equal(sig.parameters?.length, 2, 'two keyword parts → two parameters');
});

test('activeSignature prefers the fully-typed selector', () => {
  const help = helpAt('dict do: ▮'); // do: and do:separatedBy: both match
  assert.ok(help);
  const active = help.signatures[help.activeSignature ?? 0];
  assert.equal(active?.selector, 'do:', 'the exactly-typed selector is active');
});

test('nested keyword argument is skipped, not treated as the outer message', () => {
  // `coll inject: (a at: b) into: ▮` — the inner at: is inside parens (depth>0),
  // so the outer typed keywords are inject:into: (active parameter 1).
  const help = helpAt('coll inject: (a at: b) into: ▮', [
    ...KERNEL,
    cand('inject:into:'),
  ]);
  assert.ok(help);
  assert.equal(help.activeParameter, 1, 'inject:into: with into: active');
  assert.ok(help.signatures.some((s) => s.selector === 'inject:into:'));
});

test('provenance is carried on each signature', () => {
  const help = helpAt('x at: ▮', [cand('at:put:', Provenance.Workspace)]);
  const sig = help?.signatures[0];
  assert.ok(sig, 'a signature is returned');
  const doc = typeof sig.documentation === 'string' ? sig.documentation : sig.documentation?.value ?? '';
  assert.match(doc + (sig.label ?? ''), /workspace/i, 'workspace provenance is surfaced');
});

test('workspace ≻ bundled dedup keeps one signature per selector', () => {
  const help = helpAt('x at: ▮', [cand('at:put:', Provenance.Workspace), cand('at:put:', Provenance.BundledKernel)]);
  assert.ok(help);
  assert.equal(help.signatures.filter((s) => s.selector === 'at:put:').length, 1, 'deduped to one');
});

// --- AC1: null cases (no signature help) ---------------------------------------

test('unary send cursor returns null', () => {
  assert.equal(helpAt('x printString ▮'), null, 'no keyword context → null');
});

test('head / receiver cursor returns null', () => {
  assert.equal(helpAt('di▮ct at: 1'), null, 'cursor in the receiver, before any keyword → null');
});

test('binary send cursor returns null', () => {
  assert.equal(helpAt('a + ▮'), null, 'binary send → null');
});

test('cascade `;` bounds the message; the new keyword tracks its own active parameter', () => {
  const help = helpAt('x foo; at: ▮');
  assert.ok(help, 'the cascade message at: gets help');
  assert.equal(help.activeParameter, 0);
});

test('assignment bounds the left; RHS keyword send tracks from zero', () => {
  const help = helpAt('y := dict at: ▮');
  assert.ok(help);
  assert.equal(help.activeParameter, 0);
});

test('a period statement boundary is respected', () => {
  // The first statement is complete; the cursor is in a fresh unary context.
  assert.equal(helpAt('x at: 1 put: 2. y printString ▮'), null);
});

test('empty index returns null even at a keyword cursor', () => {
  assert.equal(helpAt('dict at: ▮', []), null, 'no candidates → null');
});

test('a typed keyword that matches no candidate returns null', () => {
  assert.equal(helpAt('dict frobnicate: ▮'), null, 'unknown keyword → null');
});

console.log(`\nsignatureHelp: ${passed} tests passed.`);
