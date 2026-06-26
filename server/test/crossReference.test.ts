// Unit tests for the branded command result builder (US-423, Slice C / AC2).
// Pure: hand-built ResolvedRefs → the header (union/uncertainty disclaimer) +
// per-row provenance and receiver-hint badges the client renders as a tree.
import assert from 'node:assert/strict';
import { buildCrossReference } from '../src/providers/crossReference.ts';
import type { ResolvedRef } from '../src/xref/resolve.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

const r = (line: number) => ({ start: { line, character: 0 }, end: { line, character: 4 } });
const WS = { kind: 'workspace' as const, label: 'workspace' };
const CART = { kind: 'cartridge' as const, label: 'reference (gst 3.2.5)' };

test('senders result carries the union/uncertainty disclaimer + a count', () => {
  const refs: ResolvedRef[] = [
    { kind: 'sender', uri: 'file:///a.st', range: r(1), provenance: WS, className: 'Stage', side: 'instance', inSelector: 'run', receiverHint: 'self' },
  ];
  const result = buildCrossReference('senders', 'do:', refs);
  assert.equal(result.title, 'Senders of #do:');
  assert.match(result.disclaimer, /dynamic/i, 'disclaimer states the dynamic-dispatch caveat');
  assert.match(result.disclaimer, /union/i, 'disclaimer names the lexical union');
  assert.match(result.disclaimer, /none are hidden|all candidates/i, 'disclaimer promises nothing is filtered (AC6)');
  assert.equal(result.count, 1);
});

test('a sender row labels the calling method + a provenance and receiver badge', () => {
  const refs: ResolvedRef[] = [
    { kind: 'sender', uri: 'file:///a.st', range: r(2), provenance: WS, className: 'Stage', side: 'instance', inSelector: 'run', receiverHint: 'self' },
  ];
  const row = buildCrossReference('senders', 'greet', refs).rows[0];
  assert.equal(row.label, 'Stage » run', 'labels the calling method');
  assert.match(row.detail, /workspace/, 'per-row provenance badge (AC2)');
  assert.match(row.detail, /receiver: self/, 'receiver-hint badge for senders');
  assert.equal(row.provenance, 'workspace');
});

test('a class-side sender qualifies the class; a top-level send is honest about it', () => {
  const refs: ResolvedRef[] = [
    { kind: 'sender', uri: 'file:///a.st', range: r(0), provenance: WS, className: 'Factory', side: 'class', inSelector: 'build', receiverHint: null },
    { kind: 'sender', uri: 'file:///s.st', range: r(0), provenance: WS, receiverHint: null },
  ];
  const rows = buildCrossReference('senders', 'new', refs).rows;
  assert.equal(rows[0].label, 'Factory class » build');
  assert.equal(rows[1].label, '(top level)');
});

test('implementors rows carry per-row provenance incl. the cartridge label', () => {
  const refs: ResolvedRef[] = [
    { kind: 'implementor', uri: 'file:///a.st', range: r(3), provenance: WS, className: 'MyList', side: 'instance' },
    { kind: 'implementor', uri: 'smalltalk-cartridge:/gnu-smalltalk/3.2.5/x', range: r(0), provenance: CART, className: 'OrderedCollection', side: 'instance' },
  ];
  const result = buildCrossReference('implementors', 'do:', refs);
  assert.equal(result.title, 'Implementors of #do:');
  assert.equal(result.rows[0].label, 'MyList');
  assert.equal(result.rows[0].provenance, 'workspace');
  assert.equal(result.rows[1].provenance, 'reference (gst 3.2.5)');
  assert.match(result.disclaimer, /receiver's class/i, 'implementors disclaimer frames runtime dispatch');
});

test('an empty result still produces a header (count 0) — the user sees the honest framing', () => {
  const result = buildCrossReference('senders', 'nope', []);
  assert.equal(result.count, 0);
  assert.equal(result.rows.length, 0);
  assert.ok(result.disclaimer.length > 0);
});

console.log(`crossReference.test: ${passed} passed`);
