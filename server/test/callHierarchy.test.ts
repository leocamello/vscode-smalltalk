// Unit tests for the call-hierarchy provider (US-423, Slice D / AC4). Pure: the
// AST/tokens drive `resolveCallTarget`/`prepare`; hand-built tier facts drive the
// incoming (senders) / outgoing (sends) grouping. No server runtime, no VS Code.
import assert from 'node:assert/strict';
import { parse } from '../src/parser/parser.ts';
import { tokenize } from '../src/parser/lexer.ts';
import {
  incomingCalls,
  outgoingCalls,
  prepareCallHierarchy,
  resolveCallTarget,
  type CallItemData,
} from '../src/providers/callHierarchy.ts';
import type { ResolvedRef } from '../src/xref/resolve.ts';
import type { WorkspaceSendSite } from '../src/xref/workspaceXref.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

function targetAt(src: string, needle: string, occurrence = 0) {
  let from = -1;
  for (let i = 0; i <= occurrence; i++) {
    from = src.indexOf(needle, from + 1);
  }
  const offset = from + 1; // land inside the token
  return resolveCallTarget(parse(src).ast, tokenize(src).tokens, offset);
}

const r = (line: number) => ({ start: { line, character: 0 }, end: { line, character: 4 } });

test('resolveCallTarget on a method-definition selector yields the method identity', () => {
  const src = 'Object subclass: Speaker [\n  greet [ ^self ]\n]';
  const t = targetAt(src, 'greet', 0);
  assert.ok(t && t.kind === 'method', 'cursor on the method selector → method target');
  assert.equal(t.selector, 'greet');
  assert.equal(t.className, 'Speaker');
  assert.equal(t.side, 'instance');
});

test('resolveCallTarget on a class-side method captures side = class', () => {
  const src = 'Object subclass: Factory [\n  Factory class >> build [ ^self new ]\n]';
  const t = targetAt(src, 'build', 0);
  assert.ok(t && t.kind === 'method');
  assert.equal(t.className, 'Factory');
  assert.equal(t.side, 'class');
});

test('resolveCallTarget on a message send yields a send target', () => {
  const src = 'Object subclass: Stage [\n  run: who [ ^who greet ]\n]';
  const t = targetAt(src, 'greet', 0); // the send (only occurrence)
  assert.ok(t && t.kind === 'send');
  assert.equal(t.selector, 'greet');
  assert.equal(t.className, undefined, 'a send target carries no method identity');
});

test('prepare builds one item carrying the round-trip identity on data', () => {
  const src = 'Object subclass: Speaker [\n  greet [ ^self ]\n]';
  const t = targetAt(src, 'greet', 0);
  const items = prepareCallHierarchy(t, 'file:///a.st');
  assert.equal(items.length, 1);
  assert.equal(items[0].name, 'Speaker>>greet');
  const data = items[0].data as CallItemData;
  assert.equal(data.selector, 'greet');
  assert.equal(data.className, 'Speaker');
  assert.equal(data.side, 'instance');
  assert.equal(data.uri, 'file:///a.st');
});

test('prepare on nothing returns no items', () => {
  assert.deepEqual(prepareCallHierarchy(undefined, 'file:///a.st'), []);
});

test('incomingCalls groups senders by the calling method (multiple sends → one caller, many ranges)', () => {
  const senders: ResolvedRef[] = [
    { kind: 'sender', uri: 'file:///a.st', range: r(1), provenance: { kind: 'workspace', label: 'workspace' }, className: 'Stage', side: 'instance', inSelector: 'run', receiverHint: null },
    { kind: 'sender', uri: 'file:///a.st', range: r(2), provenance: { kind: 'workspace', label: 'workspace' }, className: 'Stage', side: 'instance', inSelector: 'run', receiverHint: 'self' },
    { kind: 'sender', uri: 'file:///b.st', range: r(5), provenance: { kind: 'workspace', label: 'workspace' }, className: 'Crew', side: 'instance', inSelector: 'cue', receiverHint: null },
  ];
  const incoming = incomingCalls(senders);
  assert.equal(incoming.length, 2, 'two distinct calling methods');
  const stage = incoming.find((c) => c.from.name === 'Stage>>run');
  assert.ok(stage, 'Stage>>run is a caller');
  assert.equal(stage.fromRanges.length, 2, 'its two send sites collapse into one caller with two ranges');
});

test('incomingCalls labels a top-level (script) sender honestly', () => {
  const senders: ResolvedRef[] = [
    { kind: 'sender', uri: 'file:///s.st', range: r(0), provenance: { kind: 'workspace', label: 'workspace' }, receiverHint: null },
  ];
  assert.equal(incomingCalls(senders)[0].from.name, '(top level)');
});

test('outgoingCalls groups the method body sends by callee selector', () => {
  const sends: WorkspaceSendSite[] = [
    { uri: 'file:///a.st', selector: 'a', range: r(1), receiverHint: 'self' },
    { uri: 'file:///a.st', selector: 'b', range: r(1), receiverHint: 'self' },
    { uri: 'file:///a.st', selector: 'a', range: r(2), receiverHint: 'self' },
  ];
  const outgoing = outgoingCalls(sends);
  assert.equal(outgoing.length, 2, 'two distinct callees (a, b)');
  const a = outgoing.find((c) => c.to.name === 'a');
  assert.ok(a);
  assert.equal(a.fromRanges.length, 2, 'two calls to a');
});

console.log(`callHierarchy.test: ${passed} passed`);
