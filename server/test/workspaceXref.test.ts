// Unit tests for the workspace cross-reference index (US-423, Slice A). Runs in
// Node via tsx — pure front end + the index, no server runtime, no VS Code.
// Written BEFORE the merge engine consumes it: pins the send-site facts and the
// incremental-patch contract (AC1 union half, AC5 O(1), AC6 receiver hint).
import assert from 'node:assert/strict';
import { WorkspaceXref } from '../src/xref/workspaceXref.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

const URI = 'file:///ws/a.st';

test('records a send site with its enclosing method context', () => {
  const src = ['Object subclass: Stage [', '  run: who [ ^who greet ]', ']'].join('\n');
  const xref = new WorkspaceXref();
  xref.setFile(URI, src);
  const sites = xref.sendersOf('greet');
  assert.equal(sites.length, 1, 'one send of #greet');
  const s = sites[0];
  assert.equal(s.uri, URI);
  assert.equal(s.inClass, 'Stage', 'enclosing class is captured');
  assert.equal(s.inSelector, 'run:', 'enclosing selector is captured');
  assert.equal(s.side, 'instance');
});

test('selector range lands on the selector token, not the whole send', () => {
  const src = 'Object subclass: Stage [\n  run: who [ ^who greet ]\n]';
  const xref = new WorkspaceXref();
  xref.setFile(URI, src);
  const s = xref.sendersOf('greet')[0];
  // `greet` begins at column after "  run: who [ ^who " on line 1.
  const line = '  run: who [ ^who greet ]';
  assert.equal(s.range.start.line, 1);
  assert.equal(s.range.start.character, line.indexOf('greet'));
  assert.equal(s.range.end.character, line.indexOf('greet') + 'greet'.length);
});

test('receiver hints: self / super / capitalized class / dynamic', () => {
  const src = [
    'Object subclass: Foo [',
    '  m [ self a. super b. OrderedCollection new. x c ]',
    ']',
  ].join('\n');
  const xref = new WorkspaceXref();
  xref.setFile(URI, src);
  assert.equal(xref.sendersOf('a')[0].receiverHint, 'self');
  assert.equal(xref.sendersOf('b')[0].receiverHint, 'super');
  assert.equal(xref.sendersOf('new')[0].receiverHint, 'OrderedCollection', 'capitalized literal receiver');
  assert.equal(xref.sendersOf('c')[0].receiverHint, null, 'lowercase/dynamic receiver → no hint');
});

test('keyword and binary sends are recorded under their full selector', () => {
  const src = 'Object subclass: Foo [\n  m [ a at: 1 put: 2. b , c ]\n]';
  const xref = new WorkspaceXref();
  xref.setFile(URI, src);
  assert.equal(xref.sendersOf('at:put:').length, 1, 'keyword selector keyed whole');
  assert.equal(xref.sendersOf(',').length, 1, 'binary selector recorded');
});

test('top-level (script) sends are recorded with no enclosing class', () => {
  const xref = new WorkspaceXref();
  xref.setFile(URI, 'Transcript showCr: 42 printString');
  const show = xref.sendersOf('showCr:')[0];
  assert.ok(show, 'top-level send is indexed');
  assert.equal(show.inClass, undefined, 'no enclosing method');
  assert.equal(xref.sendersOf('printString').length, 1);
});

test('incremental patch: re-setFile replaces, removeFile clears', () => {
  const xref = new WorkspaceXref();
  xref.setFile(URI, 'Object subclass: Foo [\n  m [ self greet ]\n]');
  assert.equal(xref.sendersOf('greet').length, 1);
  // Edit the file so it no longer sends greet.
  xref.setFile(URI, 'Object subclass: Foo [\n  m [ self wave ]\n]');
  assert.equal(xref.sendersOf('greet').length, 0, 'stale send removed on re-index');
  assert.equal(xref.sendersOf('wave').length, 1, 'new send indexed');
  xref.removeFile(URI);
  assert.equal(xref.sendersOf('wave').length, 0, 'removeFile clears the slice');
  assert.equal(xref.size, 0);
});

test('global map merges sends across files; removing one file keeps the other', () => {
  const xref = new WorkspaceXref();
  xref.setFile('file:///ws/a.st', 'Object subclass: A [\n  m [ self ping ]\n]');
  xref.setFile('file:///ws/b.st', 'Object subclass: B [\n  m [ self ping ]\n]');
  assert.equal(xref.sendersOf('ping').length, 2, 'union across files');
  xref.removeFile('file:///ws/a.st');
  const left = xref.sendersOf('ping');
  assert.equal(left.length, 1);
  assert.equal(left[0].uri, 'file:///ws/b.st', 'only b.st remains');
});

test('sendsFrom returns the outgoing sends of one method (call-hierarchy outgoing)', () => {
  const src = ['Object subclass: Foo [', '  m [ self a. self b ]', '  n [ self c ]', ']'].join('\n');
  const xref = new WorkspaceXref();
  xref.setFile(URI, src);
  const out = xref.sendsFrom(URI, 'Foo', 'instance', 'm').map((s) => s.selector).sort();
  assert.deepEqual(out, ['a', 'b'], 'only the sends of method m, not n');
});

console.log(`workspaceXref.test: ${passed} passed`);
