// Cancellation-token plumbing (US-902, AC6). tsx.
//
// The workspace-spanning requests (workspace symbols, references, call
// hierarchy, rename) honour the LSP CancellationToken so a superseded request
// stops instead of completing a full multi-file scan. The reusable primitive is
// `readFilesCancellable` (used by the rename file-gather loops) + `isCancelled`.
// This test drives the primitive directly (a genuine contract test); the server
// handlers wire the token in (verified by review + the manual-QA matrix).
import assert from 'node:assert/strict';
import { isCancelled, readFilesCancellable } from '../src/providers/cancellation.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

test('isCancelled: undefined token is never cancelled', () => {
  assert.equal(isCancelled(undefined), false);
  assert.equal(isCancelled({ isCancellationRequested: false }), false);
  assert.equal(isCancelled({ isCancellationRequested: true }), true);
});

test('readFilesCancellable reads every file when not cancelled', () => {
  const reads: string[] = [];
  const read = (uri: string): string | undefined => {
    reads.push(uri);
    return `text:${uri}`;
  };
  const out = readFilesCancellable(['a', 'b', 'c'], read, undefined);
  assert.deepEqual(reads, ['a', 'b', 'c'], 'all files read with no token');
  assert.deepEqual(out, [
    { uri: 'a', text: 'text:a' },
    { uri: 'b', text: 'text:b' },
    { uri: 'c', text: 'text:c' },
  ]);
});

test('readFilesCancellable stops early once the token is cancelled', () => {
  const reads: string[] = [];
  const token = { isCancellationRequested: false };
  const read = (uri: string): string | undefined => {
    reads.push(uri);
    if (reads.length === 2) token.isCancellationRequested = true; // cancel mid-scan
    return `text:${uri}`;
  };
  const out = readFilesCancellable(['a', 'b', 'c', 'd', 'e'], read, token);
  // It must NOT read all five — the loop bails once cancellation is observed.
  assert.ok(reads.length < 5, `expected an early bail, read all ${reads.length} files`);
  assert.ok(out.length < 5, 'the returned set must be partial after cancellation');
});

test('readFilesCancellable with an already-cancelled token reads nothing', () => {
  const reads: string[] = [];
  const read = (uri: string): string | undefined => {
    reads.push(uri);
    return uri;
  };
  const out = readFilesCancellable(['a', 'b'], read, { isCancellationRequested: true });
  assert.deepEqual(reads, [], 'a pre-cancelled request does no file reads');
  assert.deepEqual(out, []);
});

test('readFilesCancellable skips unreadable files (undefined) without throwing', () => {
  const read = (uri: string): string | undefined => (uri === 'bad' ? undefined : uri);
  const out = readFilesCancellable(['ok', 'bad', 'ok2'], read, undefined);
  assert.deepEqual(out, [
    { uri: 'ok', text: 'ok' },
    { uri: 'ok2', text: 'ok2' },
  ]);
});

console.log(`cancellation.test: ${passed} passed.`);
