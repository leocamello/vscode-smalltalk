// Pure unit tests for gst resolution + command-string quoting (US-301 AC5/AC6/AC11).
// Runs in Node via tsx — no VS Code required.
import assert from 'node:assert/strict';
import { buildRunCommand, defaultExeNames, resolveGst } from '../src/gstLocator.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

const SEP = ':';
const onPath = (execs: Set<string>) => (c: string) => execs.has(c);

test('configured setting wins (AC5)', () => {
  const r = resolveGst({
    configuredPath: '/opt/gst/bin/gst',
    pathEnv: '/usr/bin',
    delimiter: SEP,
    exeNames: ['gst'],
    isExecutable: onPath(new Set(['/opt/gst/bin/gst', '/usr/bin/gst'])),
  });
  assert.deepEqual(r, { path: '/opt/gst/bin/gst', source: 'setting' });
});

test('configured-but-invalid resolves to undefined (no silent PATH fallback)', () => {
  const r = resolveGst({
    configuredPath: '/nope/gst',
    pathEnv: '/usr/bin',
    delimiter: SEP,
    exeNames: ['gst'],
    isExecutable: onPath(new Set(['/usr/bin/gst'])),
  });
  assert.equal(r, undefined);
});

test('falls back to PATH when setting empty (AC6)', () => {
  const r = resolveGst({
    configuredPath: '   ',
    pathEnv: '/usr/local/bin:/usr/bin',
    delimiter: SEP,
    exeNames: ['gst'],
    isExecutable: onPath(new Set(['/usr/bin/gst'])),
  });
  assert.deepEqual(r, { path: '/usr/bin/gst', source: 'path' });
});

test('returns undefined when not found anywhere (AC7)', () => {
  const r = resolveGst({
    configuredPath: '',
    pathEnv: '/usr/bin:/bin',
    delimiter: SEP,
    exeNames: ['gst'],
    isExecutable: onPath(new Set()),
  });
  assert.equal(r, undefined);
});

test('windows exe names include gst.exe', () => {
  assert.deepEqual(defaultExeNames('win32'), ['gst.exe', 'gst']);
  assert.deepEqual(defaultExeNames('linux'), ['gst']);
});

test('command quotes paths with spaces (AC11)', () => {
  assert.equal(
    buildRunCommand('/opt/gnu smalltalk/gst', '/home/me/my file.st'),
    '"/opt/gnu smalltalk/gst" "/home/me/my file.st"',
  );
});

console.log(`gstLocator: ${passed} tests passed.`);
