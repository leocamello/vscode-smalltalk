// Unit tests for the opt-in gst diagnostics tier (US-414, Slice B / AC2-AC3).
// Pure + injected-spawner — no real gst, CI-safe. Covers: gst-3.2.5 stderr → LSP
// diagnostics; no-zombie / kill-on-supersede runner behaviour; gst resolution.
import assert from 'node:assert/strict';
import { DiagnosticSeverity } from 'vscode-languageserver-types';
import {
  parseGstStderr,
  GstDiagnosticsRunner,
  GST_DIAGNOSTIC_SOURCE,
  GST_DIAGNOSTIC_CODE,
  type SpawnedProc,
  type Spawner,
} from '../src/gst/gstRunner.ts';
import { resolveGst } from '../src/gst/resolveGst.ts';

const tests: [string, () => void | Promise<void>][] = [];
const test = (name: string, fn: () => void | Promise<void>): void => void tests.push([name, fn]);

// --- parseGstStderr (verified gst-3.2.5 stderr format) ---
test('parseGstStderr: missing ] → whole-line Error on the right line', () => {
  const src = 'Object subclass: Foo [\n  bar [ ^1 \n]\n';
  const diags = parseGstStderr("bad.st:4: parse error, expected ']'\n", src);
  assert.equal(diags.length, 1);
  const [d] = diags;
  assert.equal(d.source, GST_DIAGNOSTIC_SOURCE);
  assert.equal(d.code, GST_DIAGNOSTIC_CODE);
  assert.equal(GST_DIAGNOSTIC_SOURCE, 'gst');
  assert.equal(d.severity, DiagnosticSeverity.Error);
  assert.equal(d.message, "parse error, expected ']'");
  assert.equal(d.range.start.line, 3, 'gst line 4 → 0-based line 3');
  assert.equal(d.range.start.character, 0, 'whole-line range starts at column 0');
});

test('parseGstStderr: end character sizes to the source line length', () => {
  const src = "Transcript showCr: 'oops."; // single line, length 25
  const [d] = parseGstStderr('e3.st:1: Unterminated string, attempting recovery\n', src);
  assert.equal(d.range.start.line, 0);
  assert.equal(d.range.end.line, 0);
  assert.equal(d.range.end.character, src.length);
});

test('parseGstStderr: several error formats all parse', () => {
  const stderr = [
    'a.st:2: expected object',
    'a.st:3: invalid class body element',
    'a.st:4: expected expression',
  ].join('\n');
  const diags = parseGstStderr(stderr, 'l0\nl1\nl2\nl3\nl4');
  assert.equal(diags.length, 3);
  assert.deepEqual(diags.map((d) => d.range.start.line), [1, 2, 3]);
  assert.deepEqual(diags.map((d) => d.message), ['expected object', 'invalid class body element', 'expected expression']);
});

test('parseGstStderr: non-diagnostic lines (banner/blank) are ignored', () => {
  const stderr = 'GNU Smalltalk ready\n\nSome unrelated note without the colon-line pattern\n';
  assert.deepEqual(parseGstStderr(stderr, 'x'), []);
});

// --- GstDiagnosticsRunner: no-zombie / kill-on-supersede ---
class FakeProc implements SpawnedProc {
  killed = false;
  closed = false;
  readonly dataCbs: ((c: Buffer | string) => void)[] = [];
  readonly closeCbs: ((code: number | null) => void)[] = [];
  readonly errorCbs: ((e: Error) => void)[] = [];
  readonly stderr = { on: (_e: 'data', cb: (c: Buffer | string) => void): void => void this.dataCbs.push(cb) };
  on(event: 'close' | 'error', cb: (arg: never) => void): void {
    if (event === 'close') this.closeCbs.push(cb as (code: number | null) => void);
    else this.errorCbs.push(cb as (e: Error) => void);
  }
  kill(): boolean {
    this.killed = true;
    this.emitClose(null);
    return true;
  }
  emitData(s: string): void {
    for (const cb of this.dataCbs) cb(s);
  }
  emitClose(code: number | null): void {
    if (this.closed) return;
    this.closed = true;
    for (const cb of this.closeCbs) cb(code);
  }
  emitError(e: Error): void {
    for (const cb of this.errorCbs) cb(e);
  }
}

test('runner: rapid runs for one uri kill every superseded child, leaving none running', async () => {
  const procs: FakeProc[] = [];
  const spawner: Spawner = () => {
    const p = new FakeProc();
    procs.push(p);
    return p;
  };
  const runner = new GstDiagnosticsRunner(spawner, 10_000);
  const uri = 'file:///x.st';

  const promises: Promise<unknown>[] = [];
  for (let i = 0; i < 5; i++) {
    promises.push(runner.run(uri, 'gst', '/x.st', 'src'));
  }
  assert.equal(procs.length, 5);
  assert.ok(procs.slice(0, 4).every((p) => p.killed), 'every superseded child must be killed');
  assert.ok(runner.isRunning(uri), 'the last run is still in flight');

  procs[4].emitData("z.st:1: parse error, expected ']'\n");
  procs[4].emitClose(0);
  const results = await Promise.all(promises);
  assert.equal(runner.isRunning(uri), false, 'no child left running after completion (no zombies)');
  assert.equal(results.slice(0, 4).every((r) => r === null), true, 'superseded runs resolve null');
  assert.ok(Array.isArray(results[4]) && (results[4] as unknown[]).length === 1, 'final run yields diagnostics');
});

test('runner: spawn error makes the tier inert (resolves null, nothing running)', async () => {
  const spawner: Spawner = () => {
    const p = new FakeProc();
    queueMicrotask(() => p.emitError(new Error('ENOENT')));
    return p;
  };
  const runner = new GstDiagnosticsRunner(spawner, 10_000);
  const result = await runner.run('file:///y.st', 'gst', '/y.st', 'src');
  assert.equal(result, null);
  assert.equal(runner.isRunning('file:///y.st'), false);
});

test('runner: cancel kills the in-flight child', () => {
  const procs: FakeProc[] = [];
  const runner = new GstDiagnosticsRunner(() => {
    const p = new FakeProc();
    procs.push(p);
    return p;
  }, 10_000);
  void runner.run('file:///z.st', 'gst', '/z.st', 'src');
  assert.ok(runner.isRunning('file:///z.st'));
  runner.cancel('file:///z.st');
  assert.ok(procs[0].killed);
  assert.equal(runner.isRunning('file:///z.st'), false);
});

// --- resolveGst (server-side) ---
test('resolveGst: a non-empty configured path wins when executable', () => {
  const got = resolveGst({ configuredPath: '/opt/gst/bin/gst', isExecutable: (c) => c === '/opt/gst/bin/gst' });
  assert.equal(got, '/opt/gst/bin/gst');
});

test('resolveGst: configured-but-not-executable yields undefined (no PATH fallback)', () => {
  const got = resolveGst({ configuredPath: '/nope/gst', pathEnv: '/usr/bin', isExecutable: () => false });
  assert.equal(got, undefined);
});

test('resolveGst: empty config searches PATH', () => {
  const got = resolveGst({
    configuredPath: '',
    pathEnv: '/a:/usr/local/bin:/b',
    platform: 'linux',
    delimiter: ':',
    isExecutable: (c) => c === '/usr/local/bin/gst',
  });
  assert.equal(got, '/usr/local/bin/gst');
});

test('resolveGst: nothing found → undefined (tier inert)', () => {
  assert.equal(resolveGst({ configuredPath: '', pathEnv: '/a:/b', delimiter: ':', isExecutable: () => false }), undefined);
});

// Run sequentially in an async IIFE (the test runner imports this under CJS, so
// no top-level await). Exit non-zero on the first failure, mirroring the evals.
void (async (): Promise<void> => {
  let passed = 0;
  for (const [name, fn] of tests) {
    try {
      await fn();
    } catch (e) {
      console.error(`  FAIL - ${name}: ${(e as Error).message}`);
      process.exit(1);
    }
    passed += 1;
    console.log(`  ok - ${name}`);
  }
  console.log(`gst diagnostics: ${passed} checks passed.`);
})();
