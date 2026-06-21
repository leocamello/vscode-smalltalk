// Kernel index service + discovery tests (US-413, slice B / AC5+AC6).
//
// Pure logic — no real `gst` needed (works in CI). Installed-kernel discovery is
// exercised against temporary fixture directories created at runtime.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {
  DEFAULT_COMMON_LOCATIONS,
  deriveKernelDirFromGst,
  discoverKernelDir,
  looksLikeKernelDir,
} from '../src/kernel/discovery.ts';
import { KernelIndexService } from '../src/kernel/kernelIndexService.ts';
import { Provenance } from '../src/kernel/model.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

/** Make a throwaway directory containing one parseable kernel-ish `.st` file. */
function makeFixtureKernel(className = 'Foo'): string {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'st-kernel-'));
  fs.writeFileSync(
    path.join(dir, `${className}.st`),
    `Object subclass: ${className} [\n  bar [ ^42 ]\n  baz: x [ ^x ]\n]\n`,
    'utf8',
  );
  return dir;
}

// --- discovery -------------------------------------------------------------
test('looksLikeKernelDir: true for a dir with .st, false otherwise', () => {
  const dir = makeFixtureKernel();
  assert.equal(looksLikeKernelDir(dir), true);
  assert.equal(looksLikeKernelDir(path.join(dir, 'nope')), false);
  const empty = fs.mkdtempSync(path.join(os.tmpdir(), 'st-empty-'));
  assert.equal(looksLikeKernelDir(empty), false);
});

test('deriveKernelDirFromGst: <prefix>/bin/gst → <prefix>/share/smalltalk/kernel', () => {
  assert.equal(
    deriveKernelDirFromGst('/usr/local/bin/gst'),
    path.join('/usr/local', 'share', 'smalltalk', 'kernel'),
  );
  assert.equal(deriveKernelDirFromGst('gst'), undefined); // bare PATH name
});

test('discoverKernelDir: explicit kernelPath wins', () => {
  const dir = makeFixtureKernel();
  assert.equal(discoverKernelDir({ kernelPath: dir }), dir);
});

test('discoverKernelDir: derives from the gst executable prefix', () => {
  const prefix = fs.mkdtempSync(path.join(os.tmpdir(), 'st-prefix-'));
  fs.mkdirSync(path.join(prefix, 'bin'), { recursive: true });
  fs.writeFileSync(path.join(prefix, 'bin', 'gst'), '', 'utf8');
  const kernel = path.join(prefix, 'share', 'smalltalk', 'kernel');
  fs.mkdirSync(kernel, { recursive: true });
  fs.writeFileSync(path.join(kernel, 'Object.st'), 'Object subclass: Bar [ ]\n', 'utf8');
  assert.equal(discoverKernelDir({ gnuSmalltalkPath: path.join(prefix, 'bin', 'gst') }), kernel);
});

test('discoverKernelDir: probes common locations, else undefined', () => {
  const dir = makeFixtureKernel();
  assert.equal(discoverKernelDir({}, [dir]), dir);
  assert.equal(discoverKernelDir({}, [path.join(dir, 'absent')]), undefined);
  assert.ok(DEFAULT_COMMON_LOCATIONS.length >= 2); // sane default set
});

// --- service resolution ----------------------------------------------------
test('off: no active kernel, identity off', () => {
  const svc = new KernelIndexService(undefined, []);
  svc.configure({ kernelLibrary: 'off' });
  assert.equal(svc.isEnabled, false);
  assert.equal(svc.identity.source, 'off');
  assert.deepEqual(svc.selectors(), []);
  assert.deepEqual(svc.classes(), []);
});

test('bundled: serves the gst-3.2.5 reference index with bundled provenance', () => {
  const svc = new KernelIndexService(undefined, []);
  svc.configure({ kernelLibrary: 'bundled' });
  assert.equal(svc.identity.source, 'bundled');
  assert.match(svc.identity.label, /bundled \(gst 3\.2\.5\)/);
  const classNames = svc.classes().map((c) => c.name);
  assert.ok(classNames.includes('Object'), 'bundled index should contain Object');
  const sels = svc.selectors();
  assert.ok(sels.length > 100);
  assert.ok(sels.every((s) => s.provenance === Provenance.BundledKernel));
});

test('auto with a discoverable install: indexes it live with installed provenance', () => {
  const dir = makeFixtureKernel('Widget');
  const svc = new KernelIndexService(undefined, []);
  svc.configure({ kernelLibrary: 'auto', kernelPath: dir });
  assert.equal(svc.identity.source, 'installed');
  const classNames = svc.classes().map((c) => c.name);
  assert.ok(classNames.includes('Widget'), 'should index the fixture class');
  assert.ok(!classNames.includes('Object') || classNames.includes('Widget'));
  const sels = svc.selectors();
  assert.ok(sels.some((s) => s.selector === 'baz:' && s.arity === 1));
  assert.ok(sels.every((s) => s.provenance === Provenance.InstalledKernel));
});

test('auto with no install: falls back to bundled', () => {
  const svc = new KernelIndexService(undefined, []);
  svc.configure({ kernelLibrary: 'auto', kernelPath: path.join(os.tmpdir(), 'definitely-absent-kernel') });
  assert.equal(svc.identity.source, 'bundled');
  assert.ok(svc.classes().some((c) => c.name === 'Object'));
});

test('reconfigure switches sources (auto→off→bundled)', () => {
  const dir = makeFixtureKernel('Gadget');
  const svc = new KernelIndexService(undefined, []);
  svc.configure({ kernelLibrary: 'auto', kernelPath: dir });
  assert.equal(svc.identity.source, 'installed');
  svc.configure({ kernelLibrary: 'off' });
  assert.equal(svc.isEnabled, false);
  svc.configure({ kernelLibrary: 'bundled' });
  assert.equal(svc.identity.source, 'bundled');
});

console.log(`kernelService: ${passed} tests passed.`);
