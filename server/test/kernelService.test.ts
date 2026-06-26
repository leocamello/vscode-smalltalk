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
import { indexKernelDirectoryToCartridge } from '../src/kernel/indexer.ts';
import { KernelIndexService } from '../src/kernel/kernelIndexService.ts';
import { Provenance } from '../src/kernel/model.ts';
import type { CartridgeHeader } from '../src/types/knowledge-base.ts';

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

test('bundled: serves the gst-3.2.5 reference floor with bundled provenance', () => {
  const svc = new KernelIndexService(undefined, []);
  svc.configure({ kernelLibrary: 'bundled' });
  assert.equal(svc.identity.source, 'bundled');
  // ADR-0003 status label: the frozen reference floor, distinct from installed.
  assert.equal(svc.identity.label, 'reference (gst 3.2.5)');
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

// --- Tier-1 installed adapter (cartridge shape; ADR-0003) ------------------
const TEST_CARTRIDGE_HEADER: CartridgeHeader = {
  schema: 1,
  dialect: 'gnu-smalltalk',
  dialectLabel: 'GNU Smalltalk',
  library: 'kernel',
  version: 'installed',
  source: 'gst-source',
  sourceLicense: 'LGPL-2.1-only',
  carriesProse: false,
  tiers: ['classes', 'crossReference'],
  builtAt: '2026-06-22T00:00:00+00:00',
  contentHash: 'unstamped',
};

test('installed adapter (indexKernelDirectoryToCartridge) emits cartridge shape', () => {
  const dir = makeFixtureKernel('Sprocket');
  const cart = indexKernelDirectoryToCartridge(dir, TEST_CARTRIDGE_HEADER);
  assert.equal(cart.header.schema, 1);
  assert.equal(cart.header.carriesProse, false);
  // The installed adapter now ships a crossReference tier (US-423): implementors
  // from the class tables; senders scanned from method bodies (none here). Each
  // implementor carries the real def location so navigation opens the file.
  assert.ok(cart.crossReference, 'installed adapter emits a crossReference tier');
  const bazImpl = cart.crossReference?.implementors['baz:']?.[0];
  assert.equal(bazImpl?.inClass, 'Sprocket');
  assert.equal(bazImpl?.side, 'instance');
  assert.match(bazImpl?.sourceUri ?? '', /Sprocket\.st$/, 'implementor points at the real file');
  assert.equal(typeof bazImpl?.sourceLine, 'number');
  assert.ok(cart.crossReference?.implementors['bar'], 'bar is an implementor too');
  assert.equal(Object.keys(cart.crossReference?.senders ?? {}).length, 0, 'Sprocket sends nothing → no senders');
  const fact = cart.classes['Sprocket'];
  assert.ok(fact, 'expected the fixture class as a ClassFact');
  assert.equal(fact?.id, 'Sprocket'); // flat dialect: id === name
  assert.equal(fact?.name, 'Sprocket');
  assert.equal(fact?.kind, 'class');
  assert.equal(fact?.superclass, 'Object');
  const baz = fact?.instanceMethods.find((m) => m.selector === 'baz:');
  assert.deepEqual(baz, { selector: 'baz:', arity: 1, keywords: ['baz:'] });
  // Facts-only: no prose anywhere on the emitted facts.
  assert.equal(fact?.documentation, undefined);
});

test('installed adapter scans method bodies into crossReference senders (US-423)', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'st-senders-'));
  fs.writeFileSync(
    path.join(dir, 'Gadget.st'),
    'Object subclass: Gadget [\n  run [ ^self spin ]\n  spin [ ^42 ]\n]\n',
    'utf8',
  );
  const cart = indexKernelDirectoryToCartridge(dir, TEST_CARTRIDGE_HEADER);
  const spin = cart.crossReference?.senders['spin']?.[0];
  assert.ok(cart.crossReference?.senders['spin']?.length === 1, 'the self-send of #spin is recorded as a sender');
  assert.equal(spin?.inClass, 'Gadget');
  assert.equal(spin?.side, 'instance');
  assert.equal(spin?.inSelector, 'run');
  assert.equal(spin?.receiverHint, 'self');
  // The sender carries the real send location (file + absolute line) to jump to.
  assert.match(spin?.sourceUri ?? '', /Gadget\.st$/);
  assert.equal(spin?.sourceLine, 1, '`^self spin` is on line 1 (0-based)');
  // And implementors of #spin point back at Gadget, with the def location.
  const spinImpl = cart.crossReference?.implementors['spin']?.[0];
  assert.equal(spinImpl?.inClass, 'Gadget');
  assert.equal(spinImpl?.sourceLine, 2, '`spin [ ^42 ]` is defined on line 2 (0-based)');
});

/** A throwaway kernel dir whose class + method carry comments (US-415 slice B). */
function makeCommentedKernel(className = 'Widget'): string {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'st-prose-'));
  fs.writeFileSync(
    path.join(dir, `${className}.st`),
    `Object subclass: ${className} [\n  <comment: 'A ${className}.'>\n  bar [ "Answer bar." ^42 ]\n]\n`,
    'utf8',
  );
  return dir;
}

test('installed adapter captures prose when carriesProse=true (US-415)', () => {
  const dir = makeCommentedKernel('Widget');
  const cart = indexKernelDirectoryToCartridge(dir, {
    ...TEST_CARTRIDGE_HEADER,
    carriesProse: true,
    tiers: ['classes', 'documentation'],
  });
  const fact = cart.classes['Widget'];
  assert.equal(fact?.documentation?.text, 'A Widget.');
  assert.equal(fact?.instanceMethods.find((m) => m.selector === 'bar')?.documentation?.text, 'Answer bar.');
});

test('installed adapter stays facts-only when carriesProse=false (US-415)', () => {
  const dir = makeCommentedKernel('Gizmo');
  const cart = indexKernelDirectoryToCartridge(dir, TEST_CARTRIDGE_HEADER); // carriesProse:false
  const fact = cart.classes['Gizmo'];
  assert.equal(fact?.documentation, undefined, 'class prose must not leak when facts-only');
  assert.equal(fact?.instanceMethods.find((m) => m.selector === 'bar')?.documentation, undefined);
});

test('service surfaces installed prose; bundled reference stays facts-only (US-415)', () => {
  const dir = makeCommentedKernel('Sprig');
  const svc = new KernelIndexService(undefined, []);
  svc.configure({ kernelLibrary: 'auto', kernelPath: dir });
  assert.equal(svc.identity.source, 'installed');
  assert.equal(svc.classComment('Sprig'), 'A Sprig.');
  assert.equal(svc.implementorsOf('bar').find((c) => c.name === 'Sprig')?.comment, 'Answer bar.');
  // The shipped reference floor is facts-only: provenance gates prose, not dialect.
  svc.configure({ kernelLibrary: 'bundled' });
  assert.equal(svc.classComment('Object'), undefined, 'bundled reference must not surface prose');
});

test('auto installed: status label reads "installed", resolved via the cartridge adapter', () => {
  const dir = makeFixtureKernel('Cog');
  const svc = new KernelIndexService(undefined, []);
  svc.configure({ kernelLibrary: 'auto', kernelPath: dir });
  assert.equal(svc.identity.source, 'installed');
  assert.match(svc.identity.label, /installed \(gst\)/);
  assert.ok(svc.classes().some((c) => c.name === 'Cog'));
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
