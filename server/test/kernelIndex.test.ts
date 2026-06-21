// Kernel-index tests (US-413, slice A / AC1; ADR-0002).
//
// Two layers, mirroring kernel.test.ts's skip-when-absent pattern:
//   1. ALWAYS (CI, corpus absent): load the COMMITTED server/data/kernel-index.json
//      and assert its invariants — known classes, sorted/typed shape, and the
//      LICENSING gate (facts only: structurally no comment/prose fields).
//   2. WHEN the corpus is present (local): regenerate in-memory and assert it is
//      byte-identical to the committed file — a drift guard.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import {
  BUNDLED_HEADER,
  bundledIndexPath,
  bundledKernelDir,
  indexKernelDirectory,
  serializeKernelIndex,
} from '../src/kernel/indexer.ts';
import type { KernelIndexData } from '../src/kernel/model.ts';

const CLASS_KEYS = new Set(['superclass', 'instanceSelectors', 'classSelectors']);
const SELECTOR_KEYS = new Set(['selector', 'arity']);
const HEADER_KEYS = new Set(['dialect', 'library', 'version', 'source', 'classCount', 'selectorCount', 'classes']);

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

const isSorted = (xs: string[]): boolean => xs.every((x, i) => i === 0 || (xs[i - 1] as string) <= x);

const raw = fs.readFileSync(bundledIndexPath(), 'utf8');
const index = JSON.parse(raw) as KernelIndexData;

test('committed index has the bundled GST 3.2.5 header', () => {
  assert.deepEqual(
    { dialect: index.dialect, library: index.library, version: index.version, source: index.source },
    BUNDLED_HEADER,
  );
  assert.deepEqual(new Set(Object.keys(index)), HEADER_KEYS);
});

test('counts are self-consistent and substantial', () => {
  const names = Object.keys(index.classes);
  assert.equal(index.classCount, names.length);
  let selectors = 0;
  for (const c of Object.values(index.classes)) {
    selectors += c.instanceSelectors.length + c.classSelectors.length;
  }
  assert.equal(index.selectorCount, selectors);
  assert.ok(index.classCount >= 100, `expected a substantial corpus (got ${index.classCount} classes)`);
});

test('known kernel classes are present with expected superclass links', () => {
  for (const n of ['Object', 'Collection', 'String', 'Integer', 'Boolean', 'Array', 'OrderedCollection']) {
    assert.ok(index.classes[n], `expected class ${n} in the kernel index`);
  }
  assert.equal(index.classes['Collection']?.superclass, 'Iterable');
  assert.equal(index.classes['SmallInteger']?.superclass, 'Integer');
  assert.ok((index.classes['Collection']?.instanceSelectors.length ?? 0) > 0);
});

test('class keys and selector lists are sorted (deterministic output)', () => {
  assert.ok(isSorted(Object.keys(index.classes)), 'class names must be sorted');
  for (const [name, c] of Object.entries(index.classes)) {
    assert.ok(isSorted(c.instanceSelectors.map((s) => s.selector)), `${name} instance selectors must be sorted`);
    assert.ok(isSorted(c.classSelectors.map((s) => s.selector)), `${name} class selectors must be sorted`);
  }
});

test('LICENSING gate: facts only — no comment/prose fields (ADR-0002)', () => {
  for (const [name, c] of Object.entries(index.classes)) {
    // Only the allowed structural keys exist on a class — no `comment`, `doc`, `body`, …
    for (const k of Object.keys(c)) {
      assert.ok(CLASS_KEYS.has(k), `class ${name} has unexpected field "${k}" (possible prose leak)`);
    }
    if (c.superclass !== undefined) {
      assert.equal(typeof c.superclass, 'string');
      assert.ok(!/\s/.test(c.superclass), `superclass of ${name} looks like prose`);
    }
    for (const s of [...c.instanceSelectors, ...c.classSelectors]) {
      assert.deepEqual(new Set(Object.keys(s)), SELECTOR_KEYS, `selector on ${name} has unexpected fields`);
      assert.equal(typeof s.selector, 'string');
      assert.equal(typeof s.arity, 'number');
      assert.ok(s.arity >= 0 && Number.isInteger(s.arity), `bad arity for ${s.selector} on ${name}`);
      // A selector is an identifier / keyword chain / binary op — it never
      // contains whitespace (keyword chains can be long, e.g.
      // `subclass:declaration:classVariableNames:poolDictionaries:category:`),
      // so the no-whitespace invariant is the real facts-vs-prose signal.
      assert.ok(!/\s/.test(s.selector) && s.selector.length <= 128, `selector "${s.selector}" looks like prose`);
    }
  }
});

if (!fs.existsSync(bundledKernelDir())) {
  console.log('kernelIndex: drift guard skipped (corpus not found).');
} else {
  test('drift guard: regenerating from the corpus reproduces the committed file', () => {
    const regenerated = serializeKernelIndex(indexKernelDirectory(bundledKernelDir(), BUNDLED_HEADER));
    assert.equal(
      regenerated,
      raw,
      'server/data/kernel-index.json is stale — run `npm run gen:kernel-index` and commit the result',
    );
  });
}

console.log(`kernelIndex: ${passed} tests passed.`);
