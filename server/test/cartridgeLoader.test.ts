// Cartridge loader + projection tests (US-430, slice B; EPIC-005, ADR-0003).
//
// Three concerns, mirroring kernelIndex.test.ts's facts-only discipline:
//   1. AC2 — the committed Cartridge #01 round-trips through JSON with no
//      functions/cycles and validates structurally against the schema.
//   2. AC4 — facts-only licensing gate: carriesProse === false ⇒ NO `documentation`
//      anywhere (class- or method-level), and selectors carry no prose.
//   3. convergence — `cartridgeToKernelIndex` projects a (fixture and real)
//      cartridge to the US-413 KernelIndexData the completion service consumes,
//      and `loadCartridge` resolves inheritance/trait method tables correctly.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { cartridgeContentHash } from '../src/kernel/cartridgeHash.ts';
import {
  bundledCartridge,
  cartridgeToKernelIndex,
  loadCartridge,
} from '../src/kernel/cartridgeLoader.ts';
import type { DialectCartridge } from '../src/types/knowledge-base.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

const cartridgePath = path.join(process.cwd(), 'server/data/cartridges/gst-3.2.5-cartridge.json');

const isSorted = (xs: string[]): boolean => xs.every((x, i) => i === 0 || (xs[i - 1] as string) <= x);

// --- AC2: round-trip + structural validation ------------------------------
test('AC2 committed cartridge round-trips through JSON (no functions/cycles)', () => {
  const raw = fs.readFileSync(cartridgePath, 'utf8');
  const parsed = JSON.parse(raw) as DialectCartridge;
  // A structure with functions/cycles cannot survive a stringify→parse identity.
  assert.deepEqual(JSON.parse(JSON.stringify(parsed)), parsed);
  // The inlined (esbuild) copy must be the same facts as the committed file.
  assert.deepEqual(bundledCartridge, parsed);
});

test('AC2 header is structurally valid (schema 1, tiers, facts provenance)', () => {
  const h = bundledCartridge.header;
  assert.equal(h.schema, 1);
  assert.equal(h.dialect, 'gnu-smalltalk');
  assert.equal(h.version, '3.2.5');
  assert.equal(typeof h.contentHash, 'string');
  assert.ok(h.tiers.includes('classes'), 'must declare the classes tier');
  assert.ok(['gst-source', 'image-export', 'curated'].includes(h.source));
  assert.ok(typeof h.sourceLicense === 'string' && h.sourceLicense.length > 0);
});

test('the SHIPPED cartridge carries no machine-specific source paths (US-423)', () => {
  // `sourceUri`/`sourceLine` are a LOCAL-only convenience of the installed adapter
  // (real-file navigation). A committed/redistributed cartridge must never ship
  // them — they are this-machine paths and there is no portable source to point at.
  const cr = bundledCartridge.crossReference;
  const sites = [...Object.values(cr?.senders ?? {}), ...Object.values(cr?.implementors ?? {})].flat();
  assert.ok(sites.length > 0, 'the bundled reference ships a crossReference tier to check');
  assert.ok(
    sites.every((s) => (s as { sourceUri?: string }).sourceUri === undefined),
    'no shipped cross-reference fact may carry a sourceUri',
  );
});

test('AC2 every class fact has the required resolved-facts shape', () => {
  for (const [id, c] of Object.entries(bundledCartridge.classes)) {
    assert.equal(c.id, id, 'class record key must equal its id');
    assert.equal(typeof c.name, 'string');
    assert.ok(c.superclass === null || typeof c.superclass === 'string');
    assert.ok(Array.isArray(c.instanceMethods) && Array.isArray(c.classMethods));
    for (const m of [...c.instanceMethods, ...c.classMethods]) {
      assert.equal(typeof m.selector, 'string');
      assert.ok(Number.isInteger(m.arity) && m.arity >= 0);
      assert.ok(Array.isArray(m.keywords));
    }
  }
});

// --- AC5: deterministic contentHash stamping ------------------------------
test('AC5 contentHash is stamped and matches the hash of the fact tables', () => {
  const stamped = bundledCartridge.header.contentHash;
  assert.match(stamped, /^sha256-[0-9a-f]{64}$/, 'contentHash must be a stamped sha256, not "pending"');
  // The committed hash equals a fresh recompute (deterministic-output guard):
  // if the cartridge is regenerated, `npm run stamp:cartridge` must be re-run.
  assert.equal(stamped, cartridgeContentHash(bundledCartridge));
});

test('AC5 the hash excludes the header (idempotent under re-stamping)', () => {
  // Mutating a volatile header field must NOT change the fact-table hash.
  const touched: DialectCartridge = {
    ...bundledCartridge,
    header: { ...bundledCartridge.header, builtAt: '1999-01-01T00:00:00+00:00', contentHash: 'pending' },
  };
  assert.equal(cartridgeContentHash(touched), cartridgeContentHash(bundledCartridge));
});

// --- AC4: facts-only licensing gate ---------------------------------------
test('AC4 facts-only: carriesProse=false ⇒ no documentation prose anywhere', () => {
  assert.equal(bundledCartridge.header.carriesProse, false);
  for (const c of Object.values(bundledCartridge.classes)) {
    assert.equal(c.documentation, undefined, `class ${c.id} leaked documentation prose`);
    for (const m of [...c.instanceMethods, ...c.classMethods]) {
      assert.equal(m.documentation, undefined, `selector ${m.selector} on ${c.id} leaked prose`);
      // A selector is an identifier/keyword-chain/binary op — never whitespace prose.
      assert.ok(!/\s/.test(m.selector) && m.selector.length <= 128, `selector "${m.selector}" looks like prose`);
    }
  }
});

// --- convergence: projection over a fixture --------------------------------
/** A tiny, hand-built cartridge exercising namespaced ids, a null root superclass,
 *  and class/instance split — independent of the real Cartridge #01. */
const FIXTURE: DialectCartridge = {
  header: {
    schema: 1,
    dialect: 'gnu-smalltalk',
    dialectLabel: 'GNU Smalltalk',
    library: 'base',
    version: '9.9',
    source: 'image-export',
    sourceLicense: 'LGPL-2.1-only',
    carriesProse: false,
    tiers: ['classes'],
    builtAt: '2026-06-22T00:00:00+00:00',
    contentHash: 'fixture-projection',
  },
  classes: {
    'Smalltalk.Object': {
      id: 'Smalltalk.Object',
      name: 'Object',
      kind: 'class',
      superclass: null,
      instanceVariables: [],
      classVariables: [],
      classInstanceVariables: [],
      instanceMethods: [
        { selector: 'yourself', arity: 0, keywords: ['yourself'] },
        { selector: 'printOn:', arity: 1, keywords: ['printOn:'] },
      ],
      classMethods: [{ selector: 'new', arity: 0, keywords: ['new'] }],
      taxonomy: { namespace: 'Smalltalk' },
    },
    'Coll.Bag': {
      id: 'Coll.Bag',
      name: 'Bag',
      kind: 'class',
      superclass: 'Smalltalk.Object',
      instanceVariables: ['contents'],
      classVariables: [],
      classInstanceVariables: [],
      instanceMethods: [{ selector: 'add:', arity: 1, keywords: ['add:'] }],
      classMethods: [],
      taxonomy: { namespace: 'Coll' },
    },
  },
};

test('convergence projection keys by simple name, maps superclass, drops cartridge-only fields', () => {
  const idx = cartridgeToKernelIndex(FIXTURE);
  assert.deepEqual(idx, {
    dialect: 'gst',
    library: 'gst-9.9',
    version: '9.9',
    source: 'bundled',
    classCount: 2,
    selectorCount: 4,
    classes: {
      Bag: {
        superclass: 'Object', // ClassId Smalltalk.Object → simple name
        instanceSelectors: [{ selector: 'add:', arity: 1 }],
        classSelectors: [],
      },
      Object: {
        // root: superclass null ⇒ omitted (matches the US-413 index shape)
        instanceSelectors: [
          { selector: 'printOn:', arity: 1 },
          { selector: 'yourself', arity: 0 },
        ],
        classSelectors: [{ selector: 'new', arity: 0 }],
      },
    },
  });
  // sorted keys + sorted selectors (deterministic output, matches the old index)
  assert.ok(isSorted(Object.keys(idx.classes)));
  assert.ok(isSorted(idx.classes['Object']?.instanceSelectors.map((s) => s.selector) ?? []));
});

test('convergence projection of the real cartridge preserves completion facts', () => {
  const idx = cartridgeToKernelIndex(bundledCartridge);
  assert.equal(idx.dialect, 'gst');
  assert.equal(idx.version, '3.2.5');
  assert.equal(idx.library, 'gst-3.2.5'); // keeps the bundled status label stable
  assert.equal(idx.source, 'bundled');
  assert.equal(idx.classCount, Object.keys(bundledCartridge.classes).length);
  for (const n of ['Object', 'Collection', 'String', 'OrderedCollection']) {
    assert.ok(idx.classes[n], `expected class ${n} after projection`);
  }
  assert.equal(idx.classes['Collection']?.superclass, 'Iterable'); // namespaced id stripped
  assert.ok(isSorted(Object.keys(idx.classes)));
});

// --- convergence: loader resolves inheritance + traits ---------------------
/** Fixture with a superclass chain, an override, and a trait composition. */
const RESOLVE_FIXTURE: DialectCartridge = {
  header: { ...FIXTURE.header, contentHash: 'fixture-resolve' },
  classes: {
    'X.Object': {
      id: 'X.Object',
      name: 'Object',
      kind: 'class',
      superclass: null,
      instanceVariables: [],
      classVariables: [],
      classInstanceVariables: [],
      instanceMethods: [
        { selector: 'printString', arity: 0, keywords: ['printString'] },
        { selector: '=', arity: 1, keywords: ['='] },
      ],
      classMethods: [],
      taxonomy: {},
    },
    'X.Greetable': {
      id: 'X.Greetable',
      name: 'Greetable',
      kind: 'trait',
      superclass: null,
      instanceVariables: [],
      classVariables: [],
      classInstanceVariables: [],
      instanceMethods: [
        { selector: 'hello', arity: 0, keywords: ['hello'] },
        { selector: 'secret', arity: 0, keywords: ['secret'] },
      ],
      classMethods: [],
      taxonomy: {},
    },
    'X.Widget': {
      id: 'X.Widget',
      name: 'Widget',
      kind: 'class',
      superclass: 'X.Object',
      instanceVariables: [],
      classVariables: [],
      classInstanceVariables: [],
      // override `printString` from the superclass; own `hello` shadows the trait's.
      instanceMethods: [
        { selector: 'printString', arity: 0, keywords: ['printString'] },
        { selector: 'hello', arity: 0, keywords: ['hello'] },
      ],
      classMethods: [],
      traitComposition: [
        { trait: 'X.Greetable', exclusions: ['secret'], aliases: { hi: 'hello' } },
      ],
      taxonomy: {},
    },
  },
};

test('loader methodTableOf resolves own ∪ inherited ∪ trait with correct shadowing', () => {
  const loaded = loadCartridge(RESOLVE_FIXTURE);
  const table = loaded.methodTableOf('X.Widget');
  // inherited from the superclass:
  assert.equal(table.get('=')?.inClass, 'X.Object');
  // override: own printString shadows the superclass definition:
  assert.equal(table.get('printString')?.inClass, 'X.Widget');
  // trait alias present; excluded trait method absent:
  assert.equal(table.get('hi')?.inClass, 'X.Greetable');
  assert.equal(table.get('hi')?.signature.selector, 'hi');
  assert.equal(table.get('secret'), undefined);
  // own method shadows the trait's same-named method:
  assert.equal(table.get('hello')?.inClass, 'X.Widget');
});

test('loader implementorsOf/sendersOf read the crossReference (derive when absent)', () => {
  // Real cartridge ships crossReference: use the precomputed view.
  const real = loadCartridge(bundledCartridge);
  const impls = real.implementorsOf('printString');
  assert.ok(impls.length > 0 && impls.some((i) => i.inClass === 'Smalltalk.Object'));
  assert.deepEqual(real.sendersOf('a-selector-that-does-not-exist'), []);

  // Fixture has no crossReference → implementors derived from class tables.
  const loaded = loadCartridge(RESOLVE_FIXTURE);
  const helloImpls = loaded.implementorsOf('hello');
  const classesWithHello = helloImpls.map((i) => i.inClass).sort();
  assert.deepEqual(classesWithHello, ['X.Greetable', 'X.Widget']);
});

test('loadCartridge memoizes derived views by content hash', () => {
  assert.equal(loadCartridge(bundledCartridge), loadCartridge(bundledCartridge));
});

console.log(`cartridgeLoader: ${passed} tests passed.`);
