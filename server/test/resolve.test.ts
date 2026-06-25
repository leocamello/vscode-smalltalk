// Unit tests for the two-tier merge engine (US-423, Slice B). Pure: feeds
// hand-built tier facts and pins the union, dev-box dedup/precedence, synthetic
// cartridge URIs, and the honest receiver-hint ranking (AC1/AC5/AC6).
import assert from 'node:assert/strict';
import {
  cartridgeUri,
  resolveImplementors,
  resolveReferences,
  resolveSenders,
  simpleName,
  type XrefSources,
} from '../src/xref/resolve.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

const GST = { dialect: 'gnu-smalltalk', version: '3.2.5', label: 'reference (gst 3.2.5)' };
const r = (line: number) => ({ start: { line, character: 0 }, end: { line, character: 4 } });

function sources(partial: Partial<XrefSources>): XrefSources {
  return {
    workspaceSenders: [],
    workspaceImplementors: [],
    cartridgeSenders: [],
    cartridgeImplementors: [],
    cartridge: GST,
    ...partial,
  };
}

test('simpleName strips the namespace; cartridgeUri is a stable read-only target', () => {
  assert.equal(simpleName('Smalltalk.OrderedCollection'), 'OrderedCollection');
  assert.equal(simpleName('ProtoObject'), 'ProtoObject');
  assert.equal(
    cartridgeUri(GST, 'Smalltalk.Object', 'instance', 'printOn:'),
    'smalltalk-cartridge:/gnu-smalltalk/3.2.5/Smalltalk.Object/instance/printOn%3A',
  );
});

test('implementors are the union of workspace + cartridge, each tagged with provenance', () => {
  const refs = resolveImplementors(
    sources({
      workspaceImplementors: [{ uri: 'file:///ws/a.st', className: 'MyList', side: 'instance', range: r(2) }],
      cartridgeImplementors: [{ inClass: 'Smalltalk.OrderedCollection', side: 'instance' }],
    }),
    'do:',
  );
  assert.equal(refs.length, 2, 'both implementors present');
  assert.equal(refs[0].provenance.kind, 'workspace', 'workspace ranks first');
  assert.equal(refs[1].provenance.label, 'reference (gst 3.2.5)', 'per-row cartridge provenance (status-bar identity)');
  assert.equal(refs[1].className, 'OrderedCollection');
  assert.ok(refs[1].uri.startsWith('smalltalk-cartridge:/'), 'a cartridge hit with no source gets a virtual URI');
});

test('a located cartridge ref (installed source) navigates to the REAL file, not a stub', () => {
  const refs = resolveImplementors(
    sources({
      cartridgeImplementors: [
        { inClass: 'Array', side: 'instance', sourceUri: 'file:///usr/local/share/smalltalk/kernel/Array.st', sourceLine: 59 },
      ],
    }),
    'printOn:',
  );
  assert.equal(refs.length, 1);
  assert.equal(refs[0].uri, 'file:///usr/local/share/smalltalk/kernel/Array.st', 'opens the real installed file');
  assert.equal(refs[0].range.start.line, 59, 'jumps to the def line');
  assert.equal(refs[0].provenance.kind, 'cartridge', 'still tagged as a kernel/cartridge row');
});

test('a located cartridge SENDER navigates to the real send line', () => {
  const refs = resolveSenders(
    sources({
      cartridgeSenders: [
        { inClass: 'Autoload', side: 'instance', inSelector: 'loadedClass_', line: 14, sourceUri: 'file:///k/Autoload.st', sourceLine: 132 },
      ],
    }),
    'error:',
  );
  assert.equal(refs[0].uri, 'file:///k/Autoload.st');
  assert.equal(refs[0].range.start.line, 132, 'jumps to the real send line, not the within-method offset');
});

test('dev-box overlap: a live workspace implementor shadows the frozen cartridge one', () => {
  const refs = resolveImplementors(
    sources({
      // The kernel source is ALSO open in the workspace (dev box), so Object>>printOn:
      // appears in both tiers. Keep the live one, drop the frozen duplicate.
      workspaceImplementors: [{ uri: 'file:///gst/Object.st', className: 'Object', side: 'instance', range: r(10) }],
      cartridgeImplementors: [
        { inClass: 'Smalltalk.Object', side: 'instance' },
        { inClass: 'Smalltalk.Point', side: 'instance' },
      ],
    }),
    'printOn:',
  );
  assert.equal(refs.length, 2, 'Object de-duped (live wins), Point kept');
  const objectRefs = refs.filter((x) => x.className === 'Object');
  assert.equal(objectRefs.length, 1);
  assert.equal(objectRefs[0].provenance.kind, 'workspace', 'the surviving Object impl is the live one');
});

test('senders are ranked by hint confidence but never filtered (AC6)', () => {
  const refs = resolveSenders(
    sources({
      workspaceSenders: [
        { uri: 'file:///ws/a.st', selector: 'do:', range: r(1), inClass: 'A', side: 'instance', inSelector: 'm1', receiverHint: null },
        { uri: 'file:///ws/a.st', selector: 'do:', range: r(2), inClass: 'A', side: 'instance', inSelector: 'm2', receiverHint: 'self' },
        { uri: 'file:///ws/a.st', selector: 'do:', range: r(3), inClass: 'A', side: 'instance', inSelector: 'm3', receiverHint: 'Bag' },
      ],
      isKnownClass: (n) => n === 'Bag',
    }),
    'do:',
  );
  assert.equal(refs.length, 3, 'the dynamic (null-hint) long-tail sender is NOT dropped');
  // self (2) and known-class Bag (2) rank above the null-hint send (0).
  assert.equal(refs[2].receiverHint, null, 'unknown receiver sinks to the bottom, still present');
  assert.ok(refs[0].receiverHint === 'self' || refs[0].receiverHint === 'Bag');
});

test('dev-box overlap for senders: same enclosing method shadows the frozen sender', () => {
  const refs = resolveSenders(
    sources({
      workspaceSenders: [
        { uri: 'file:///gst/Object.st', selector: 'error:', range: r(5), inClass: 'Object', side: 'instance', inSelector: 'check', receiverHint: 'self' },
      ],
      cartridgeSenders: [
        { inClass: 'Smalltalk.Object', side: 'instance', inSelector: 'check', line: 5, receiverHint: 'self' },
        { inClass: 'Smalltalk.Point', side: 'instance', inSelector: 'validate', line: 9 },
      ],
    }),
    'error:',
  );
  assert.equal(refs.length, 2, 'Object>>check de-duped, Point>>validate kept');
  assert.equal(refs.filter((x) => x.className === 'Object').length, 1);
});

test('with no cartridge loaded, only the workspace tier contributes (AC7)', () => {
  const refs = resolveReferences(
    sources({
      cartridge: undefined,
      workspaceImplementors: [{ uri: 'file:///ws/a.st', className: 'A', side: 'instance', range: r(0) }],
      cartridgeImplementors: [{ inClass: 'Smalltalk.Object', side: 'instance' }],
      cartridgeSenders: [{ inClass: 'Smalltalk.Object', side: 'instance', inSelector: 'x', line: 1 }],
    }),
    'do:',
  );
  assert.equal(refs.length, 1, 'cartridge tier ignored when absent');
  assert.equal(refs[0].provenance.kind, 'workspace');
});

test('references is the union of implementors and senders', () => {
  const refs = resolveReferences(
    sources({
      workspaceImplementors: [{ uri: 'file:///ws/a.st', className: 'A', side: 'instance', range: r(0) }],
      workspaceSenders: [
        { uri: 'file:///ws/b.st', selector: 'greet', range: r(4), inClass: 'B', side: 'instance', inSelector: 'run', receiverHint: null },
      ],
    }),
    'greet',
  );
  assert.equal(refs.length, 2);
  assert.deepEqual(refs.map((x) => x.kind).sort(), ['implementor', 'sender']);
});

console.log(`resolve.test: ${passed} passed`);
