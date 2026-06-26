// SPIKE-01 — unit tests for the unknown-selector gate (AC1 truth table, AC2 the
// six escape hatches) + the cartridge-backed ClassWorld behaviours that matter
// most for false positives: the `self` subclass-closure (Template Method) and the
// class-object metaclass protocol. Pure; no server runtime, no VS Code.
import assert from 'node:assert/strict';
import { NodeKind, type MessageNode, type Node } from '../src/parser/ast.ts';
import type { MethodSide } from '../src/types/knowledge-base.ts';
import {
  evaluateMissingSelector,
  type ClassWorld,
  type GateOutcome,
  type SendContext,
} from '../src/diagnostics/unknownSelectorGate.ts';
import { cartridgeClassWorld } from '../src/diagnostics/cartridgeClassWorld.ts';
import { bundledCartridge, loadCartridge } from '../src/kernel/cartridgeLoader.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

// --- minimal AST builders (the gate reads only receiver/selector/messageType) ---
const POS = { line: 0, character: 0 };
const base = { start: 0, end: 0, startPos: POS, endPos: POS };
function v(name: string): Node {
  return { kind: NodeKind.Variable, name, ...base } as Node;
}
function send(receiver: Node, selector: string, messageType: 'unary' | 'binary' | 'keyword' = 'unary'): MessageNode {
  return { kind: NodeKind.Message, receiver, selector, messageType, arguments: [], ...base } as MessageNode;
}

// --- a hand-rolled ClassWorld for controlled truth-table / hatch tests ----------
interface Spec {
  super?: string;
  inst?: string[];
  cls?: string[];
  subclasses?: string[]; // descendants whose OWN methods join `self` resolution
  dnu?: string; // simple-name of the class defining doesNotUnderstand:
  incomplete?: boolean;
}
function makeWorld(classes: Record<string, Spec>): ClassWorld {
  const META = new Set(['new', 'basicNew', 'name', 'category']); // metaclass protocol stub
  const chainSet = (name: string, side: MethodSide): Set<string> | undefined => {
    const acc = new Set<string>();
    let cur: string | undefined = name;
    const seen = new Set<string>();
    while (cur && !seen.has(cur)) {
      seen.add(cur);
      const s = classes[cur];
      if (!s) {
        return undefined; // unknown class in the chain → open world
      }
      for (const m of (side === 'instance' ? s.inst : s.cls) ?? []) acc.add(m);
      cur = s.super;
    }
    if (side === 'class') for (const m of META) acc.add(m);
    return acc;
  };
  return {
    rootDnuClass: 'Object',
    isKnownClass: (n) => classes[n] !== undefined,
    superclassOf: (n) => classes[n]?.super,
    isIncompleteTable: (n) => classes[n]?.incomplete === true,
    respondsTo: chainSet,
    respondsToSelf: (name, side) => {
      const base = chainSet(name, side);
      if (!base) return undefined;
      const acc = new Set(base);
      for (const sub of classes[name]?.subclasses ?? []) {
        for (const m of (side === 'instance' ? classes[sub]?.inst : classes[sub]?.cls) ?? []) acc.add(m);
      }
      return acc;
    },
    dnuDefiner: (n) => {
      let cur: string | undefined = n;
      const seen = new Set<string>();
      while (cur && !seen.has(cur)) {
        seen.add(cur);
        if (classes[cur]?.dnu) return classes[cur]!.dnu;
        cur = classes[cur]?.super;
      }
      return 'Object'; // root default DNU
    },
  };
}

function ctx(node: MessageNode, over: Partial<SendContext> = {}): SendContext {
  return { node, enclosingSide: 'instance', methodClean: true, optedOut: false, ...over };
}

const outcome = (c: SendContext, w: ClassWorld): GateOutcome => evaluateMissingSelector(c, w).outcome;

// --- AC1: truth table -----------------------------------------------------------

test('Unknown receiver (a local/expression) → silent (open world)', () => {
  const w = makeWorld({ Foo: { inst: ['bar'] } });
  assert.equal(outcome(ctx(send(v('x'), 'anything')), w), 'unknown-receiver');
});

test('self understood in its own table → silent', () => {
  const w = makeWorld({ Animal: { inst: ['speak'] } });
  assert.equal(outcome(ctx(send(v('self'), 'speak'), { enclosingClass: 'Animal' }), w), 'understood');
});

test('self NOT in table, clean, no hatch → EMIT (the only firing row)', () => {
  const w = makeWorld({ Animal: { inst: ['speak'] } });
  assert.equal(outcome(ctx(send(v('self'), 'fly'), { enclosingClass: 'Animal' }), w), 'emit');
});

test('self to a subclass-only method (Template Method) → silent (subclass union)', () => {
  // Abstract `Shape` sends `self area`; only the concrete `Circle` implements it.
  const w = makeWorld({ Shape: { inst: ['draw'], subclasses: ['Circle'] }, Circle: { super: 'Shape', inst: ['area'] } });
  assert.equal(outcome(ctx(send(v('self'), 'area'), { enclosingClass: 'Shape' }), w), 'understood');
});

test('unknown selector, method not clean → silent (don\'t lint broken code)', () => {
  const w = makeWorld({ Animal: { inst: ['speak'] } });
  assert.equal(outcome(ctx(send(v('self'), 'fly'), { enclosingClass: 'Animal', methodClean: false }), w), 'method-not-clean');
});

test('super resolves to the superclass table (exact, not subclasses)', () => {
  const w = makeWorld({ Base: { inst: ['init'] }, Derived: { super: 'Base', inst: ['extra'] } });
  // super init → Base understands it.
  assert.equal(outcome(ctx(send(v('super'), 'init'), { enclosingClass: 'Derived' }), w), 'understood');
  // super extra → Base does NOT (extra is Derived's own) → emit.
  assert.equal(outcome(ctx(send(v('super'), 'extra'), { enclosingClass: 'Derived' }), w), 'emit');
});

test('a literal class receiver resolves to the class side + metaclass protocol', () => {
  const w = makeWorld({ Widget: { cls: ['default'] } });
  assert.equal(outcome(ctx(send(v('Widget'), 'new')), w), 'understood'); // `new` via metaclass stub
  assert.equal(outcome(ctx(send(v('Widget'), 'default')), w), 'understood');
  assert.equal(outcome(ctx(send(v('Widget'), 'nope')), w), 'emit');
});

// --- AC2: escape hatches --------------------------------------------------------

test('hatch: perform:-family is always silent', () => {
  const w = makeWorld({ Animal: { inst: ['speak'] } });
  assert.equal(outcome(ctx(send(v('self'), 'perform:with:', 'keyword'), { enclosingClass: 'Animal' }), w), 'escape-perform');
});

test('hatch: reflective allowlist (respondsTo:) is silent', () => {
  const w = makeWorld({ Animal: { inst: ['speak'] } });
  assert.equal(outcome(ctx(send(v('self'), 'respondsTo:', 'keyword'), { enclosingClass: 'Animal' }), w), 'escape-allowlist');
});

test('hatch: custom doesNotUnderstand: in the chain → forwarding → silent', () => {
  // A neutral name (not matching the proxy regex) so the DNU hatch is what fires.
  const w = makeWorld({ RemoteThing: { inst: ['speak'], dnu: 'RemoteThing' } });
  assert.equal(outcome(ctx(send(v('self'), 'anythingAtAll'), { enclosingClass: 'RemoteThing' }), w), 'escape-dnu');
});

test('hatch: proxy/mock/stub name signal → silent', () => {
  const w = makeWorld({ ServiceProxy: { inst: ['speak'] } });
  assert.equal(outcome(ctx(send(v('self'), 'whatever'), { enclosingClass: 'ServiceProxy' }), w), 'escape-proxy');
});

test('hatch: incomplete/extension table → silent', () => {
  const w = makeWorld({ Patched: { inst: ['speak'], incomplete: true } });
  assert.equal(outcome(ctx(send(v('self'), 'whatever'), { enclosingClass: 'Patched' }), w), 'escape-incomplete');
});

test('hatch: explicit opt-out pragma → silent', () => {
  const w = makeWorld({ Animal: { inst: ['speak'] } });
  assert.equal(outcome(ctx(send(v('self'), 'fly'), { enclosingClass: 'Animal', optedOut: true }), w), 'escape-optout');
});

// --- cartridge-backed reality checks (the real FP drivers) ----------------------

const world = cartridgeClassWorld(loadCartridge(bundledCartridge));

test('cartridge: `Float>>… self exponent` is understood (subclass-union, abstract class)', () => {
  // exponent lives on FloatD/FloatE/FloatQ, never on abstract Float — must NOT emit.
  assert.equal(outcome(ctx(send(v('self'), 'exponent'), { enclosingClass: 'Float' }), world), 'understood');
});

test('cartridge: `OrderedCollection new` is understood (metaclass `new` via Behavior)', () => {
  assert.equal(outcome(ctx(send(v('OrderedCollection'), 'new'), {}), world), 'understood');
});

test('cartridge: a fabricated typo on a closed-world receiver EMITS', () => {
  // `self prtinString` inside Object — a clear misspelling of printString.
  assert.equal(outcome(ctx(send(v('self'), 'prtinString'), { enclosingClass: 'Object' }), world), 'emit');
});

console.log(`\nunknownSelectorGate: ${passed} tests passed.`);
