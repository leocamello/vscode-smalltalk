// SPIKE-01 — Unknown-Selector Heuristic ("Heuristic of Extreme Caution").
//
// The static shadow of `doesNotUnderstand:`: decide whether a message send's
// selector is *provably* unknown — and therefore worth a (future) lint squiggle.
// In a dynamically typed language a single false positive on valid code is a
// ten-minute uninstall, so this gate is biased hard to SILENCE: it speaks only
// when the receiver resolves to a fully-indexed, closed-world class and every
// escape hatch is clear (spec §4 truth table, §5 hatches).
//
// THIS SHIPS NOTHING. It is a measurement instrument: a pure predicate driven by
// the corpus harness (`scripts/spike-unknown-selector.ts`) to tally false
// positives + closed-world coverage and inform the go/no-go memo. No published
// diagnostics, no UI. Pure — no `vscode`, no Node `fs`.

import { NodeKind, type MessageNode, type Node } from '../parser/ast';
import type { MethodSide } from '../types/knowledge-base';

/** Why the gate stayed silent — or that it fired. The harness tallies these to
 *  compute closed-world coverage (everything that is NOT `unknown-receiver`) and
 *  the emit set (`emit`). */
export type GateOutcome =
  | 'unknown-receiver' // open world — receiver could be an un-indexed class (silent)
  | 'understood' // selector is in the resolved method table (silent)
  | 'method-not-clean' // enclosing method has a parse error — don't lint mid-edit (silent)
  | 'escape-allowlist' // a reflective/meta selector (perform:, respondsTo:, …) (silent)
  | 'escape-perform' // a perform:-family send — computed selector (silent)
  | 'escape-proxy' // receiver class name signals a proxy/mock/stub (silent)
  | 'escape-dnu' // a custom doesNotUnderstand: in the chain → forwarding (silent)
  | 'escape-incomplete' // receiver is an extension / partially-indexed table (silent)
  | 'escape-optout' // explicit opt-out pragma/setting on the method (silent)
  | 'emit'; // KnownClosedWorld ∧ ¬understood ∧ clean ∧ ¬escape → the only firing row

export interface GateResult {
  readonly outcome: GateOutcome;
  /** Set only when a receiver resolves to a closed-world class (coverage + emit). */
  readonly targetClass?: string;
  readonly targetSide?: MethodSide;
}

/** The closed-world the gate queries — workspace ∪ cartridge, resolved to facts.
 *  An implementation walks the superclass chain to a known root; if the chain
 *  can't be fully resolved it returns `undefined` (open world → the gate is silent). */
export interface ClassWorld {
  /** Is `name` a known class at all (workspace ∪ cartridge)? */
  isKnownClass(name: string): boolean;
  /** Immediate superclass simple-name of `name`, or undefined. */
  superclassOf(name: string): string | undefined;
  /** The selectors a `name` instance / class-object responds to — own ∪ inherited
   *  ∪ traits, and for `class` side ∪ the metaclass (Behavior/Class) protocol.
   *  `undefined` ⇒ the chain isn't fully indexed (open world). */
  respondsTo(name: string, side: MethodSide): ReadonlySet<string> | undefined;
  /** Like {@link respondsTo}, but UNIONS the subclass closure of `name`. Used for
   *  `self` sends: in an *abstract* class, `self` is some concrete subclass, so a
   *  selector implemented only in a subclass (the Template Method pattern, e.g.
   *  `Float>>asFraction` sending `self exponent` where `exponent` lives in
   *  `FloatD`/`FloatE`/`FloatQ`) is genuinely understood and must NOT be flagged.
   *  `undefined` ⇒ the chain isn't fully indexed (open world). */
  respondsToSelf(name: string, side: MethodSide): ReadonlySet<string> | undefined;
  /** Simple-name of the class that defines `doesNotUnderstand:` for `name`'s
   *  instances, or undefined. Used for the custom-DNU escape hatch. */
  dnuDefiner(name: string): string | undefined;
  /** Is `name` an extension / explicitly-incomplete table (spec §5.4)? */
  isIncompleteTable(name: string): boolean;
  /** The dialect root that carries the *default* error `doesNotUnderstand:`
   *  (GST: `Object`). A DNU resolved anywhere else is a custom forwarder. */
  readonly rootDnuClass: string;
}

/** Context for one send under test, supplied by the harness from the AST walk. */
export interface SendContext {
  readonly node: MessageNode;
  /** The class enclosing the send (the `self`/`super` target), if inside a method. */
  readonly enclosingClass?: string;
  /** The enclosing method's side — `self` in a class method is the class object. */
  readonly enclosingSide: MethodSide;
  /** False when the enclosing method has a parse error (don't lint broken code). */
  readonly methodClean: boolean;
  /** True when the enclosing method/class carries the opt-out pragma (spec §5.6). */
  readonly optedOut: boolean;
}

/** Class-side instantiators whose result is an *instance* of the receiver class:
 *  `Foo new`, `Foo basicNew`. (`new:`/`basicNew:` handled as keyword sends.) */
const UNARY_INSTANTIATORS = new Set(['new', 'basicNew']);
const KEYWORD_INSTANTIATORS = new Set(['new:', 'basicNew:']);

/** perform:-family selectors — the dispatched selector is computed at runtime, so
 *  the literal send is invisible to static analysis (spec §5.2). */
const PERFORM_FAMILY = new Set([
  'perform:',
  'perform:with:',
  'perform:with:with:',
  'perform:with:with:with:',
  'perform:with:with:with:with:',
  'perform:withArguments:',
  'perform:withArguments:inSuperclass:',
  'perform:inSuperclass:',
]);

/** Reflective / meta allowlist (spec §5.5) — never flagged even if a table is thin. */
const REFLECTIVE_ALLOWLIST = new Set([
  'respondsTo:',
  'doesNotUnderstand:',
  '->',
  'value',
  'value:',
  'value:value:',
  'instVarNamed:',
  'instVarNamed:put:',
  'instVarAt:',
  'instVarAt:put:',
  'yourself',
  'isKindOf:',
  'isMemberOf:',
  'class',
]);

/** Soft proxy/forwarding name signals (spec §5.3) — configurable; conservative. */
const PROXY_NAME = /(Proxy|Mock|Stub|Adapter|Forwarder)$/;

function isVar(node: Node, name?: string): boolean {
  return node.kind === NodeKind.Variable && (name === undefined || node.name === name);
}

function isCapitalized(name: string): boolean {
  return /^[A-Z]/.test(name);
}

/** A resolved closed-world receiver. `polymorphic` is true only for `self` — the
 *  receiver could be any concrete subclass, so the understood-set unions the
 *  subclass closure (Template Method). `super`/class-literal/`new` are monomorphic. */
interface ResolvedReceiver {
  readonly className: string;
  readonly side: MethodSide;
  readonly polymorphic: boolean;
}

/** Resolve the receiver to a closed-world `{class, side}` in exactly the high-
 *  confidence cases (spec §4) — else `undefined` (open world). */
function resolveReceiver(ctx: SendContext, world: ClassWorld): ResolvedReceiver | undefined {
  const r = ctx.node.receiver;

  // `self` inside a fully-indexed method — polymorphic over the subclass closure.
  if (isVar(r, 'self')) {
    return ctx.enclosingClass ? { className: ctx.enclosingClass, side: ctx.enclosingSide, polymorphic: true } : undefined;
  }
  // `super` — lookup starts at the superclass and goes UP; not polymorphic.
  if (isVar(r, 'super')) {
    if (!ctx.enclosingClass) {
      return undefined;
    }
    const sup = world.superclassOf(ctx.enclosingClass);
    return sup ? { className: sup, side: ctx.enclosingSide, polymorphic: false } : undefined;
  }

  // A literal class receiver: `OrderedCollection foo` → the class object (class side).
  if (r.kind === NodeKind.Variable && isCapitalized(r.name) && world.isKnownClass(r.name)) {
    return { className: r.name, side: 'class', polymorphic: false };
  }

  // `Foo new` / `Foo basicNew[:]` → an instance of Foo (instance side, monomorphic).
  if (r.kind === NodeKind.Message) {
    const inst =
      (r.messageType === 'unary' && UNARY_INSTANTIATORS.has(r.selector)) ||
      (r.messageType === 'keyword' && KEYWORD_INSTANTIATORS.has(r.selector));
    if (inst && r.receiver.kind === NodeKind.Variable && isCapitalized(r.receiver.name) && world.isKnownClass(r.receiver.name)) {
      return { className: r.receiver.name, side: 'instance', polymorphic: false };
    }
  }

  return undefined;
}

/**
 * The gate (spec §4 truth table). Returns the outcome classification; the harness
 * tallies coverage (¬`unknown-receiver`) and the emit set. EMIT is the single row:
 * a closed-world receiver, the selector absent from its resolved table, the method
 * parses clean, and no escape hatch fired.
 */
export function evaluateMissingSelector(ctx: SendContext, world: ClassWorld): GateResult {
  const selector = ctx.node.selector;
  if (selector === '') {
    return { outcome: 'unknown-receiver' }; // a recovery/empty send — nothing to say
  }

  const resolved = resolveReceiver(ctx, world);
  if (!resolved) {
    return { outcome: 'unknown-receiver' };
  }
  const { className, side, polymorphic } = resolved;
  const tag = { targetClass: className, targetSide: side };

  // §5.2 / §5.5 — reflective sends are never flagged (cheap, receiver-independent).
  if (PERFORM_FAMILY.has(selector)) {
    return { outcome: 'escape-perform', ...tag };
  }
  if (REFLECTIVE_ALLOWLIST.has(selector)) {
    return { outcome: 'escape-allowlist', ...tag };
  }

  // §5.4 — extension / partially-indexed receiver table ⇒ open world.
  if (world.isIncompleteTable(className)) {
    return { outcome: 'escape-incomplete', ...tag };
  }

  // §5.3 — proxy/forwarding by name signal.
  if (PROXY_NAME.test(className)) {
    return { outcome: 'escape-proxy', ...tag };
  }

  // §5.1 — a custom doesNotUnderstand: anywhere below the root ⇒ forwarding.
  const dnu = world.dnuDefiner(className);
  if (dnu !== undefined && dnu !== world.rootDnuClass) {
    return { outcome: 'escape-dnu', ...tag };
  }

  // `self` unions the subclass closure (Template Method); other receivers are exact.
  const table = polymorphic ? world.respondsToSelf(className, side) : world.respondsTo(className, side);
  if (!table) {
    // Chain not fully indexed (e.g. a workspace class rooted in an unknown super).
    return { outcome: 'escape-incomplete', ...tag };
  }
  if (table.has(selector)) {
    return { outcome: 'understood', ...tag };
  }

  // The receiver is closed-world and does NOT understand the selector. Final guards:
  if (!ctx.methodClean) {
    return { outcome: 'method-not-clean', ...tag };
  }
  if (ctx.optedOut) {
    return { outcome: 'escape-optout', ...tag };
  }
  return { outcome: 'emit', ...tag };
}

/** The published-API predicate the spec names (§4) — the boolean over the gate. */
export function shouldEmitMissingSelectorWarning(ctx: SendContext, world: ClassWorld): boolean {
  return evaluateMissingSelector(ctx, world).outcome === 'emit';
}
