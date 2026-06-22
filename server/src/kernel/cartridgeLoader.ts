// Console cartridge loader (US-430, slice B; EPIC-005, ADR-0003).
//
// Loads a frozen, pure-JSON DialectCartridge (GST 3.2.5 is Cartridge #01) and
// builds the in-memory, resolved views the Console features consume:
//   - classOf(id)            — O(1) class fact lookup by ClassId,
//   - methodTableOf(id,side) — own ∪ inherited (superclass chain) ∪ trait-composed,
//                              with own > trait and subclass > superclass shadowing,
//   - implementorsOf / sendersOf — the "Implementors/Senders of" cross-reference
//                              (read from the precomputed `crossReference` tier,
//                              implementors derived from class tables if absent).
// Pure data — no `vscode`, no Node `fs`; the committed JSON is inlined by esbuild
// (mirrors bundledIndex.ts). Derived views are memoized by `header.contentHash`.
//
// `cartridgeToKernelIndex` projects a cartridge down to the US-413 KernelIndexData
// so the existing completion service consumes a cartridge unchanged (convergence,
// AC7). The cartridge is a strict superset of that model, so the projection drops
// crossReference/traits/taxonomy/protocol and maps ClassId keys to simple names.

import data from '../../data/cartridges/gst-3.2.5-cartridge.json';
import type {
  ClassFact,
  ClassId,
  DialectCartridge,
  ImplementorRef,
  MethodSide,
  SelectorSignature,
  SendSite,
  Selector,
} from '../types/knowledge-base';
import type { KernelClass, KernelIndexData, KernelSelector, KernelSource } from './model';

/** The committed GST Cartridge #01, inlined by esbuild (mirrors bundledIndex.ts). */
export const bundledCartridge: DialectCartridge = data as unknown as DialectCartridge;

/** A resolved method-table entry: a locally-defined signature plus the class and
 *  side that actually defines it (after inheritance/trait resolution). */
export interface ResolvedMethod {
  readonly inClass: ClassId;
  readonly side: MethodSide;
  readonly signature: SelectorSignature;
}

/** In-memory, resolved views over one cartridge. Pure data; `vscode`-free. */
export interface LoadedCartridge {
  readonly cartridge: DialectCartridge;
  /** O(1) class fact lookup by id. */
  classOf(id: ClassId): ClassFact | undefined;
  /** Resolved selector → defining method for a class on `side` (default instance):
   *  own ∪ inherited via the superclass chain ∪ trait-composed. Own methods shadow
   *  trait methods; a subclass shadows its superclass. */
  methodTableOf(id: ClassId, side?: MethodSide): ReadonlyMap<Selector, ResolvedMethod>;
  /** "Implementors of" — every class/side that defines `selector`. */
  implementorsOf(selector: Selector): readonly ImplementorRef[];
  /** "Senders of" — every recorded send site of `selector`. */
  sendersOf(selector: Selector): readonly SendSite[];
}

/** Keyword parts of a selector: `at:put:` → [`at:`,`put:`]; unary/binary → [sel]. */
function keywordsOf(selector: Selector): string[] {
  return selector.match(/[^:]+:/g) ?? [selector];
}

/** Simple class name from a (possibly namespaced) ClassId: `Smalltalk.Object` → `Object`. */
function simpleName(id: ClassId): string {
  const dot = id.lastIndexOf('.');
  return dot >= 0 ? id.slice(dot + 1) : id;
}

class LoadedCartridgeImpl implements LoadedCartridge {
  private readonly tableCache = new Map<string, ReadonlyMap<Selector, ResolvedMethod>>();
  private derivedImplementors?: Map<Selector, ImplementorRef[]>;

  constructor(readonly cartridge: DialectCartridge) {}

  classOf(id: ClassId): ClassFact | undefined {
    return this.cartridge.classes[id];
  }

  methodTableOf(id: ClassId, side: MethodSide = 'instance'): ReadonlyMap<Selector, ResolvedMethod> {
    const cacheKey = `${side}|${id}`;
    const cached = this.tableCache.get(cacheKey);
    if (cached) {
      return cached;
    }
    // Walk the superclass chain (cycle-guarded): [id, super, …, root].
    const chain: ClassFact[] = [];
    const seen = new Set<ClassId>();
    let cursor: ClassId | null = id;
    while (cursor && !seen.has(cursor)) {
      seen.add(cursor);
      const fact: ClassFact | undefined = this.cartridge.classes[cursor];
      if (!fact) {
        break;
      }
      chain.push(fact);
      cursor = fact.superclass;
    }
    // Apply root → subclass so nearer definitions overwrite; within a class apply
    // trait methods first, then own methods (own shadows trait).
    const table = new Map<Selector, ResolvedMethod>();
    for (let i = chain.length - 1; i >= 0; i--) {
      const fact = chain[i] as ClassFact;
      this.applyTraits(table, fact, side);
      for (const sig of side === 'class' ? fact.classMethods : fact.instanceMethods) {
        table.set(sig.selector, { inClass: fact.id, side, signature: sig });
      }
    }
    this.tableCache.set(cacheKey, table);
    return table;
  }

  private applyTraits(table: Map<Selector, ResolvedMethod>, fact: ClassFact, side: MethodSide): void {
    for (const use of fact.traitComposition ?? []) {
      const trait = this.cartridge.classes[use.trait];
      if (!trait) {
        continue;
      }
      const methods = side === 'class' ? trait.classMethods : trait.instanceMethods;
      const excluded = new Set(use.exclusions ?? []);
      for (const sig of methods) {
        if (!excluded.has(sig.selector)) {
          table.set(sig.selector, { inClass: trait.id, side, signature: sig });
        }
      }
      for (const [alias, original] of Object.entries(use.aliases ?? {})) {
        const sig = methods.find((m) => m.selector === original);
        if (sig) {
          table.set(alias, {
            inClass: trait.id,
            side,
            signature: { ...sig, selector: alias, keywords: keywordsOf(alias) },
          });
        }
      }
    }
  }

  implementorsOf(selector: Selector): readonly ImplementorRef[] {
    const precomputed = this.cartridge.crossReference?.implementors[selector];
    if (precomputed) {
      return precomputed;
    }
    return this.deriveImplementorIndex().get(selector) ?? [];
  }

  sendersOf(selector: Selector): readonly SendSite[] {
    return this.cartridge.crossReference?.senders[selector] ?? [];
  }

  /** Lazily derive implementors from the class tables (used only when a cartridge
   *  ships without the `crossReference` tier). */
  private deriveImplementorIndex(): Map<Selector, ImplementorRef[]> {
    if (this.derivedImplementors) {
      return this.derivedImplementors;
    }
    const index = new Map<Selector, ImplementorRef[]>();
    const add = (selector: Selector, ref: ImplementorRef): void => {
      const refs = index.get(selector);
      if (refs) {
        refs.push(ref);
      } else {
        index.set(selector, [ref]);
      }
    };
    for (const fact of Object.values(this.cartridge.classes)) {
      for (const sig of fact.instanceMethods) {
        add(sig.selector, { inClass: fact.id, side: 'instance' });
      }
      for (const sig of fact.classMethods) {
        add(sig.selector, { inClass: fact.id, side: 'class' });
      }
    }
    this.derivedImplementors = index;
    return index;
  }
}

/** Memo of loaded cartridges, keyed by the content hash of the fact tables. The
 *  identity guard rebuilds rather than returning a stale view if two distinct
 *  cartridges share a placeholder hash (`contentHash` is stamped in slice D). */
const loadedByHash = new Map<string, LoadedCartridge>();

/** Build (or return the memoized) resolved views over `cartridge`. */
export function loadCartridge(cartridge: DialectCartridge): LoadedCartridge {
  const key = cartridge.header.contentHash;
  const hit = loadedByHash.get(key);
  if (hit && hit.cartridge === cartridge) {
    return hit;
  }
  const loaded = new LoadedCartridgeImpl(cartridge);
  loadedByHash.set(key, loaded);
  return loaded;
}

/** Short dialect code for the legacy KernelIndex model (`gnu-smalltalk` → `gst`).
 *  The neutral model predates the cartridge's verbose DialectId vocabulary. */
const DIALECT_SHORT: Readonly<Record<string, string>> = { 'gnu-smalltalk': 'gst' };
function shortDialect(dialect: string): string {
  return DIALECT_SHORT[dialect] ?? dialect;
}

function projectSelectors(methods: readonly SelectorSignature[]): KernelSelector[] {
  return methods
    .map((m) => ({ selector: m.selector, arity: m.arity }))
    .sort((a, b) => (a.selector < b.selector ? -1 : a.selector > b.selector ? 1 : 0));
}

/** Project a cartridge down to the US-413 KernelIndexData the completion service
 *  consumes (convergence, AC7). Drops crossReference/traits/taxonomy/protocol,
 *  keys classes by simple name, and maps superclass ClassId → simple name. */
export function cartridgeToKernelIndex(
  cartridge: DialectCartridge,
  source: KernelSource = 'bundled',
): KernelIndexData {
  const facts = cartridge.classes;
  const nameOf = (id: ClassId): string => facts[id]?.name ?? simpleName(id);

  // Simple class names are unique within a cartridge (ADR-0003); key by name.
  const byName = new Map<string, ClassFact>();
  for (const fact of Object.values(facts)) {
    byName.set(fact.name, fact);
  }

  const classes: Record<string, KernelClass> = {};
  let selectorCount = 0;
  for (const name of [...byName.keys()].sort()) {
    const fact = byName.get(name) as ClassFact;
    const instanceSelectors = projectSelectors(fact.instanceMethods);
    const classSelectors = projectSelectors(fact.classMethods);
    selectorCount += instanceSelectors.length + classSelectors.length;
    classes[name] = {
      ...(fact.superclass !== null ? { superclass: nameOf(fact.superclass) } : {}),
      instanceSelectors,
      classSelectors,
    };
  }

  const dialect = shortDialect(cartridge.header.dialect);
  const version = cartridge.header.version;
  return {
    dialect,
    library: `${dialect}-${version}`,
    version,
    source,
    classCount: Object.keys(classes).length,
    selectorCount,
    classes,
  };
}
