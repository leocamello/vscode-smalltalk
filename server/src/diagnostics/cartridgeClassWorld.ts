// SPIKE-01 — a `ClassWorld` over a loaded cartridge ∪ a workspace class map.
//
// Resolves the closed-world facts the unknown-selector gate queries: the union of
// the workspace's parsed method tables and the cartridge's resolved tables
// (own ∪ inherited ∪ traits), walking the superclass chain to a known root. The
// crucial correctness point: a *class object* responds not only to its class-side
// methods but to the metaclass protocol (`Behavior`/`ClassDescription`/`Class`
// instance methods — `new`, `name`, `category`, …), so class-side resolution
// unions `methodTableOf(Class, instance)`; without this, `Foo new` itself would be
// a false positive. Pure data — no `vscode`, no Node `fs`.

import type { LoadedCartridge } from '../kernel/cartridgeLoader';
import type { ClassId, MethodSide } from '../types/knowledge-base';
import type { ClassWorld } from './unknownSelectorGate';

/** A workspace class as parsed from corpus source (facts only). */
export interface WorkspaceClass {
  readonly superclass?: string;
  readonly instance: ReadonlySet<string>;
  readonly classSel: ReadonlySet<string>;
  /** True when the class is known ONLY through an `extend`/`methodsFor:` chunk
   *  (no full `subclass:` definition seen) — its table may be incomplete. */
  readonly extensionOnly: boolean;
}

/** Simple name from a (possibly namespaced) ClassId: `Smalltalk.Object` → `Object`. */
function simpleName(id: ClassId): string {
  const dot = id.lastIndexOf('.');
  return dot >= 0 ? id.slice(dot + 1) : id;
}

/** The GST dialect root carrying the default error `doesNotUnderstand:`. */
const ROOT_DNU_CLASS = 'Object';

export function cartridgeClassWorld(
  loaded: LoadedCartridge,
  workspace: ReadonlyMap<string, WorkspaceClass> = new Map(),
): ClassWorld {
  // simple name → ClassId (simple names are unique within a cartridge, ADR-0003).
  const nameToId = new Map<string, ClassId>();
  for (const id of Object.keys(loaded.cartridge.classes)) {
    nameToId.set(simpleName(id), id);
  }

  // The metaclass protocol: instance methods of Class ∪ its ancestors
  // (ClassDescription, Behavior, Object) — what every class object responds to.
  let metaclassProtocol: ReadonlySet<string> | undefined;
  const getMetaclassProtocol = (): ReadonlySet<string> => {
    if (metaclassProtocol) {
      return metaclassProtocol;
    }
    const classId = nameToId.get('Class');
    metaclassProtocol = classId ? new Set(loaded.methodTableOf(classId, 'instance').keys()) : new Set<string>();
    return metaclassProtocol;
  };

  const isCartridge = (name: string): boolean => nameToId.has(name);

  // Direct-subclasses index (workspace ∪ cartridge), by simple name. Built lazily;
  // powers the `self` subclass-closure (Template Method) union.
  let childrenIndex: Map<string, string[]> | undefined;
  const childrenOf = (name: string): readonly string[] => {
    if (!childrenIndex) {
      childrenIndex = new Map<string, string[]>();
      const link = (child: string, parent: string | undefined): void => {
        if (!parent) {
          return;
        }
        const kids = childrenIndex!.get(parent) ?? [];
        kids.push(child);
        childrenIndex!.set(parent, kids);
      };
      for (const id of Object.keys(loaded.cartridge.classes)) {
        const fact = loaded.cartridge.classes[id];
        link(simpleName(id), fact?.superclass ? simpleName(fact.superclass) : undefined);
      }
      for (const [name, ws] of workspace) {
        link(name, ws.superclass);
      }
    }
    return childrenIndex.get(name) ?? [];
  };

  /** Locally-defined selectors of `name` on `side` (workspace own ∪ cartridge own,
   *  NOT inherited). For the subclass-closure union. */
  const ownSelectors = (name: string): { instance: ReadonlySet<string>; class: ReadonlySet<string> } => {
    const inst = new Set<string>();
    const cls = new Set<string>();
    const ws = workspace.get(name);
    if (ws) {
      for (const s of ws.instance) inst.add(s);
      for (const s of ws.classSel) cls.add(s);
    }
    const id = nameToId.get(name);
    if (id) {
      const fact = loaded.cartridge.classes[id];
      for (const m of fact?.instanceMethods ?? []) inst.add(m.selector);
      for (const m of fact?.classMethods ?? []) cls.add(m.selector);
    }
    return { instance: inst, class: cls };
  };

  /** Selectors `name` responds to on `side` — own ∪ inherited ∪ traits, walking the
   *  workspace chain into the cartridge (which resolves the rest to root). For the
   *  class side, ∪ the metaclass protocol. `undefined` ⇒ chain not fully indexed. */
  const baseRespondsTo = (name: string, side: MethodSide): Set<string> | undefined => {
    const acc = new Set<string>();
    let cur: string | undefined = name;
    const seen = new Set<string>();
    while (cur && !seen.has(cur)) {
      seen.add(cur);
      const ws = workspace.get(cur);
      if (ws) {
        for (const s of side === 'instance' ? ws.instance : ws.classSel) {
          acc.add(s);
        }
      }
      const id = nameToId.get(cur);
      if (id) {
        for (const k of loaded.methodTableOf(id, side).keys()) {
          acc.add(k);
        }
        if (side === 'class') {
          for (const k of getMetaclassProtocol()) {
            acc.add(k);
          }
        }
        return acc; // cartridge resolved this class + all ancestors
      }
      if (!ws) {
        return undefined; // an unknown class in the chain → open world
      }
      cur = ws.superclass;
    }
    return undefined; // workspace chain ended without reaching a known root
  };

  return {
    rootDnuClass: ROOT_DNU_CLASS,

    isKnownClass(name) {
      return workspace.has(name) || isCartridge(name);
    },

    superclassOf(name) {
      const ws = workspace.get(name);
      if (ws) {
        return ws.superclass;
      }
      const id = nameToId.get(name);
      const sup = id ? loaded.cartridge.classes[id]?.superclass : undefined;
      return sup ? simpleName(sup) : undefined;
    },

    isIncompleteTable(name) {
      const ws = workspace.get(name);
      return ws?.extensionOnly === true && !isCartridge(name);
    },

    respondsTo(name, side: MethodSide) {
      return baseRespondsTo(name, side);
    },

    respondsToSelf(name, side: MethodSide) {
      const base = baseRespondsTo(name, side);
      if (!base) {
        return undefined; // open world — don't widen an already-unknown chain
      }
      // `self` could be any concrete subclass: ∪ each descendant's OWN methods
      // (the Template Method pattern — abstract super, concrete subclass impls).
      const acc = new Set(base);
      const stack = [...childrenOf(name)];
      const seen = new Set<string>([name]);
      while (stack.length > 0) {
        const sub = stack.pop() as string;
        if (seen.has(sub)) {
          continue;
        }
        seen.add(sub);
        for (const s of side === 'instance' ? ownSelectors(sub).instance : ownSelectors(sub).class) {
          acc.add(s);
        }
        stack.push(...childrenOf(sub));
      }
      return acc;
    },

    dnuDefiner(name) {
      let cur: string | undefined = name;
      const seen = new Set<string>();
      while (cur && !seen.has(cur)) {
        seen.add(cur);
        const ws = workspace.get(cur);
        if (ws?.instance.has('doesNotUnderstand:')) {
          return cur;
        }
        const id = nameToId.get(cur);
        if (id) {
          const m = loaded.methodTableOf(id, 'instance').get('doesNotUnderstand:');
          return m ? simpleName(m.inClass) : undefined;
        }
        if (!ws) {
          return undefined;
        }
        cur = ws.superclass;
      }
      return undefined;
    },
  };
}
