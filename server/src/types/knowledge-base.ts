// server/src/types/knowledge-base.ts
//
// Dialect Cartridge — the compiled, frozen, pure-JSON knowledge unit that the
// Offline Knowledge Graph loads per dialect. GNU Smalltalk 3.2.5 is Cartridge #01.
//
// Evolves the ADR-0002 neutral model (server/src/kernel/model.ts) into the full
// "Console & Cartridges" foundation. INVARIANTS:
//   - Pure JSON: scalars, arrays, records only. No class instances, no functions,
//     no object cycles. All graph edges are *string ids*, never pointers.
//   - Resolved facts only: NO ASTs. Inherited members are NOT denormalized down
//     the chain — the chain is walked at index-build time, not stored per class.
//   - Class-side vs instance-side is ALWAYS explicit, never inferred at use.
//
// See docs/decisions/0002-kernel-symbol-sourcing.md and EPIC-005 (US-430).

/** Bumped on any breaking shape change; gates migration of on-disk cartridges. */
export type CartridgeSchemaVersion = 1;

/** JSON-serializable leaf/value types — enforces the "pure JSON" invariant in the
 *  open `metadata`/`taxonomy` bags at the type level. */
export type JsonScalar = string | number | boolean | null;
export type JsonValue = JsonScalar | JsonValue[] | { [key: string]: JsonValue };

/** Fully-qualified, rebuild-stable class identifier and primary join key.
 *  GST: "<Namespace>.<Class>" (e.g. "Smalltalk.Object").
 *  Flat dialects: "<Class>" (e.g. "ProtoObject"). Unique within a cartridge. */
export type ClassId = string;

/** A selector exactly as written: "do:", "at:put:", "+", "printOn:", "yourself". */
export type Selector = string;

/** Which method table a member lives in. Never inferred downstream. */
export type MethodSide = 'instance' | 'class';

/** Open dialect axis — known ids are conveniences, not a closed set. */
export type DialectId =
  | 'gnu-smalltalk' | 'pharo' | 'squeak' | 'cuis' | 'gemstone' | (string & {});

/** Payload tiers — let a cartridge ship lean (facts) or full (facts + call graph),
 *  and let the loader lazy-load the heavy tier. */
export type CartridgeTier = 'classes' | 'documentation' | 'crossReference';

export type ClassKind = 'class' | 'trait' | 'metaclass' | 'extension';

/** Cartridge identity + provenance + the licensing gate for prose. */
export interface CartridgeHeader {
  readonly schema: CartridgeSchemaVersion;
  readonly dialect: DialectId;
  /** Display label, e.g. "GNU Smalltalk", "Pharo". */
  readonly dialectLabel: string;
  /** Library/image identity, e.g. "kernel", "Pharo64-12.0". */
  readonly library: string;
  /** Version of the *source* library, e.g. "3.2.5", "12.0". */
  readonly version: string;
  /** How the facts were obtained. Drives completeness assumptions in the linter. */
  readonly source: 'gst-source' | 'image-export' | 'curated';
  /** SPDX id of the source the facts/prose derive from. */
  readonly sourceLicense: string;
  /** When false, every `documentation` field MUST be absent (facts-only build,
   *  e.g. the LGPL-2.1 GST kernel — see ADR-0002 Licensing). */
  readonly carriesProse: boolean;
  /** Tiers actually present in this file. */
  readonly tiers: readonly CartridgeTier[];
  /** ISO-8601 build timestamp. */
  readonly builtAt: string;
  /** Content hash over the fact tables — cache key for the in-memory index. */
  readonly contentHash: string;
}

/** License-gated prose. Present only when header.carriesProse === true. */
export interface Documentation {
  readonly text: string;
  /** Attribution/notice surfaced in hover UI where the license requires it. */
  readonly attribution?: string;
}

/** A locally-defined method's resolved signature (no body, no AST). */
export interface SelectorSignature {
  readonly selector: Selector;
  /** Arg count. Derivable from the selector but stored for O(1) ranking/snippets. */
  readonly arity: number;
  /** Keyword parts for snippet insertion: ["at:","put:"]; unary/binary => [selector]. */
  readonly keywords: readonly string[];
  /** Organizational tag: GST method category / Pharo protocol. Display only. */
  readonly protocol?: string;
  /** Raw pragma facts, e.g. "<primitive: 60>", "<category: 'accessing'>". */
  readonly pragmas?: readonly string[];
  /** License-gated method comment. */
  readonly documentation?: Documentation;
}

/** Pharo/Squeak trait composition as resolved facts — affects which selectors a
 *  class responds to, so it is first-class, not metadata. GST omits this entirely. */
export interface TraitUse {
  readonly trait: ClassId;
  /** Conflict resolution: alias newSelector -> originalSelector. */
  readonly aliases?: Readonly<Record<Selector, Selector>>;
  /** Selectors removed from the trait in this composition. */
  readonly exclusions?: readonly Selector[];
}

/** One class/trait as resolved facts. Locally-defined members only. */
export interface ClassFact {
  readonly id: ClassId;
  readonly name: string;
  readonly kind: ClassKind;
  /** Superclass id, or null for a dialect root (Object in GST, ProtoObject in Pharo). */
  readonly superclass: ClassId | null;
  /** Variable names, split by side. Names are facts; values are never stored. */
  readonly instanceVariables: readonly string[];
  readonly classVariables: readonly string[];
  readonly classInstanceVariables: readonly string[];
  readonly sharedPools?: readonly string[];
  /** Locally-defined methods only — inheritance is resolved at index-build time. */
  readonly instanceMethods: readonly SelectorSignature[];
  readonly classMethods: readonly SelectorSignature[];
  /** Trait composition (Pharo/Squeak). Absent for trait-less dialects. */
  readonly traitComposition?: readonly TraitUse[];
  /** Neutral organizational taxonomy. Each dialect fills its OWN keys with no
   *  schema change: GST -> {namespace, category}; Pharo -> {package, tag}. */
  readonly taxonomy: Readonly<Record<string, string>>;
  readonly documentation?: Documentation;
  /** Escape bag for genuinely dialect-unique, display-only facts. */
  readonly metadata?: Readonly<Record<string, JsonScalar | readonly JsonScalar[]>>;
}

/** A reference to a method definition (the answer to "Implementors of"). */
export interface ImplementorRef {
  readonly inClass: ClassId;
  readonly side: MethodSide;
}

/** A single message-send fact (the answer to "Senders of"). No AST — just the
 *  coordinates needed to render a jump target. */
export interface SendSite {
  readonly inClass: ClassId;
  readonly side: MethodSide;
  readonly inSelector: Selector;
  /** 0-based line within that method's source for jump-to. */
  readonly line: number;
  /** Cheap, best-effort static receiver hint — powers honest UX, never proof. */
  readonly receiverHint?: 'self' | 'super' | ClassId | null;
}

/** Pre-resolved cross-reference graph. Heaviest tier; gate behind 'crossReference'.
 *  `implementors` is derivable from class tables but frozen here for O(1) lookup. */
export interface CrossReference {
  readonly implementors: Readonly<Record<Selector, readonly ImplementorRef[]>>;
  readonly senders: Readonly<Record<Selector, readonly SendSite[]>>;
}

/** The compiled cartridge. `classes` is a record keyed by ClassId for O(1) joins. */
export interface DialectCartridge {
  readonly header: CartridgeHeader;
  readonly classes: Readonly<Record<ClassId, ClassFact>>;
  readonly crossReference?: CrossReference;
}
