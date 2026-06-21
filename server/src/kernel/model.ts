// Dialect-neutral kernel-index model (US-413, ADR-0002).
//
// The shape of a "standard library" index — classes, superclass links, and
// selectors + arity for one Smalltalk library. It is deliberately dialect- and
// source-agnostic: the same model is produced by the bundled build-time
// generator (GST 3.2.5 from `.st` sources) and, later, by other source adapters
// (a live installed kernel directory, or an image-based reflective export for
// Pharo/Squeak). Pure data — no `vscode`, no Node.
//
// Licensing (LGPL-2.1): this model stores FACTS ONLY — class/selector names,
// arity, and superclass links. It must never carry method-comment prose or
// bodies. A test (`kernelIndex.test.ts`) enforces the absence of prose fields.

/** How a kernel index was sourced. `bundled` ships in the VSIX; `installed` is
 *  built live from the user's actual Smalltalk installation. */
export type KernelSource = 'bundled' | 'installed';

/** A single selector and its arity (0 = unary, 1 = binary, n = keyword parts). */
export interface KernelSelector {
  readonly selector: string;
  readonly arity: number;
}

/** The facts known about one class. The full superclass *chain* is resolved
 *  transitively over the index, so only the immediate superclass is stored. */
export interface KernelClass {
  readonly superclass?: string;
  readonly instanceSelectors: KernelSelector[];
  readonly classSelectors: KernelSelector[];
}

/** Self-describing identity of an index (surfaced in the status bar + provenance). */
export interface KernelIndexHeader {
  /** Dialect family, e.g. `gst`. */
  readonly dialect: string;
  /** Library identifier, e.g. `gst-3.2.5`. */
  readonly library: string;
  /** Library version, e.g. `3.2.5`. */
  readonly version: string;
  readonly source: KernelSource;
}

/** A complete kernel index: identity header + derived counts + class facts.
 *  In the serialized form, `classes` keys and every selector list are sorted. */
export interface KernelIndexData extends KernelIndexHeader {
  readonly classCount: number;
  readonly selectorCount: number;
  readonly classes: Record<string, KernelClass>;
}

/** Where a completion came from — drives ranking and the provenance label.
 *  (Consumed by the completion provider in a later slice.) */
export enum Provenance {
  Workspace = 'workspace',
  InstalledKernel = 'installed',
  BundledKernel = 'bundled',
}
