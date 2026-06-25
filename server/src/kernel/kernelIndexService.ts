// Kernel index service (US-413, slice B / AC5+AC6; ADR-0002).
//
// Resolves the active kernel tier from the `smalltalk.completion.kernelLibrary`
// strategy and exposes provenance-tagged lookups for the completion provider:
//   - `off`     → no kernel completions,
//   - `bundled` → the shipped gst-3.2.5 reference index,
//   - `auto`    → the user's INSTALLED kernel (discovered + indexed live with
//                 `indexKernelDirectoryToCartridge`), falling back to `bundled`.
// The resolved identity drives the status bar (slice D). Pure logic + Node `fs`;
// no `vscode`. Re-`configure()` on `didChangeConfiguration`.

import type { CartridgeHeader, DialectCartridge, ImplementorRef, SendSite } from '../types/knowledge-base';
import { bundledCartridge, cartridgeToKernelIndex, loadCartridge } from './cartridgeLoader';
import { DEFAULT_COMMON_LOCATIONS, discoverKernelDir } from './discovery';
import { indexKernelDirectoryToCartridge } from './indexer';
import { Provenance, type KernelIndexData } from './model';

export type KernelLibrarySetting = 'auto' | 'bundled' | 'off';

export interface KernelConfig {
  readonly kernelLibrary: KernelLibrarySetting;
  /** `smalltalk.completion.kernelPath` — explicit installed kernel dir. */
  readonly kernelPath?: string;
  /** `smalltalk.gnuSmalltalkPath` — the gst executable (for prefix discovery). */
  readonly gnuSmalltalkPath?: string;
}

/** Resolved identity of the active kernel source (for status + provenance). */
export interface KernelIdentity {
  readonly source: 'installed' | 'bundled' | 'off';
  /** Short human label, e.g. `bundled (gst 3.2.5)`, `installed (gst)`, `off`. */
  readonly label: string;
  readonly library?: string;
  readonly version?: string;
}

export interface KernelSelectorEntry {
  readonly selector: string;
  readonly arity: number;
  readonly provenance: Provenance;
}

export interface KernelClassEntry {
  readonly name: string;
  readonly superclass?: string;
  readonly provenance: Provenance;
  /** Comment prose, present only when the active source carries it (US-415). */
  readonly comment?: string;
}

/** Cartridge header for a live-indexed installed kernel (ADR-0003 Tier-1). The
 *  static source parse can't know the exact version without running `gst`, so it
 *  is left as a placeholder; `builtAt` is stamped per live index. `carriesProse`
 *  is TRUE: the installed cartridge is built locally from the user's OWN `.st`
 *  files and never redistributed, so it may carry class/method comments for hover
 *  (US-415 §4a). The shipped *bundled* cartridge stays facts-only. */
function installedCartridgeHeader(): CartridgeHeader {
  return {
    schema: 1,
    dialect: 'gnu-smalltalk',
    dialectLabel: 'GNU Smalltalk',
    library: 'kernel',
    version: 'installed',
    source: 'gst-source',
    sourceLicense: 'LGPL-2.1-only',
    carriesProse: true,
    tiers: ['classes', 'documentation'],
    builtAt: new Date().toISOString(),
    contentHash: 'unstamped', // US-603 keys the generate-and-cache chain on this.
  };
}

const OFF: KernelIdentity = { source: 'off', label: 'off' };

/** Status-bar label like `reference (gst 3.2.5)` / `installed (gst 3.2.5)`. The
 *  version is appended only when it's a real, known version — the static installed
 *  adapter can't determine it without running `gst`, so it stays `installed (gst)`
 *  until a version becomes available. */
function identityLabel(kind: 'reference' | 'installed', dialect: string, version: string): string {
  const known = version !== '' && version !== 'installed' && /\d/.test(version);
  return known ? `${kind} (${dialect} ${version})` : `${kind} (${dialect})`;
}

export class KernelIndexService {
  private active?: KernelIndexData;
  /** The source cartridge behind `active`, retained so license-gated prose can be
   *  queried (the projected `KernelIndexData` is deliberately facts-only). */
  private activeCartridge?: DialectCartridge;
  private identity_: KernelIdentity = OFF;
  /** Cache of the last live-indexed directory, to avoid re-indexing on every config event. */
  private installedCache?: { readonly dir: string; readonly index: KernelIndexData; readonly cartridge: DialectCartridge };

  constructor(
    /** The bundled reference floor: the committed GST Cartridge #01 projected to
     *  the KernelIndexData the completion service consumes (US-430 convergence). */
    private readonly bundled: KernelIndexData = cartridgeToKernelIndex(bundledCartridge),
    /** Well-known install locations probed in `auto` (overridable for tests). */
    private readonly commonLocations: readonly string[] = DEFAULT_COMMON_LOCATIONS,
  ) {}

  /** (Re)resolve the active kernel source from configuration. Never throws. */
  configure(config: KernelConfig): void {
    if (config.kernelLibrary === 'off') {
      this.active = undefined;
      this.activeCartridge = undefined;
      this.identity_ = OFF;
      return;
    }
    if (config.kernelLibrary === 'auto') {
      const installed = this.tryInstalled(config);
      if (installed) {
        this.active = installed.index;
        this.activeCartridge = installed.cartridge;
        this.identity_ = {
          source: 'installed',
          label: identityLabel('installed', installed.index.dialect, installed.index.version),
          library: installed.index.library,
          version: installed.index.version,
        };
        return;
      }
    }
    // `bundled`, or `auto` with no usable installation: the Tier-2 frozen
    // reference floor (ADR-0003). Labelled "reference" to distinguish it from a
    // version-correct installed source in the status bar. Facts-only.
    this.active = this.bundled;
    this.activeCartridge = bundledCartridge;
    this.identity_ = {
      source: 'bundled',
      label: identityLabel('reference', this.bundled.dialect, this.bundled.version),
      library: this.bundled.library,
      version: this.bundled.version,
    };
  }

  private tryInstalled(config: KernelConfig): { index: KernelIndexData; cartridge: DialectCartridge } | undefined {
    const dir = discoverKernelDir(
      { kernelPath: config.kernelPath, gnuSmalltalkPath: config.gnuSmalltalkPath },
      this.commonLocations,
    );
    if (!dir) {
      return undefined;
    }
    if (this.installedCache?.dir === dir) {
      return { index: this.installedCache.index, cartridge: this.installedCache.cartridge };
    }
    let cartridge: DialectCartridge;
    let index: KernelIndexData;
    try {
      // Tier-1 installed adapter: parse the source dir into cartridge shape, then
      // project to the completion model — like-for-like with the floor (ADR-0003).
      cartridge = indexKernelDirectoryToCartridge(dir, installedCartridgeHeader());
      if (Object.keys(cartridge.classes).length === 0) {
        return undefined; // empty/non-kernel dir → fall back to the floor
      }
      index = cartridgeToKernelIndex(cartridge, 'installed');
    } catch {
      return undefined; // never throw from configuration
    }
    this.installedCache = { dir, index, cartridge };
    return { index, cartridge };
  }

  get identity(): KernelIdentity {
    return this.identity_;
  }

  get isEnabled(): boolean {
    return this.active !== undefined;
  }

  private get provenance(): Provenance {
    return this.identity_.source === 'installed' ? Provenance.InstalledKernel : Provenance.BundledKernel;
  }

  /** Class names in the active kernel (sorted), tagged with provenance. */
  classes(): KernelClassEntry[] {
    if (!this.active) {
      return [];
    }
    const prov = this.provenance;
    return Object.entries(this.active.classes).map(([name, c]) => ({
      name,
      provenance: prov,
      ...(c.superclass !== undefined ? { superclass: c.superclass } : {}),
    }));
  }

  /** Unique selectors across every class in the active kernel, with provenance. */
  selectors(): KernelSelectorEntry[] {
    if (!this.active) {
      return [];
    }
    const prov = this.provenance;
    const arities = new Map<string, number>();
    for (const c of Object.values(this.active.classes)) {
      for (const s of [...c.instanceSelectors, ...c.classSelectors]) {
        if (!arities.has(s.selector)) {
          arities.set(s.selector, s.arity);
        }
      }
    }
    return [...arities.keys()]
      .sort()
      .map((selector) => ({ selector, arity: arities.get(selector) ?? 0, provenance: prov }));
  }

  /** Provenance of the active kernel source (hover prose gate). */
  get activeProvenance(): Provenance {
    return this.provenance;
  }

  /** Is `name` a class in the active kernel? */
  hasClass(name: string): boolean {
    return this.active?.classes[name] !== undefined;
  }

  /** Immediate superclass of `name` in the active kernel, or undefined. */
  superclassOf(name: string): string | undefined {
    return this.active?.classes[name]?.superclass;
  }

  /** Whether the active source may surface comment prose (US-415 §4a gate). */
  private get carriesProse(): boolean {
    return this.activeCartridge?.header.carriesProse === true;
  }

  /** Kernel classes implementing `selector` (instance or class side), with
   *  provenance and — when the active source carries prose — the method comment. */
  implementorsOf(selector: string): KernelClassEntry[] {
    const cart = this.activeCartridge;
    if (!cart) {
      return [];
    }
    const prov = this.provenance;
    const withProse = this.carriesProse;
    const out: KernelClassEntry[] = [];
    for (const c of Object.values(cart.classes)) {
      const sig =
        c.instanceMethods.find((m) => m.selector === selector) ??
        c.classMethods.find((m) => m.selector === selector);
      if (sig) {
        const comment = withProse ? sig.documentation?.text : undefined;
        out.push({
          name: c.name, // simple name (matches the projected view + installed tier)
          provenance: prov,
          ...(c.superclass != null ? { superclass: c.superclass } : {}),
          ...(comment ? { comment } : {}),
        });
      }
    }
    return out;
  }

  /** The active cartridge's identity (dialect + version), for cross-reference
   *  provenance + synthetic URIs (US-423). Undefined when no kernel is active. */
  get cartridgeId(): { dialect: string; version: string } | undefined {
    const h = this.activeCartridge?.header;
    return h ? { dialect: h.dialect, version: h.version } : undefined;
  }

  /** Cartridge send sites of `selector` — the frozen kernel half of "Senders of"
   *  (US-423). Reads the precomputed `crossReference` tier; `[]` when absent. */
  crossReferenceSenders(selector: string): readonly SendSite[] {
    if (!this.activeCartridge) {
      return [];
    }
    return loadCartridge(this.activeCartridge).sendersOf(selector);
  }

  /** Cartridge implementors of `selector` — the frozen kernel half of
   *  "Implementors of" (US-423). O(1) off the precomputed tier (derived if absent). */
  crossReferenceImplementors(selector: string): readonly ImplementorRef[] {
    if (!this.activeCartridge) {
      return [];
    }
    return loadCartridge(this.activeCartridge).implementorsOf(selector);
  }

  /** Class comment for `name`, only when the active source carries prose (else
   *  undefined). The bundled reference cartridge is facts-only, so it returns
   *  undefined there by construction. */
  classComment(name: string): string | undefined {
    if (!this.carriesProse) {
      return undefined;
    }
    return this.activeCartridge?.classes[name]?.documentation?.text;
  }
}
