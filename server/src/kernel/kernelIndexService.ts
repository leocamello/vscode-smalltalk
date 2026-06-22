// Kernel index service (US-413, slice B / AC5+AC6; ADR-0002).
//
// Resolves the active kernel tier from the `smalltalk.completion.kernelLibrary`
// strategy and exposes provenance-tagged lookups for the completion provider:
//   - `off`     → no kernel completions,
//   - `bundled` → the shipped gst-3.2.5 reference index,
//   - `auto`    → the user's INSTALLED kernel (discovered + indexed live with the
//                 same `indexKernelDirectory`), falling back to `bundled`.
// The resolved identity drives the status bar (slice D). Pure logic + Node `fs`;
// no `vscode`. Re-`configure()` on `didChangeConfiguration`.

import type { CartridgeHeader } from '../types/knowledge-base';
import { bundledCartridge, cartridgeToKernelIndex } from './cartridgeLoader';
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
}

/** Cartridge header for a live-indexed installed kernel (ADR-0003 Tier-1). The
 *  static source parse can't know the exact version without running `gst`, so it
 *  is left as a placeholder; `builtAt` is stamped per live index. Facts-only. */
function installedCartridgeHeader(): CartridgeHeader {
  return {
    schema: 1,
    dialect: 'gnu-smalltalk',
    dialectLabel: 'GNU Smalltalk',
    library: 'kernel',
    version: 'installed',
    source: 'gst-source',
    sourceLicense: 'LGPL-2.1-only',
    carriesProse: false,
    tiers: ['classes'],
    builtAt: new Date().toISOString(),
    contentHash: 'unstamped', // US-603 keys the generate-and-cache chain on this.
  };
}

const OFF: KernelIdentity = { source: 'off', label: 'off' };

export class KernelIndexService {
  private active?: KernelIndexData;
  private identity_: KernelIdentity = OFF;
  /** Cache of the last live-indexed directory, to avoid re-indexing on every config event. */
  private installedCache?: { readonly dir: string; readonly index: KernelIndexData };

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
      this.identity_ = OFF;
      return;
    }
    if (config.kernelLibrary === 'auto') {
      const installed = this.tryInstalled(config);
      if (installed) {
        this.active = installed;
        this.identity_ = {
          source: 'installed',
          label: `installed (${installed.dialect})`,
          library: installed.library,
          version: installed.version,
        };
        return;
      }
    }
    // `bundled`, or `auto` with no usable installation: the Tier-2 frozen
    // reference floor (ADR-0003). Labelled "frozen reference" to distinguish it
    // from a version-correct installed source in the status bar.
    this.active = this.bundled;
    this.identity_ = {
      source: 'bundled',
      label: `frozen reference (${this.bundled.dialect} ${this.bundled.version})`,
      library: this.bundled.library,
      version: this.bundled.version,
    };
  }

  private tryInstalled(config: KernelConfig): KernelIndexData | undefined {
    const dir = discoverKernelDir(
      { kernelPath: config.kernelPath, gnuSmalltalkPath: config.gnuSmalltalkPath },
      this.commonLocations,
    );
    if (!dir) {
      return undefined;
    }
    if (this.installedCache?.dir === dir) {
      return this.installedCache.index;
    }
    let index: KernelIndexData;
    try {
      // Tier-1 installed adapter: parse the source dir into cartridge shape, then
      // project to the completion model — like-for-like with the floor (ADR-0003).
      const cartridge = indexKernelDirectoryToCartridge(dir, installedCartridgeHeader());
      if (Object.keys(cartridge.classes).length === 0) {
        return undefined; // empty/non-kernel dir → fall back to the floor
      }
      index = cartridgeToKernelIndex(cartridge, 'installed');
    } catch {
      return undefined; // never throw from configuration
    }
    this.installedCache = { dir, index };
    return index;
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
}
