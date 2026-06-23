// Server-side `gst` executable resolution (US-414, Slice B / AC2).
//
// Mirrors the client's gstLocator policy (configured setting wins, else PATH) so
// the language server can spawn `gst` for the opt-in diagnostics tier without
// depending on the client. Pure except for the injected `isExecutable` probe, so
// it is unit-tested with a fake filesystem.

import fs from 'node:fs';
import path from 'node:path';

export interface ResolveServerGstOptions {
  /** Value of `smalltalk.gnuSmalltalkPath` (may be empty/undefined). */
  readonly configuredPath?: string;
  /** Contents of PATH (defaults to `process.env.PATH`). */
  readonly pathEnv?: string;
  /** Platform (defaults to `process.platform`); selects candidate exe names. */
  readonly platform?: NodeJS.Platform;
  /** Path delimiter (defaults to `path.delimiter`). */
  readonly delimiter?: string;
  /** Probe whether a candidate path is an executable file (defaults to fs check). */
  readonly isExecutable?: (candidate: string) => boolean;
}

function defaultIsExecutable(candidate: string): boolean {
  try {
    if (!fs.statSync(candidate).isFile()) {
      return false;
    }
    if (process.platform === 'win32') {
      return true;
    }
    fs.accessSync(candidate, fs.constants.X_OK);
    return true;
  } catch {
    return false;
  }
}

function exeNames(platform: NodeJS.Platform): string[] {
  return platform === 'win32' ? ['gst.exe', 'gst'] : ['gst'];
}

/**
 * Resolve the `gst` executable: a non-empty configured path wins (when
 * executable); otherwise PATH is searched. Returns `undefined` when neither
 * yields an executable, so the gst tier stays silently inert (ADR-0001).
 */
export function resolveGst(opts: ResolveServerGstOptions = {}): string | undefined {
  const isExecutable = opts.isExecutable ?? defaultIsExecutable;
  const platform = opts.platform ?? process.platform;
  const delimiter = opts.delimiter ?? path.delimiter;
  const pathEnv = opts.pathEnv ?? process.env.PATH ?? '';

  const configured = (opts.configuredPath ?? '').trim();
  if (configured.length > 0) {
    return isExecutable(configured) ? configured : undefined;
  }

  for (const dir of pathEnv.split(delimiter).filter((d) => d.length > 0)) {
    for (const exe of exeNames(platform)) {
      const candidate = path.join(dir, exe);
      if (isExecutable(candidate)) {
        return candidate;
      }
    }
  }
  return undefined;
}
