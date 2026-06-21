// Installed GST kernel-directory discovery (US-413, slice B / AC6; ADR-0002).
//
// Locates a real GNU Smalltalk kernel source directory (loose `.st` files) so
// the `auto` strategy can index the user's ACTUAL kernel. All filesystem-based —
// never spawns `gst` (honours ADR-0001's no-runtime principle). Precedence:
//   1. an explicit `kernelPath` override,
//   2. the prefix derived from the `smalltalk.gnuSmalltalkPath` executable, then
//   3. common install locations.
// Returns the first directory that looks like a kernel, else `undefined`.

import fs from 'node:fs';
import path from 'node:path';

export interface DiscoveryOptions {
  /** Explicit override — `smalltalk.completion.kernelPath`. */
  readonly kernelPath?: string;
  /** The `gst` executable path — `smalltalk.gnuSmalltalkPath`. */
  readonly gnuSmalltalkPath?: string;
}

/** Well-known install locations (overridable for tests). */
export const DEFAULT_COMMON_LOCATIONS: readonly string[] = [
  '/usr/share/smalltalk/kernel',
  '/usr/local/share/smalltalk/kernel',
  '/opt/homebrew/share/smalltalk/kernel',
  '/opt/local/share/smalltalk/kernel',
];

/** A directory "looks like a kernel" when it exists and holds at least one `.st`. */
export function looksLikeKernelDir(dir: string): boolean {
  try {
    return fs.statSync(dir).isDirectory() && fs.readdirSync(dir).some((f) => /\.st$/i.test(f));
  } catch {
    return false;
  }
}

/** `<prefix>/bin/gst` → `<prefix>/share/smalltalk/kernel`, when derivable. */
export function deriveKernelDirFromGst(gstPath: string): string | undefined {
  const dir = path.dirname(gstPath); // .../bin
  if (!dir || dir === '.' || dir === gstPath) {
    return undefined; // bare `gst` (resolved via PATH) — nothing to derive
  }
  const prefix = path.dirname(dir); // strip /bin
  return path.join(prefix, 'share', 'smalltalk', 'kernel');
}

export function discoverKernelDir(
  opts: DiscoveryOptions,
  commonLocations: readonly string[] = DEFAULT_COMMON_LOCATIONS,
): string | undefined {
  if (opts.kernelPath && looksLikeKernelDir(opts.kernelPath)) {
    return opts.kernelPath;
  }
  if (opts.gnuSmalltalkPath) {
    const derived = deriveKernelDirFromGst(opts.gnuSmalltalkPath);
    if (derived && looksLikeKernelDir(derived)) {
      return derived;
    }
  }
  for (const loc of commonLocations) {
    if (looksLikeKernelDir(loc)) {
      return loc;
    }
  }
  return undefined;
}
