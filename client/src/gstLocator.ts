import * as path from 'path';

export interface GstResolution {
  /** Absolute (or configured) path to the gst executable. */
  path: string;
  /** Where it came from. */
  source: 'setting' | 'path';
}

export interface ResolveGstOptions {
  /** Value of the `smalltalk.gnuSmalltalkPath` setting (may be empty). */
  configuredPath: string;
  /** Contents of the PATH environment variable (may be undefined). */
  pathEnv: string | undefined;
  /** Platform path delimiter (`path.delimiter`). */
  delimiter: string;
  /** Candidate executable names to probe on PATH (e.g. `['gst']`). */
  exeNames: string[];
  /** Probe whether a candidate path is an executable file. */
  isExecutable: (candidate: string) => boolean;
}

/**
 * Resolve the `gst` executable. The configured setting wins (AC5); if it is
 * empty, PATH is searched (AC6). Returns undefined when neither yields an
 * executable, so the caller can surface a helpful error (AC7).
 */
export function resolveGst(opts: ResolveGstOptions): GstResolution | undefined {
  const configured = opts.configuredPath.trim();
  if (configured.length > 0) {
    return opts.isExecutable(configured) ? { path: configured, source: 'setting' } : undefined;
  }

  const dirs = (opts.pathEnv ?? '').split(opts.delimiter).filter((d) => d.length > 0);
  for (const dir of dirs) {
    for (const exe of opts.exeNames) {
      const candidate = path.join(dir, exe);
      if (opts.isExecutable(candidate)) {
        return { path: candidate, source: 'path' };
      }
    }
  }
  return undefined;
}

/** Candidate executable names for the platform. */
export function defaultExeNames(platform: NodeJS.Platform): string[] {
  return platform === 'win32' ? ['gst.exe', 'gst'] : ['gst'];
}

/**
 * Build the terminal command line, double-quoting both arguments so paths with
 * spaces work across the default cmd/pwsh/bash shells (AC11).
 */
export function buildRunCommand(gstPath: string, filePath: string): string {
  return `${quote(gstPath)} ${quote(filePath)}`;
}

function quote(value: string): string {
  return `"${value.replace(/"/g, '\\"')}"`;
}
