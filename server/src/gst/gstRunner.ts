// Opt-in `gst` diagnostics tier (US-414, Slice B / AC2-AC3).
//
// Runs the *real* GNU Smalltalk compiler as a second, authoritative opinion on a
// saved file and turns its stderr into LSP diagnostics. Strictly optional
// (ADR-0001): silently inert when `gst` is absent. No-zombie discipline (the
// US-301 lineage): one in-flight child per document, a bounded timeout, and
// kill-on-supersede so rapid saves/edits never leak processes.
//
// `parseGstStderr` is pure (no spawn, no fs) so it is unit-tested against the
// verified gst-3.2.5 stderr format; the runner takes an injectable spawner so the
// no-zombie behaviour is tested deterministically without a real `gst`.

import { spawn } from 'node:child_process';
import { DiagnosticSeverity, type Diagnostic } from 'vscode-languageserver-types';

/** Diagnostic `source` for the opt-in gst tier (badge: `gst(compile)`). */
export const GST_DIAGNOSTIC_SOURCE = 'gst';
/** Diagnostic `code` for the gst tier. */
export const GST_DIAGNOSTIC_CODE = 'compile';

/** Default bounded run time for a single `gst` invocation. */
export const DEFAULT_GST_TIMEOUT_MS = 5000;

// gst 3.2.5 stderr is line-based: `<file>:<LINE>: <message>` — no column, no
// `error:` prefix, no severity (verified on GNU Smalltalk 3.2.5). Examples:
//   bad.st:4: parse error, expected ']'
//   e2.st:2: expected object
//   e3.st:2: Unterminated string, attempting recovery
const GST_LINE = /^(?:.+?):(\d+):\s*(.+)$/;

/**
 * Parse `gst` stderr into diagnostics. Each recognized line maps to a whole-line
 * range (gst gives no column); severity is always Error. `sourceText` is the
 * saved file content, used to size the underline to the line's length.
 */
export function parseGstStderr(stderr: string, sourceText: string): Diagnostic[] {
  const lines = sourceText.split('\n');
  const diags: Diagnostic[] = [];
  for (const raw of stderr.split('\n')) {
    const m = GST_LINE.exec(raw.trim());
    if (!m || m[1] === undefined || m[2] === undefined) {
      continue;
    }
    const lineNo = Math.max(0, Number(m[1]) - 1); // gst is 1-based; LSP 0-based
    const endChar = lines[lineNo]?.length ?? 0;
    diags.push({
      range: { start: { line: lineNo, character: 0 }, end: { line: lineNo, character: endChar } },
      severity: DiagnosticSeverity.Error,
      message: m[2].trim(),
      source: GST_DIAGNOSTIC_SOURCE,
      code: GST_DIAGNOSTIC_CODE,
    });
  }
  return diags;
}

/** Minimal child-process surface the runner needs (so tests can inject a fake). */
export interface SpawnedProc {
  readonly stderr: { on(event: 'data', cb: (chunk: Buffer | string) => void): void } | null;
  on(event: 'close', cb: (code: number | null) => void): void;
  on(event: 'error', cb: (err: Error) => void): void;
  kill(signal?: NodeJS.Signals): boolean;
}

export type Spawner = (command: string, args: readonly string[]) => SpawnedProc;

const defaultSpawn: Spawner = (command, args) => spawn(command, args as string[]);

/**
 * Runs `gst` per document with no-zombie guarantees. A new run for a uri kills
 * the prior in-flight child (kill-on-supersede); a timeout bounds runaway runs;
 * the result of a superseded or errored run is dropped (resolves `null`).
 */
export class GstDiagnosticsRunner {
  private readonly inFlight = new Map<string, SpawnedProc>();

  constructor(
    private readonly spawnFn: Spawner = defaultSpawn,
    private readonly timeoutMs: number = DEFAULT_GST_TIMEOUT_MS,
  ) {}

  /** Kill and forget any in-flight run for `uri` (e.g. on edit or close). */
  cancel(uri: string): void {
    const child = this.inFlight.get(uri);
    if (child) {
      this.inFlight.delete(uri);
      child.kill('SIGKILL');
    }
  }

  /** True when a child for `uri` is currently running (test/diagnostic aid). */
  isRunning(uri: string): boolean {
    return this.inFlight.has(uri);
  }

  /**
   * Run `gst` on `filePath` for `uri`. Resolves to diagnostics, or `null` if the
   * run was superseded, timed out, or `gst` could not be spawned (tier inert).
   */
  run(uri: string, gstPath: string, filePath: string, sourceText: string): Promise<Diagnostic[] | null> {
    this.cancel(uri); // kill-on-supersede before starting a new run
    return new Promise((resolve) => {
      let stderr = '';
      let settled = false;
      const child = this.spawnFn(gstPath, [filePath]);
      this.inFlight.set(uri, child);

      const timer = setTimeout(() => child.kill('SIGKILL'), this.timeoutMs);
      const finish = (result: Diagnostic[] | null): void => {
        if (settled) {
          return;
        }
        settled = true;
        clearTimeout(timer);
        if (this.inFlight.get(uri) === child) {
          this.inFlight.delete(uri);
        }
        resolve(result);
      };

      child.stderr?.on('data', (chunk) => {
        stderr += chunk.toString();
      });
      child.on('error', () => finish(null)); // gst missing/unspawnable → inert
      child.on('close', () => {
        // Drop the result if a newer run replaced us (kill-on-supersede).
        finish(this.inFlight.get(uri) === child ? parseGstStderr(stderr, sourceText) : null);
      });
    });
  }
}
