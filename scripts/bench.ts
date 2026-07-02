// Perf benchmark for the offline language server (US-901). tsx, no Electron/LSP.
//
// Drives the same pure functions the server uses, over a generated synthetic
// corpus (scripts/gen-corpus.ts), and asserts the documented budgets:
//   AC1  index      1000 files  < 5 s   (cold, from disk)
//   AC2  completion p95         < 100 ms (on the 1k-file workspace)
// plus the class-rename whole-workspace scan (the flagged suspect) as info.
//
// Local + release-ritual tool — NOT a CI gate (runner variance → flaky, spec §3).
// Exits non-zero if a hard budget FAILs, so it's usable in the release checklist.
//
// Usage:  npm run bench            (1000 files, temp dir, cleaned up)
//         npm run bench -- --files 2000 --out /tmp/corpus --keep
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { performance } from 'node:perf_hooks';
import { pathToFileURL } from 'node:url';

import { generateCorpus } from './gen-corpus.ts';
import { WorkspaceIndex, walkStFiles, defaultExclude } from '../server/src/providers/workspaceIndex.ts';
import { WorkspaceXref } from '../server/src/xref/workspaceXref.ts';
import { KernelIndexService } from '../server/src/kernel/kernelIndexService.ts';
import { completionsAt, type SelectorCandidate, type ClassCandidate } from '../server/src/providers/completion.ts';
import { buildSymbolTable, SymbolKind } from '../server/src/parser/symbols.ts';
import { parse } from '../server/src/parser/parser.ts';
import { tokenize } from '../server/src/parser/lexer.ts';
import { Provenance } from '../server/src/kernel/model.ts';
import { buildClassWorldFromFiles, classOccurrences } from '../server/src/xref/classRefs.ts';
import type { FileText } from '../server/src/xref/ivarRefs.ts';

interface Budget {
  readonly label: string;
  readonly measured: number; // ms
  readonly budget?: number; // ms; absent → info-only
}

const args = process.argv.slice(2);
const opt = (flag: string, def: string): string => {
  const i = args.indexOf(flag);
  return i >= 0 && args[i + 1] ? args[i + 1] : def;
};
const FILES = Number(opt('--files', '1000'));
const KEEP = args.includes('--keep');
const OUT = opt('--out', fs.mkdtempSync(path.join(os.tmpdir(), 'st-bench-')));

function percentile(sorted: number[], p: number): number {
  if (sorted.length === 0) return 0;
  const idx = Math.min(sorted.length - 1, Math.ceil((p / 100) * sorted.length) - 1);
  return sorted[idx] as number;
}

function main(): void {
  console.log(`\nSmalltalk LSP benchmark — ${FILES} files\n`);
  const info = generateCorpus(OUT, FILES);
  const results: Budget[] = [];

  // ── AC1: cold index (symbol index + cross-reference index), from disk ───────
  const index = new WorkspaceIndex();
  const xref = new WorkspaceXref();
  const t0 = performance.now();
  walkStFiles(OUT, defaultExclude, (uri, text) => {
    index.setFile(uri, text);
    xref.setFile(uri, text);
  });
  const indexMs = performance.now() - t0;
  results.push({ label: `index ${FILES} files (cold)`, measured: indexMs, budget: 5000 });

  // ── AC2: completion latency ─────────────────────────────────────────────────
  // Mirror the onCompletion handler: candidate lists are rebuilt from the index
  // on EVERY request (part of the measured cost), then completionsAt runs.
  const kernel = new KernelIndexService();
  kernel.configure({ kernelLibrary: 'bundled' }); // hermetic Tier-2 floor
  const kernelSelectors = kernel.selectors().map((s) => ({ selector: s.selector, provenance: s.provenance }));
  const kernelClasses = kernel.classes().map((c) => ({ name: c.name, provenance: c.provenance }));

  // A representative completion request: a head-context prefix that filters over
  // the full class + variable candidate set, plus a selector-context one.
  const reqText = 'Object subclass: Probe [\n    | acc | run: aThing [ acc := aThing tick. ^Widg ]\n]\n';
  const headOffset = reqText.indexOf('Widg') + 'Widg'.length;
  const selText = 'Object subclass: Probe [ run [ ^Widget0001 new ren ] ]\n';
  const selOffset = selText.indexOf('ren ') + 'ren'.length;
  const samples: Array<{ text: string; offset: number }> = [
    { text: reqText, offset: headOffset },
    { text: selText, offset: selOffset },
  ];

  const runCompletion = (text: string, offset: number): number => {
    const t = performance.now();
    const entries = index.all();
    const selectors: SelectorCandidate[] = [
      ...entries
        .filter((e) => e.kind === SymbolKind.Method)
        .map((e) => ({ selector: e.name, provenance: Provenance.Workspace })),
      ...kernelSelectors,
    ];
    const classes: ClassCandidate[] = [
      ...entries
        .filter((e) => e.kind === SymbolKind.Class || e.kind === SymbolKind.Namespace)
        .map((e) => ({ name: e.name, provenance: Provenance.Workspace })),
      ...kernelClasses,
    ];
    const ast = parse(text).ast;
    const tokens = tokenize(text).tokens;
    const symbols = buildSymbolTable(ast);
    completionsAt(offset, text, tokens, ast, symbols, selectors, classes);
    return performance.now() - t;
  };

  const ITER = 200;
  const durations: number[] = [];
  for (let i = 0; i < ITER; i++) {
    const s = samples[i % samples.length] as { text: string; offset: number };
    durations.push(runCompletion(s.text, s.offset));
  }
  durations.sort((a, b) => a - b);
  const p95 = percentile(durations, 95);
  results.push({ label: `completion p50`, measured: percentile(durations, 50) });
  results.push({ label: `completion p95`, measured: p95, budget: 100 });
  results.push({ label: `completion max`, measured: durations[durations.length - 1] as number });

  // ── Class-rename whole-workspace scan (the flagged suspect) — info ───────────
  // Replicates allWorkspaceFiles(): read every indexed file from disk, then scan
  // for references to the hot class. Reported as info (no hard budget).
  const tScan = performance.now();
  const files: FileText[] = [];
  for (const uri of new Set(index.all().map((e) => e.uri))) {
    const fsPath = uri.startsWith('file://') ? fileURLToFsPath(uri) : uri;
    try {
      files.push({ uri, text: fs.readFileSync(fsPath, 'utf8') });
    } catch {
      /* skip unreadable */
    }
  }
  const world = buildClassWorldFromFiles(files, () => false);
  const occ = classOccurrences(info.hotClass, world, files);
  const scanMs = performance.now() - tScan;
  const hitFiles = occ.size;
  results.push({ label: `class-rename scan (${info.hotClass}, ${hitFiles} files hit)`, measured: scanMs });

  // ── Report ──────────────────────────────────────────────────────────────────
  console.log('  metric                                        measured   budget   status');
  console.log('  ' + '─'.repeat(76));
  let failed = false;
  for (const r of results) {
    const measured = `${r.measured.toFixed(1)} ms`.padStart(10);
    if (r.budget === undefined) {
      console.log(`  ${r.label.padEnd(44)}${measured}       —     info`);
    } else {
      const ok = r.measured < r.budget;
      failed = failed || !ok;
      const budget = `<${r.budget} ms`.padStart(9);
      console.log(`  ${r.label.padEnd(44)}${measured}${budget}   ${ok ? 'PASS' : 'FAIL'}`);
    }
  }
  console.log('');

  if (!KEEP) fs.rmSync(OUT, { recursive: true, force: true });
  else console.log(`  corpus kept at ${OUT}\n`);

  process.exit(failed ? 1 : 0);
}

/** file:// URI → fs path (POSIX; the corpus lives on the local disk). */
function fileURLToFsPath(uri: string): string {
  return decodeURIComponent(uri.replace(/^file:\/\//, ''));
}

main();
