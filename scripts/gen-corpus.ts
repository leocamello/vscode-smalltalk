// Synthetic Smalltalk corpus generator for the perf benchmark (US-901).
//
// Emits N `.st` files with realistic shapes — class definitions with a
// superclass, instance variables, class-side + instance-side methods with
// keyword/unary/binary sends, and CROSS-FILE class references — so the workspace
// index, the cross-reference index, and the class-rename whole-workspace scan are
// all exercised (not just isolated files). Deterministic (seeded), GST bracket
// syntax, parses with zero diagnostics.
//
// Committed as a SCRIPT, not as corpus files (spec §5). Usage:
//   npx tsx scripts/gen-corpus.ts --files 1000 --out /tmp/corpus
import fs from 'node:fs';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

export interface CorpusInfo {
  readonly dir: string;
  readonly files: number;
  /** A class referenced by ~40% of files — the class-rename scan worst case. */
  readonly hotClass: string;
  /** fsPath of one representative file, for the completion sample. */
  readonly sampleFile: string;
}

// Deterministic PRNG (mulberry32) — a seed reproduces the exact corpus.
function rng(seed: number): () => number {
  let a = seed >>> 0;
  return () => {
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

const name = (i: number): string => `Widget${String(i).padStart(4, '0')}`;

/** One class definition referencing a few other workspace classes by index. */
function classSource(i: number, count: number, next: () => number): string {
  const cls = name(i);
  // Superclass: mostly Object, sometimes another (lower-indexed) widget → chains.
  const sup = i > 0 && next() < 0.3 ? name(Math.floor(next() * i)) : 'Object';
  // A few cross-file references; the hot class (index 0) is referenced often.
  const ref = (): string => (next() < 0.4 ? name(0) : name(Math.floor(next() * count)));
  return [
    `${sup} subclass: ${cls} [`,
    `    | count label next peer |`,
    `    ${cls} class >> new [ ^super new init ]`,
    `    init [ count := 0. label := '${cls}'. ^self ]`,
    `    tick [ count := count + 1. ^count ]`,
    `    total: aNumber with: anotherNumber [ ^aNumber + anotherNumber + count ]`,
    `    render: aTarget [ aTarget draw: label at: count. ^${ref()} new tick ]`,
    `    linkAll [ peer := ${ref()} new. next := ${ref()} new. ^peer ]`,
    `]`,
    '',
  ].join('\n');
}

export function generateCorpus(dir: string, files: number, seed = 0x513c): CorpusInfo {
  fs.mkdirSync(dir, { recursive: true });
  const next = rng(seed);
  for (let i = 0; i < files; i++) {
    // Shard into subdirs of 100 so the disk walk sees a real tree, not one flat dir.
    const sub = path.join(dir, `pkg${String(Math.floor(i / 100)).padStart(3, '0')}`);
    fs.mkdirSync(sub, { recursive: true });
    fs.writeFileSync(path.join(sub, `${name(i)}.st`), classSource(i, files, next), 'utf8');
  }
  return {
    dir,
    files,
    hotClass: name(0),
    sampleFile: path.join(dir, 'pkg000', `${name(Math.min(42, files - 1))}.st`),
  };
}

// CLI
const isMain = process.argv[1] && pathToFileURL(process.argv[1]).href === import.meta.url;
if (isMain) {
  const args = process.argv.slice(2);
  const opt = (flag: string, def: string): string => {
    const i = args.indexOf(flag);
    return i >= 0 && args[i + 1] ? args[i + 1] : def;
  };
  const files = Number(opt('--files', '1000'));
  const out = opt('--out', path.join(process.cwd(), 'corpus'));
  const info = generateCorpus(out, files);
  console.log(`Generated ${info.files} files under ${info.dir} (hot class ${info.hotClass}).`);
}
