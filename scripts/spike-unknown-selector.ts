// SPIKE-01 corpus harness — measure the unknown-selector gate's false-positive
// rate + closed-world coverage on real code, to inform the go/no-go memo (AC3).
//
// Drives the pure gate (`server/src/diagnostics/unknownSelectorGate.ts`) over the
// bundled GST 3.2.5 cartridge ∪ a workspace class map parsed from the corpus,
// walking every message send (via the shared `forEachSend`) and tallying the
// outcome histogram. Every `emit` is printed for manual triage: on KNOWN-GOOD
// corpus code, an emit is a FALSE POSITIVE unless it is a genuine latent typo.
//
// Run: npx tsx scripts/spike-unknown-selector.ts [--corpus all|kernel|learning]
//      [--list-emits] [--emit-context]
// No gst, no VS Code; deterministic over the committed cartridge.

import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { parse } from '../server/src/parser/parser.ts';
import { tokenize } from '../server/src/parser/lexer.ts';
import { SymbolKind } from '../server/src/parser/symbols.ts';
import { WorkspaceIndex } from '../server/src/providers/workspaceIndex.ts';
import { forEachSend } from '../server/src/xref/workspaceXref.ts';
import { KernelIndexService } from '../server/src/kernel/kernelIndexService.ts';
import { bundledCartridge, loadCartridge } from '../server/src/kernel/cartridgeLoader.ts';
import { cartridgeClassWorld, type WorkspaceClass } from '../server/src/diagnostics/cartridgeClassWorld.ts';
import { evaluateMissingSelector, type GateOutcome, type SendContext } from '../server/src/diagnostics/unknownSelectorGate.ts';

const here = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(here, '..');
const args = process.argv.slice(2);
const corpusArg = (args[args.indexOf('--corpus') + 1] && args.includes('--corpus')) ? args[args.indexOf('--corpus') + 1] : 'all';
const LIST_EMITS = args.includes('--list-emits') || true; // emits are the whole point — always list
const EMIT_CONTEXT = args.includes('--emit-context');

interface Corpus {
  readonly name: string;
  readonly dir: string;
}
const CORPORA: Corpus[] = [
  { name: 'kernel', dir: path.resolve(repoRoot, '../smalltalk-3.2.5/kernel') },
  { name: 'learning', dir: path.resolve(repoRoot, '../learning-smalltalk') },
].filter((c) => corpusArg === 'all' || c.name === corpusArg);

/** Every `.st`/`.gst` file under `dir`, recursively. */
function stFiles(dir: string): string[] {
  if (!fs.existsSync(dir)) {
    return [];
  }
  const out: string[] = [];
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      out.push(...stFiles(full));
    } else if (/\.(st|gst)$/.test(entry.name)) {
      out.push(full);
    }
  }
  return out.sort();
}

interface FileRec {
  readonly corpus: string;
  readonly file: string;
  readonly text: string;
  readonly clean: boolean;
}

// ---- 1. Load every file; build the union workspace class map ------------------
const files: FileRec[] = [];
const wsClasses = new Map<string, { superclass?: string; instance: Set<string>; classSel: Set<string> }>();

for (const corpus of CORPORA) {
  for (const file of stFiles(corpus.dir)) {
    const text = fs.readFileSync(file, 'utf8');
    const { diagnostics } = parse(text);
    files.push({ corpus: corpus.name, file, text, clean: diagnostics.length === 0 });

    const index = new WorkspaceIndex();
    index.setFile(file, text);
    for (const e of index.all()) {
      if (e.kind === SymbolKind.Class || e.kind === SymbolKind.Namespace) {
        const cls = wsClasses.get(e.name) ?? { instance: new Set<string>(), classSel: new Set<string>() };
        if (e.superclass !== undefined && cls.superclass === undefined) {
          cls.superclass = e.superclass;
        }
        wsClasses.set(e.name, cls);
      }
    }
    // Second pass: attach methods to their container class.
    for (const e of index.all()) {
      if (e.kind === SymbolKind.Method && e.containerName !== undefined) {
        const cls = wsClasses.get(e.containerName) ?? { instance: new Set<string>(), classSel: new Set<string>() };
        (e.classSide ? cls.classSel : cls.instance).add(e.name);
        wsClasses.set(e.containerName, cls);
      }
    }
  }
}

const workspace = new Map<string, WorkspaceClass>();
for (const [name, c] of wsClasses) {
  workspace.set(name, { ...(c.superclass !== undefined ? { superclass: c.superclass } : {}), instance: c.instance, classSel: c.classSel, extensionOnly: false });
}

// ---- 2. Build the ClassWorld over the bundled cartridge ∪ workspace ------------
const kernelService = new KernelIndexService(undefined, []);
kernelService.configure({ kernelLibrary: 'bundled' });
const loaded = loadCartridge(bundledCartridge);
const world = cartridgeClassWorld(loaded, workspace);

// ---- 3. Walk every send; tally outcomes; collect emits ------------------------
interface Emit {
  readonly corpus: string;
  readonly file: string;
  readonly line: number;
  readonly targetClass: string;
  readonly targetSide: string;
  readonly selector: string;
  readonly enclosing?: string;
  readonly snippet: string;
}
const emits: Emit[] = [];
const histByCorpus = new Map<string, Map<GateOutcome, number>>();
const sendsByCorpus = new Map<string, number>();

function bump(corpus: string, outcome: GateOutcome): void {
  const h = histByCorpus.get(corpus) ?? new Map<GateOutcome, number>();
  h.set(outcome, (h.get(outcome) ?? 0) + 1);
  histByCorpus.set(corpus, h);
}

for (const rec of files) {
  const ast = parse(rec.text).ast;
  forEachSend(ast, (send) => {
    const ctx: SendContext = {
      node: send.node,
      ...(send.className !== undefined ? { enclosingClass: send.className } : {}),
      enclosingSide: send.side,
      methodClean: rec.clean,
      optedOut: false, // no <lint:> opt-out pragmas in these corpora (hatch unit-tested separately)
    };
    const result = evaluateMissingSelector(ctx, world);
    bump(rec.corpus, result.outcome);
    sendsByCorpus.set(rec.corpus, (sendsByCorpus.get(rec.corpus) ?? 0) + 1);
    if (result.outcome === 'emit') {
      const line = send.node.startPos.line;
      emits.push({
        corpus: rec.corpus,
        file: path.relative(repoRoot, rec.file),
        line: line + 1,
        targetClass: result.targetClass ?? '?',
        targetSide: result.targetSide ?? '?',
        selector: send.node.selector,
        ...(send.inSelector !== undefined ? { enclosing: `${send.className ?? '?'}»${send.inSelector}` } : {}),
        snippet: rec.text.slice(send.node.start, Math.min(send.node.end, send.node.start + 60)).replace(/\s+/g, ' '),
      });
    }
  });
}

// ---- 4. Report ----------------------------------------------------------------
const ORDER: GateOutcome[] = [
  'emit',
  'understood',
  'unknown-receiver',
  'escape-incomplete',
  'escape-dnu',
  'escape-proxy',
  'escape-allowlist',
  'escape-perform',
  'escape-optout',
  'method-not-clean',
];

console.log('='.repeat(78));
console.log('SPIKE-01 — Unknown-Selector Heuristic — corpus report');
console.log('='.repeat(78));
console.log(`cartridge: ${bundledCartridge.header.dialectLabel} ${bundledCartridge.header.version} (${Object.keys(bundledCartridge.classes).length} classes)`);
console.log(`workspace classes parsed: ${workspace.size}`);
console.log(`files: ${files.length}  (clean: ${files.filter((f) => f.clean).length})`);
console.log('');

let totalSends = 0;
let totalEmit = 0;
let totalResolved = 0;
let totalSpeakable = 0; // understood + emit — the gate actually consulted a table
for (const corpus of CORPORA) {
  const h = histByCorpus.get(corpus.name) ?? new Map<GateOutcome, number>();
  const sends = sendsByCorpus.get(corpus.name) ?? 0;
  const get = (o: GateOutcome): number => h.get(o) ?? 0;
  const resolved = sends - get('unknown-receiver');
  const speakable = get('understood') + get('emit');
  totalSends += sends;
  totalEmit += get('emit');
  totalResolved += resolved;
  totalSpeakable += speakable;
  console.log(`── corpus: ${corpus.name} ────────────────────────────────`);
  console.log(`  total sends:            ${sends}`);
  for (const o of ORDER) {
    if (get(o) > 0) {
      console.log(`    ${o.padEnd(20)} ${get(o)}`);
    }
  }
  console.log(`  closed-world coverage:  ${resolved}/${sends} = ${pct(resolved, sends)}  (receiver resolved)`);
  console.log(`  table-consulted:        ${speakable}/${sends} = ${pct(speakable, sends)}  (definite understood/emit)`);
  console.log(`  EMITS (candidate FPs):  ${get('emit')}`);
  console.log('');
}

console.log('── TOTAL ────────────────────────────────');
console.log(`  sends: ${totalSends}  resolved: ${totalResolved} (${pct(totalResolved, totalSends)})  table-consulted: ${totalSpeakable} (${pct(totalSpeakable, totalSends)})  emits: ${totalEmit}`);
console.log('');

if (LIST_EMITS) {
  console.log('── EMITS (triage each: false positive vs genuine latent typo) ──');
  if (emits.length === 0) {
    console.log('  (none) — zero emits on known-good corpus code.');
  } else {
    for (const e of emits) {
      console.log(`  [${e.corpus}] ${e.file}:${e.line}  ${e.targetClass}(${e.targetSide}) ⊬ #${e.selector}   in ${e.enclosing ?? '(top level)'}`);
      if (EMIT_CONTEXT) {
        console.log(`       ${e.snippet}`);
      }
    }
  }
  console.log('');
}

function pct(n: number, d: number): string {
  return d === 0 ? 'n/a' : `${((100 * n) / d).toFixed(1)}%`;
}
