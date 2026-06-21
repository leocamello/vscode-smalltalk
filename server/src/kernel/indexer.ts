// Reusable `.st`-directory kernel indexer (US-413, slice A; ADR-0002).
//
// Builds a dialect-neutral KernelIndexData from a directory of GNU Smalltalk
// `.st` source files, reusing the US-411 front end (`parse` + buildSymbolTable)
// exactly as `kernel.test.ts` does over the 122-file corpus. The SAME function
// serves the build-time bundled generator (this slice) and, later, the live
// installed-kernel path (AC6) — only the source directory + header differ.
//
// Facts only (LGPL-2.1): we read class names, superclass links, and selectors +
// arity from the symbol table, which carries no comment prose. Never throws —
// unreadable or unparsable files are skipped, mirroring the index's robustness.

import fs from 'node:fs';
import path from 'node:path';
import { parse } from '../parser/parser';
import { buildSymbolTable, SymbolKind, type SymbolNode } from '../parser/symbols';
import type { KernelClass, KernelIndexData, KernelIndexHeader, KernelSelector } from './model';

const ST_FILE = /\.st$/i;

/** Identity of the bundled GST 3.2.5 reference index. */
export const BUNDLED_HEADER: KernelIndexHeader = {
  dialect: 'gst',
  library: 'gst-3.2.5',
  version: '3.2.5',
  source: 'bundled',
};

/** Default location of the bundled kernel corpus, relative to the repo root
 *  (cwd for `npm run gen:kernel-index` and `npm run test:parser`). */
export function bundledKernelDir(): string {
  return path.join(process.cwd(), '../smalltalk-3.2.5/kernel');
}

/** Path of the committed bundled index, relative to the repo root. */
export function bundledIndexPath(): string {
  return path.join(process.cwd(), 'server/data/kernel-index.json');
}

interface ClassAcc {
  superclass?: string;
  /** selector -> arity */
  readonly instance: Map<string, number>;
  readonly klass: Map<string, number>;
}

function accFor(classes: Map<string, ClassAcc>, name: string): ClassAcc {
  const existing = classes.get(name);
  if (existing) {
    return existing;
  }
  const created: ClassAcc = { instance: new Map(), klass: new Map() };
  classes.set(name, created);
  return created;
}

/** Walk the symbol tree, merging class facts across files/chunks into `classes`. */
function collect(nodes: SymbolNode[], classes: Map<string, ClassAcc>): void {
  for (const node of nodes) {
    if (node.kind === SymbolKind.Class) {
      const acc = accFor(classes, node.name);
      if (acc.superclass === undefined && node.detail) {
        acc.superclass = node.detail;
      }
      for (const child of node.children) {
        if (child.kind === SymbolKind.Method && child.selector) {
          const into = child.classSide ? acc.klass : acc.instance;
          if (!into.has(child.selector)) {
            into.set(child.selector, child.arity ?? 0);
          }
        }
      }
      collect(node.children, classes); // nested classes
    } else if (node.kind === SymbolKind.Namespace) {
      collect(node.children, classes);
    }
  }
}

function toSelectors(m: Map<string, number>): KernelSelector[] {
  return [...m.keys()].sort().map((selector) => ({ selector, arity: m.get(selector) ?? 0 }));
}

/** Index every `.st` file in `dir` into a deterministic KernelIndexData. */
export function indexKernelDirectory(dir: string, header: KernelIndexHeader): KernelIndexData {
  const classes = new Map<string, ClassAcc>();

  let files: string[] = [];
  try {
    files = fs.readdirSync(dir).filter((f) => ST_FILE.test(f)).sort();
  } catch {
    files = [];
  }

  for (const file of files) {
    let text: string;
    try {
      text = fs.readFileSync(path.join(dir, file), 'utf8');
    } catch {
      continue;
    }
    try {
      collect(buildSymbolTable(parse(text).ast), classes);
    } catch {
      // Never throw from indexing — skip a pathological file.
    }
  }

  // Drop synthetic placeholders (`<anonymous>`, `<methods>`) — not real classes.
  const names = [...classes.keys()].filter((n) => n && !n.startsWith('<')).sort();

  const out: Record<string, KernelClass> = {};
  let selectorCount = 0;
  for (const name of names) {
    const acc = classes.get(name);
    if (!acc) {
      continue;
    }
    const instanceSelectors = toSelectors(acc.instance);
    const classSelectors = toSelectors(acc.klass);
    selectorCount += instanceSelectors.length + classSelectors.length;
    out[name] = {
      ...(acc.superclass !== undefined ? { superclass: acc.superclass } : {}),
      instanceSelectors,
      classSelectors,
    };
  }

  return {
    ...header,
    classCount: names.length,
    selectorCount,
    classes: out,
  };
}

/** Canonical, deterministic serialization (stable key order + trailing newline). */
export function serializeKernelIndex(data: KernelIndexData): string {
  return `${JSON.stringify(data, null, 2)}\n`;
}
