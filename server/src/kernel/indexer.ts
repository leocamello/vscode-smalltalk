// Static `.st`-directory kernel adapter (US-430, slice C/D; ADR-0003 Tier-1).
//
// Parses a directory of GNU Smalltalk `.st` source files into a DialectCartridge,
// reusing the US-411 front end (`parse` + buildSymbolTable) exactly as
// `kernel.test.ts` does over the 122-file corpus. This is the NO-runtime installed
// adapter: it emits the same canonical cartridge shape as the reflective exporter
// (classes tier), so `KernelIndexService` resolves installed and floor like-for-like.
//
// Facts only (LGPL-2.1): we read class names, superclass links, and selectors +
// arity from the symbol table, which carries no comment prose. Never throws —
// unreadable or unparsable files are skipped, mirroring the index's robustness.

import fs from 'node:fs';
import path from 'node:path';
import { parse } from '../parser/parser';
import { buildSymbolTable, SymbolKind, type SymbolNode } from '../parser/symbols';
import type {
  CartridgeHeader,
  ClassFact,
  DialectCartridge,
  SelectorSignature,
} from '../types/knowledge-base';

const ST_FILE = /\.st$/i;

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

/** Keyword parts of a selector: `at:put:` → [`at:`,`put:`]; unary/binary → [sel]. */
function keywordsOf(selector: string): string[] {
  return selector.match(/[^:]+:/g) ?? [selector];
}

/** Parse every `.st` in `dir` and collect class facts; returns the raw accumulator
 *  plus the sorted list of REAL class names (synthetic placeholders dropped).
 *  Never throws. */
function collectKernelDirectory(dir: string): { classes: Map<string, ClassAcc>; names: string[] } {
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
  return { classes, names };
}

function toSignatures(m: Map<string, number>): SelectorSignature[] {
  return [...m.keys()]
    .sort()
    .map((selector) => ({ selector, arity: m.get(selector) ?? 0, keywords: keywordsOf(selector) }));
}

/** Index every `.st` file in `dir` into a DialectCartridge (ADR-0003 Tier-1
 *  installed adapter). Still NO runtime `gst` — a static parse of the source dir,
 *  emitting the canonical cartridge shape so the resolution chain compares
 *  installed and floor like-for-like. Emits the `classes` tier only;
 *  `crossReference` is left to the reflective exporter.
 *
 *  Flat-id dialect: a class's `id` IS its simple name (GST source carries no
 *  namespace qualifier here). Instance/class variables are not recovered by the
 *  static collector, so those fact lists are empty — selectors + superclass are
 *  the tier this adapter guarantees. */
export function indexKernelDirectoryToCartridge(dir: string, header: CartridgeHeader): DialectCartridge {
  const { classes, names } = collectKernelDirectory(dir);

  const out: Record<string, ClassFact> = {};
  for (const name of names) {
    const acc = classes.get(name);
    if (!acc) {
      continue;
    }
    out[name] = {
      id: name,
      name,
      kind: 'class',
      superclass: acc.superclass ?? null,
      instanceVariables: [],
      classVariables: [],
      classInstanceVariables: [],
      instanceMethods: toSignatures(acc.instance),
      classMethods: toSignatures(acc.klass),
      taxonomy: {},
    };
  }

  return { header, classes: out };
}
