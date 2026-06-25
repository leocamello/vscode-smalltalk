// Static `.st`-directory kernel adapter (US-430, slice C/D; ADR-0003 Tier-1).
//
// Parses a directory of GNU Smalltalk `.st` source files into a DialectCartridge,
// reusing the US-411 front end (`parse` + buildSymbolTable) exactly as
// `kernel.test.ts` does over the 122-file corpus. This is the NO-runtime installed
// adapter: it emits the same canonical cartridge shape as the reflective exporter
// (classes tier), so `KernelIndexService` resolves installed and floor like-for-like.
//
// Facts always; PROSE only when `header.carriesProse` (US-415 slice B). The
// installed adapter reads class/method comments from the user's OWN local `.st`
// files — never redistributed — so provenance, not licence, gates it (spec §4a).
// The shipped *bundled* cartridge stays facts-only (carriesProse:false), enforced
// by `kernelIndex`/`cartridgeLoader` tests. Never throws — unreadable or unparsable
// files are skipped, mirroring the index's robustness.

import fs from 'node:fs';
import path from 'node:path';
import { extractComments, type FileComments } from '../parser/comments';
import { tokenize } from '../parser/lexer';
import { parse } from '../parser/parser';
import { buildSymbolTable, SymbolKind, type SymbolNode } from '../parser/symbols';
import { forEachSend, type RawSend } from '../xref/workspaceXref';
import type {
  CartridgeHeader,
  ClassFact,
  CrossReference,
  DialectCartridge,
  Documentation,
  ImplementorRef,
  SelectorSignature,
  SendSite,
} from '../types/knowledge-base';

const ST_FILE = /\.st$/i;

interface SelectorAcc {
  readonly arity: number;
  comment?: string;
}

interface ClassAcc {
  superclass?: string;
  comment?: string;
  /** selector -> arity (+ comment when prose is carried) */
  readonly instance: Map<string, SelectorAcc>;
  readonly klass: Map<string, SelectorAcc>;
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

/** Walk the symbol tree, merging class facts (and `comments`, when present) across
 *  files/chunks into `classes`. */
function collect(nodes: SymbolNode[], classes: Map<string, ClassAcc>, comments: FileComments | undefined): void {
  for (const node of nodes) {
    if (node.kind === SymbolKind.Class) {
      const acc = accFor(classes, node.name);
      if (acc.superclass === undefined && node.detail) {
        acc.superclass = node.detail;
      }
      if (acc.comment === undefined) {
        acc.comment = comments?.classComment(node.name);
      }
      for (const child of node.children) {
        if (child.kind === SymbolKind.Method && child.selector) {
          const classSide = child.classSide ?? false;
          const into = classSide ? acc.klass : acc.instance;
          if (!into.has(child.selector)) {
            into.set(child.selector, {
              arity: child.arity ?? 0,
              comment: comments?.methodComment(node.name, classSide, child.selector),
            });
          }
        }
      }
      collect(node.children, classes, comments); // nested classes
    } else if (node.kind === SymbolKind.Namespace) {
      collect(node.children, classes, comments);
    }
  }
}

/** Keyword parts of a selector: `at:put:` → [`at:`,`put:`]; unary/binary → [sel]. */
function keywordsOf(selector: string): string[] {
  return selector.match(/[^:]+:/g) ?? [selector];
}

/** Record a send into the `selector -> SendSite[]` accumulator. `line` is the
 *  0-based offset within the enclosing method's source (matching the bundled
 *  reflective exporter's semantic), so installed senders read like-for-like. */
function recordSend(senders: Map<string, SendSite[]>, raw: ReturnType<typeof rawSendOf>): void {
  if (raw === undefined) {
    return;
  }
  const bucket = senders.get(raw.selector);
  if (bucket) {
    bucket.push(raw.site);
  } else {
    senders.set(raw.selector, [raw.site]);
  }
}

/** Project a low-level `RawSend` to a cartridge `SendSite`, or `undefined` when it
 *  lacks the enclosing-method coordinates a cross-reference fact needs (e.g. a
 *  top-level send — none exist in kernel source, which is all class bodies). */
function rawSendOf(s: RawSend): { selector: string; site: SendSite } | undefined {
  if (s.className === undefined || s.inSelector === undefined) {
    return undefined;
  }
  const line = s.methodStartLine !== undefined ? Math.max(0, s.node.startPos.line - s.methodStartLine) : s.node.startPos.line;
  return {
    selector: s.node.selector,
    site: {
      inClass: s.className,
      side: s.side,
      inSelector: s.inSelector,
      line,
      ...(s.receiverHint != null ? { receiverHint: s.receiverHint } : {}),
    },
  };
}

/** Parse every `.st` in `dir` and collect class facts + the send graph; returns
 *  the raw accumulator, the sorted list of REAL class names (synthetic
 *  placeholders dropped), and `selector -> senders`. When `carriesProse`,
 *  class/method comments are recovered too. Never throws. */
function collectKernelDirectory(
  dir: string,
  carriesProse: boolean,
): { classes: Map<string, ClassAcc>; names: string[]; senders: Map<string, SendSite[]> } {
  const classes = new Map<string, ClassAcc>();
  const senders = new Map<string, SendSite[]>();

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
      const ast = parse(text).ast;
      const comments = carriesProse ? extractComments(ast, tokenize(text).tokens) : undefined;
      collect(buildSymbolTable(ast), classes, comments);
      // Build the installed `crossReference` senders tier from the same parse, so
      // an installed kernel answers "Senders of" as richly as the bundled floor.
      forEachSend(ast, (s) => recordSend(senders, rawSendOf(s)));
    } catch {
      // Never throw from indexing — skip a pathological file.
    }
  }

  // Drop synthetic placeholders (`<anonymous>`, `<methods>`) — not real classes.
  const names = [...classes.keys()].filter((n) => n && !n.startsWith('<')).sort();
  return { classes, names, senders };
}

function docOf(text: string | undefined): Documentation | undefined {
  return text && text.trim() !== '' ? { text } : undefined;
}

function toSignatures(m: Map<string, SelectorAcc>): SelectorSignature[] {
  return [...m.keys()].sort().map((selector) => {
    const acc = m.get(selector) as SelectorAcc;
    const documentation = docOf(acc.comment);
    return {
      selector,
      arity: acc.arity,
      keywords: keywordsOf(selector),
      ...(documentation ? { documentation } : {}),
    };
  });
}

/** Index every `.st` file in `dir` into a DialectCartridge (ADR-0003 Tier-1
 *  installed adapter). Still NO runtime `gst` — a static parse of the source dir,
 *  emitting the canonical cartridge shape so the resolution chain compares
 *  installed and floor like-for-like. Emits the `classes` tier AND a
 *  `crossReference` tier (US-423: senders scanned from method bodies,
 *  implementors from the class tables) so an installed kernel answers
 *  senders/implementors as richly as the bundled reference. Comments are
 *  populated only when `header.carriesProse` is true.
 *
 *  Flat-id dialect: a class's `id` IS its simple name (GST source carries no
 *  namespace qualifier here). Instance/class variables are not recovered by the
 *  static collector, so those fact lists are empty — selectors + superclass are
 *  the tier this adapter guarantees. */
export function indexKernelDirectoryToCartridge(dir: string, header: CartridgeHeader): DialectCartridge {
  const carriesProse = header.carriesProse === true;
  const { classes, names, senders } = collectKernelDirectory(dir, carriesProse);

  const out: Record<string, ClassFact> = {};
  const implementors: Record<string, ImplementorRef[]> = {};
  const addImplementor = (selector: string, ref: ImplementorRef): void => {
    const bucket = implementors[selector];
    if (bucket) {
      bucket.push(ref);
    } else {
      implementors[selector] = [ref];
    }
  };
  for (const name of names) {
    const acc = classes.get(name);
    if (!acc) {
      continue;
    }
    const documentation = docOf(acc.comment);
    const instanceMethods = toSignatures(acc.instance);
    const classMethods = toSignatures(acc.klass);
    out[name] = {
      id: name,
      name,
      kind: 'class',
      superclass: acc.superclass ?? null,
      instanceVariables: [],
      classVariables: [],
      classInstanceVariables: [],
      instanceMethods,
      classMethods,
      taxonomy: {},
      ...(documentation ? { documentation } : {}),
    };
    for (const m of instanceMethods) {
      addImplementor(m.selector, { inClass: name, side: 'instance' });
    }
    for (const m of classMethods) {
      addImplementor(m.selector, { inClass: name, side: 'class' });
    }
  }

  // The installed adapter now ships a real `crossReference` tier (senders scanned
  // from the source, implementors from the class tables) — so an installed kernel
  // answers "Senders of" / "Implementors of" as richly as the bundled reference.
  const crossReference: CrossReference = { implementors, senders: Object.fromEntries(senders) };
  return { header, classes: out, crossReference };
}
