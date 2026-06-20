// Workspace symbol index (US-412, slice B / AC2).
//
// Flattens each `.st`/`.gst` file's symbol tree into class/namespace/method
// entries with a Location, keyed by file URI, so `workspace/symbol` can search
// across the workspace. Open documents are updated from their live text; the
// initial population reads from disk. Pure Node (fs) — no VS Code.

import fs from 'node:fs';
import path from 'node:path';
import { pathToFileURL } from 'node:url';
import { parse } from '../parser/parser';
import { buildSymbolTable, SymbolKind, type SymbolNode } from '../parser/symbols';
import type { Position } from '../parser/token';

export interface IndexRange {
  readonly start: Position;
  readonly end: Position;
}

export interface IndexEntry {
  /** The class/namespace name, or the method selector. */
  readonly name: string;
  readonly kind: SymbolKind;
  readonly classSide?: boolean;
  /** The enclosing class/namespace name, when known. */
  readonly containerName?: string;
  readonly uri: string;
  readonly range: IndexRange;
  readonly selectionRange: IndexRange;
}

/** Returns true when a path should be skipped during the scan. */
export type ExcludePredicate = (fullPath: string, basename: string, isDirectory: boolean) => boolean;

const DEFAULT_EXCLUDED_DIRS = new Set(['node_modules', '.git', 'dist', 'out', '.vscode-test', 'bower_components']);
const MAX_FILES = 5000; // guard against pathological trees
const ST_FILE = /\.(st|gst)$/i;

/** Skip well-known build/VCS directories and dot-directories. */
export const defaultExclude: ExcludePredicate = (_full, name, isDirectory) =>
  isDirectory && (DEFAULT_EXCLUDED_DIRS.has(name) || name.startsWith('.'));

/** Build an exclude predicate from a VS Code `files.exclude` map (best-effort:
 *  honors the common `**​/name`, `name`, and `name/**` directory globs). */
export function excludeFromConfig(filesExclude: Record<string, boolean> | undefined): ExcludePredicate {
  const names = new Set<string>();
  for (const [pattern, enabled] of Object.entries(filesExclude ?? {})) {
    if (!enabled) {
      continue;
    }
    const base = pattern.replace(/^\*\*\//, '').replace(/\/\*\*$/, '').replace(/\/$/, '');
    if (base && !base.includes('/') && !base.includes('*')) {
      names.add(base);
    }
  }
  return (full, name, isDirectory) => defaultExclude(full, name, isDirectory) || names.has(name);
}

function toRange(r: { startPos: Position; endPos: Position }): IndexRange {
  return { start: r.startPos, end: r.endPos };
}

function flatten(uri: string, nodes: SymbolNode[], container: string | undefined, out: IndexEntry[]): void {
  for (const node of nodes) {
    if (node.kind === SymbolKind.Class || node.kind === SymbolKind.Namespace || node.kind === SymbolKind.Method) {
      const entry: IndexEntry = {
        name: node.name,
        kind: node.kind,
        uri,
        range: toRange(node.range),
        selectionRange: toRange(node.selectionRange),
        ...(node.classSide !== undefined ? { classSide: node.classSide } : {}),
        ...(container !== undefined ? { containerName: container } : {}),
      };
      out.push(entry);
    }
    const nextContainer =
      node.kind === SymbolKind.Class || node.kind === SymbolKind.Namespace ? node.name : container;
    flatten(uri, node.children, nextContainer, out);
  }
}

export class WorkspaceIndex {
  private readonly byUri = new Map<string, IndexEntry[]>();

  /** (Re)index a single document from its text. */
  setFile(uri: string, text: string): void {
    const out: IndexEntry[] = [];
    flatten(uri, buildSymbolTable(parse(text).ast), undefined, out);
    this.byUri.set(uri, out);
  }

  removeFile(uri: string): void {
    this.byUri.delete(uri);
  }

  clear(): void {
    this.byUri.clear();
  }

  /** Recursively index `.st`/`.gst` files under `root`, skipping `exclude` paths. */
  indexFolder(root: string, exclude: ExcludePredicate = defaultExclude): void {
    let count = 0;
    const walk = (dir: string): void => {
      let dirents: fs.Dirent[];
      try {
        dirents = fs.readdirSync(dir, { withFileTypes: true });
      } catch {
        return;
      }
      for (const dirent of dirents) {
        const full = path.join(dir, dirent.name);
        if (dirent.isDirectory()) {
          if (!exclude(full, dirent.name, true)) {
            walk(full);
          }
        } else if (dirent.isFile() && ST_FILE.test(dirent.name) && count < MAX_FILES) {
          if (exclude(full, dirent.name, false)) {
            continue;
          }
          count += 1;
          try {
            this.setFile(pathToFileURL(full).href, fs.readFileSync(full, 'utf8'));
          } catch {
            // Unreadable file — skip it; never throw from indexing.
          }
        }
      }
    };
    walk(root);
  }

  /** Entries whose name contains `query` (case-insensitive); empty query → all. */
  query(query: string): IndexEntry[] {
    const needle = query.toLowerCase();
    const out: IndexEntry[] = [];
    for (const entries of this.byUri.values()) {
      for (const entry of entries) {
        if (needle === '' || entry.name.toLowerCase().includes(needle)) {
          out.push(entry);
        }
      }
    }
    return out;
  }

  get size(): number {
    return this.byUri.size;
  }
}
