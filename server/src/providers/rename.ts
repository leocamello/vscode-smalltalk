// Scope-aware rename provider (US-426). Pure (vscode-languageserver-types only);
// the cross-file ivar path takes an explicit (uri, text) file set so it unit-tests
// in memory and the server injects open docs ∪ workspace files.
//
// Renameable: temporaries, block/method arguments, and instance variables.
// Rejected with a reason: selectors, classes/kernel globals, pseudo-variables,
// literals, and unresolved names. New names are validated (identifier + no
// collision/shadow) — the rename can never merge two distinct symbols (AC4).

import { type Range, type TextEdit, type WorkspaceEdit } from 'vscode-languageserver-types';
import { type Node, type ProgramNode } from '../parser/ast';
import { NodeKind } from '../parser/ast';
import { parse } from '../parser/parser';
import { tokenize } from '../parser/lexer';
import type { SymbolNode } from '../parser/symbols';
import { TokenKind, type Token } from '../parser/token';
import { bindingScope, isLocallyBound, pathToOffset, variableOccurrences, type Ranged } from '../parser/scope';
import { resolveQueryInAst } from './definition';
import {
  classNameForDefinition,
  classNameOf,
  isDeclaredIvar,
  isDeclaredIvarInAst,
  ivarOccurrences,
  type FileText,
} from '../xref/ivarRefs';

export interface RenameRange {
  readonly range: Range;
}
export interface RenameReject {
  readonly reject: string;
}
export type { FileText };

const PSEUDO = new Set(['self', 'super', 'thisContext', 'nil', 'true', 'false']);
const IDENTIFIER = /^[A-Za-z_][A-Za-z0-9_]*$/;
const reject = (msg: string): RenameReject => ({ reject: msg });
const isReject = (v: unknown): v is RenameReject =>
  typeof v === 'object' && v !== null && 'reject' in v;

type Classification =
  | { readonly kind: 'local'; readonly name: string; readonly tok: Token }
  | { readonly kind: 'ivar'; readonly name: string; readonly className: string; readonly tok: Token }
  | RenameReject;

/** The token covering `offset` (preferring a strictly-containing one). */
function tokenAt(tokens: Token[], offset: number): Token | undefined {
  return (
    tokens.find((t) => t.kind !== TokenKind.EOF && t.start <= offset && offset < t.end) ??
    tokens.find((t) => t.kind !== TokenKind.EOF && t.start <= offset && offset <= t.end)
  );
}

/** Nearest enclosing class name (from a Definition, or a `Class >> sel` method target). */
function enclosingClassName(path: Node[]): string | undefined {
  for (let i = path.length - 1; i >= 0; i--) {
    const n = path[i] as Node;
    if (n.kind === NodeKind.Definition) {
      const c = classNameForDefinition(n);
      if (c) return c;
    }
    if (n.kind === NodeKind.MethodDefinition && n.target) {
      const c = classNameOf(n.target);
      if (c) return c;
    }
  }
  return undefined;
}

/** Param/temp names bound by a scope node (for collision checks). */
function boundNames(scope: Node): Set<string> {
  const names = new Set<string>();
  if (scope.kind === NodeKind.Block || scope.kind === NodeKind.MethodDefinition) {
    for (const p of scope.parameters) names.add(p.name);
    for (const t of scope.temporaries) names.add(t.name);
  } else if (scope.kind === NodeKind.Program) {
    for (const t of scope.temporaries) names.add(t.name);
  }
  return names;
}

const NOT_RENAMEABLE = 'Only temporaries, arguments, and instance variables can be renamed.';

/** Classify the symbol under the cursor; `files` (optional) lets ivars resolve across files. */
function classify(
  ast: ProgramNode,
  tokens: Token[],
  offset: number,
  files?: ReadonlyArray<FileText>,
): Classification {
  const tok = tokenAt(tokens, offset);
  if (!tok) {
    return reject('There is nothing to rename here.');
  }
  if (resolveQueryInAst(ast, offset)?.target === 'selector') {
    return reject(`Selector rename isn't supported — ${NOT_RENAMEABLE.toLowerCase()}`);
  }
  if (tok.kind !== TokenKind.Identifier) {
    return reject(NOT_RENAMEABLE);
  }
  const name = tok.text;
  if (PSEUDO.has(name)) {
    return reject(`"${name}" is a pseudo-variable and can't be renamed.`);
  }
  const path = pathToOffset(ast, offset);
  if (isLocallyBound(path, name)) {
    return { kind: 'local', name, tok };
  }
  const className = enclosingClassName(path);
  if (className) {
    const declared =
      isDeclaredIvarInAst(ast, className, name) || (files ? isDeclaredIvar(className, name, files) : false);
    if (declared) {
      return { kind: 'ivar', name, className, tok };
    }
  }
  if (/^[A-Z]/.test(name)) {
    return reject(`"${name}" looks like a class — class and selector rename aren't supported in this version.`);
  }
  return reject(`"${name}" doesn't resolve to a temporary, argument, or instance variable in scope.`);
}

/** The class enclosing `offset`, if any — lets the server pre-filter cross-file ivar candidates. */
export function enclosingClassNameAt(ast: ProgramNode, offset: number): string | undefined {
  return enclosingClassName(pathToOffset(ast, offset));
}

/** prepareRename: the renameable range, or a typed rejection. `files` lets ivars resolve cross-file. */
export function prepareRenameAt(
  ast: ProgramNode,
  tokens: Token[],
  _symbols: SymbolNode[],
  offset: number,
  files?: ReadonlyArray<FileText>,
): RenameRange | RenameReject {
  const c = classify(ast, tokens, offset, files);
  if (isReject(c)) {
    return c;
  }
  return { range: { start: c.tok.startPos, end: c.tok.endPos } };
}

const toTextEdit = (r: Ranged, newText: string): TextEdit => ({
  range: { start: r.startPos, end: r.endPos },
  newText,
});

/** rename: a WorkspaceEdit, or a typed rejection. `files` must include the cursor's file. */
export function renameAt(
  uri: string,
  offset: number,
  newName: string,
  files: ReadonlyArray<FileText>,
): WorkspaceEdit | RenameReject {
  const primary = files.find((f) => f.uri === uri);
  if (!primary) {
    return reject('The file being renamed is not available.');
  }
  const ast = parse(primary.text).ast;
  const tokens = tokenize(primary.text).tokens;

  const c = classify(ast, tokens, offset, files);
  if (isReject(c)) {
    return c;
  }
  if (!IDENTIFIER.test(newName)) {
    return reject(`"${newName}" is not a valid Smalltalk identifier.`);
  }
  if (newName === c.name) {
    return { changes: {} };
  }

  const path = pathToOffset(ast, offset);

  if (c.kind === 'local') {
    const scope = bindingScope(path, c.name);
    if (boundNames(scope).has(newName)) {
      return reject(`"${newName}" already exists in this scope — rename would collide.`);
    }
    const className = enclosingClassName(path);
    if (className && isDeclaredIvar(className, newName, files)) {
      return reject(`"${newName}" is an instance variable of ${className} — rename would collide.`);
    }
    const edits = variableOccurrences(scope, c.name).map((o) => toTextEdit(o, newName));
    return { changes: { [uri]: edits } };
  }

  // Instance variable: workspace-wide.
  if (isDeclaredIvar(c.className, newName, files)) {
    return reject(`"${newName}" is already an instance variable of ${c.className} — rename would collide.`);
  }
  const byUri = ivarOccurrences(c.className, c.name, files);
  const changes: Record<string, TextEdit[]> = {};
  for (const [u, ranges] of byUri) {
    changes[u] = ranges.map((r) => toTextEdit(r, newName));
  }
  return { changes };
}

const MULTI_FILE_ANNOTATION = 'smalltalk.rename.multiFile';

/** When a rename touches more than one file, mark its edits `needsConfirmation` so VS Code routes
 *  them through the Refactor Preview (you must review + confirm, never applied blind on Enter).
 *  Single-file renames stay instant. Falls back to plain `changes` when the client lacks
 *  change-annotation support. */
export function withMultiFileConfirmation(
  edit: WorkspaceEdit,
  supported: boolean,
  versionOf: (uri: string) => number | null,
): WorkspaceEdit {
  const changes = edit.changes;
  if (!changes) {
    return edit;
  }
  const uris = Object.keys(changes).filter((u) => (changes[u]?.length ?? 0) > 0);
  if (uris.length <= 1 || !supported) {
    return edit;
  }
  return {
    documentChanges: uris.map((uri) => ({
      textDocument: { uri, version: versionOf(uri) },
      edits: (changes[uri] ?? []).map((e) => ({ ...e, annotationId: MULTI_FILE_ANNOTATION })),
    })),
    changeAnnotations: {
      [MULTI_FILE_ANNOTATION]: {
        label: 'Rename across files',
        needsConfirmation: true,
        description: `Renames in ${uris.length} files — review before applying.`,
      },
    },
  };
}
