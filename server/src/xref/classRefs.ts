// Workspace-wide class reference resolver (US-428 class rename).
//
// Given a class name, a ClassWorld (known-class + namespace resolution), and a set
// of (uri, text) files, find every occurrence that RESOLVES to that class across
// all files — its declaration, receiver/superclass references, `class`/`extend`
// receivers, class-defining symbol arguments, the binding constant `#{Foo}`, and
// the qualified forms `A.B` / `A::B` (class segment only). Never a local temp/arg
// that merely shares the name; never a same-named class in another namespace; never
// a comment or string. Pure: parses + tokenizes each file; reuses the ivar helpers
// for the `Foo` / `Foo class` receiver shapes.

import {
  NodeKind,
  type MessageNode,
  type Node,
  type ProgramNode,
} from '../parser/ast';
import { parse } from '../parser/parser';
import { tokenize } from '../parser/lexer';
import { buildSymbolTable, SymbolKind, type SymbolNode } from '../parser/symbols';
import { TokenKind, type Token } from '../parser/token';
import type { Ranged } from '../parser/scope';
import type { FileText } from './ivarRefs';

export type { FileText };

/** Class-resolution context: is-known-class predicates + namespace lookup. Built
 *  from the workspace index ∪ cartridge (server) or from a file set (eval/tests). */
export interface ClassWorld {
  /** Whether `name` is a class defined in the workspace (renameable). */
  isKnownWorkspaceClass(name: string): boolean;
  /** Whether `name` is a kernel/cartridge class (read-only; rename rejected). */
  isKernelClass(name: string): boolean;
  /** The namespace (`containerName`) of workspace class `name`; undefined = the
   *  top-level `Smalltalk` default namespace. */
  namespaceOf(name: string): string | undefined;
}

const isQualified = (name: string): boolean => name.includes('::') || name.includes('.');

/** Whether the qualified `path` (`A::B` / `A.B`) resolves to `className` given the
 *  class's own namespace: the last segment must be the class and the immediate
 *  qualifier must be `Smalltalk` (for a top-level class) or the class's namespace. */
function qualifiedResolves(path: string, className: string, world: ClassWorld): boolean {
  const parts = path.split(/::|\./);
  if (parts[parts.length - 1] !== className) {
    return false;
  }
  const qualifier = parts[parts.length - 2];
  if (qualifier === undefined) {
    return true;
  }
  const ns = world.namespaceOf(className);
  return (qualifier === 'Smalltalk' && ns === undefined) || qualifier === ns;
}

/** A Ranged built from a token's sub-slice `[tok.start+from, tok.start+from+len)`
 *  on the token's (single) line — for `#Foo` / `#'Foo'` symbol declarations. */
function subRange(tok: Token, from: number, len: number): Ranged {
  return {
    start: tok.start + from,
    end: tok.start + from + len,
    startPos: { line: tok.startPos.line, character: tok.startPos.character + from },
    endPos: { line: tok.startPos.line, character: tok.startPos.character + from + len },
  };
}

/** The Ranged of the class-segment Identifier token named `className` within
 *  `[from, to)` (the LAST match — the last segment of a qualified/binding path). */
function segmentRange(tokens: Token[], from: number, to: number, className: string): Ranged | undefined {
  let found: Token | undefined;
  for (const t of tokens) {
    if (t.start >= from && t.end <= to && t.kind === TokenKind.Identifier && t.text === className) {
      found = t;
    }
  }
  return found ? { start: found.start, end: found.end, startPos: found.startPos, endPos: found.endPos } : undefined;
}

/** The class name a subclass-family message declares (`… subclass: #Foo …`), when
 *  the symbol argument names `className`; returns the symbol arg's Ranged. */
function subclassSymbolRange(msg: MessageNode, className: string): Ranged | undefined {
  if (msg.messageType !== 'keyword' || !msg.selector.includes('subclass:')) {
    return undefined;
  }
  for (const arg of msg.arguments) {
    if (arg.kind === NodeKind.Literal && arg.literalKind === 'symbol') {
      const idx = arg.value.indexOf(className);
      // Only a symbol that IS the class name (`#Foo` / `#'Foo'`), not one containing it.
      if (idx > 0 && arg.value.replace(/^#/, '').replace(/^'(.*)'$/, '$1') === className) {
        // `arg` is a single Symbol token; its Ranged mirrors the token.
        return subRange(
          { start: arg.start, end: arg.end, startPos: arg.startPos, endPos: arg.endPos, kind: TokenKind.Symbol, text: arg.value } as Token,
          idx,
          className.length,
        );
      }
    }
  }
  return undefined;
}

class Collector {
  private readonly out: Ranged[] = [];

  constructor(
    private readonly tokens: Token[],
    private readonly className: string,
    private readonly world: ClassWorld,
  ) {}

  collect(program: ProgramNode): Ranged[] {
    const scope: Set<string>[] = [new Set(program.temporaries.map((t) => t.name))];
    for (const stmt of program.statements) {
      this.walk(stmt, scope);
    }
    return this.out;
  }

  /** Whether `name` is bound as a local temp/arg in any enclosing scope frame. */
  private isLocal(name: string, scope: Set<string>[]): boolean {
    return scope.some((frame) => frame.has(name));
  }

  private emitVariable(name: string, node: Node, scope: Set<string>[]): void {
    if (!isQualified(name)) {
      if (name === this.className && !this.isLocal(name, scope)) {
        this.out.push({ start: node.start, end: node.end, startPos: node.startPos, endPos: node.endPos });
      }
      return;
    }
    // Qualified `A::B` / `A.B` — a namespace path, never a local. Rewrite the
    // class segment only, and only when the whole path resolves to the target.
    if (qualifiedResolves(name, this.className, this.world)) {
      const seg = segmentRange(this.tokens, node.start, node.end, this.className);
      if (seg) this.out.push(seg);
    }
  }

  private walk(node: Node, scope: Set<string>[]): void {
    switch (node.kind) {
      case NodeKind.Variable:
        this.emitVariable(node.name, node, scope);
        return;
      case NodeKind.BindingConstant: {
        // `#{Foo}` / `#{NS::Foo}` — resolution-gated class segment.
        const path = node.path;
        const last = path.split(/::|\./).pop();
        if (last === this.className && (!isQualified(path) || qualifiedResolves(path, this.className, this.world))) {
          const seg = segmentRange(this.tokens, node.start, node.end, this.className);
          if (seg) this.out.push(seg);
        }
        return;
      }
      case NodeKind.Message: {
        const symDecl = subclassSymbolRange(node, this.className);
        if (symDecl) this.out.push(symDecl);
        this.walk(node.receiver, scope);
        for (const arg of node.arguments) this.walk(arg, scope);
        return;
      }
      case NodeKind.Definition: {
        // Chunk-form `subclass: #Foo` symbol declaration (brace-form bare-id is a
        // Variable, caught below by walking the definer).
        if (node.definer.kind === NodeKind.Message) {
          const symDecl = subclassSymbolRange(node.definer, this.className);
          if (symDecl) this.out.push(symDecl);
        }
        this.walk(node.definer, scope);
        for (const item of node.body) this.walk(item, scope);
        return;
      }
      case NodeKind.MethodDefinition: {
        if (node.target) this.walk(node.target, scope);
        const frame = new Set<string>([...node.parameters, ...node.temporaries].map((p) => p.name));
        const child = [...scope, frame];
        for (const pr of node.pragmas) this.walk(pr, child);
        for (const s of node.statements) this.walk(s, child);
        return;
      }
      case NodeKind.Block: {
        const frame = new Set<string>([...node.parameters, ...node.temporaries].map((p) => p.name));
        const child = [...scope, frame];
        for (const s of node.statements) this.walk(s, child);
        return;
      }
      case NodeKind.CompileTimeConstant:
      case NodeKind.DynamicArray: {
        const frame = new Set<string>(node.temporaries.map((t) => t.name));
        const child = node.temporaries.length > 0 ? [...scope, frame] : scope;
        for (const c of childrenOf(node)) this.walk(c, child);
        return;
      }
      default:
        for (const c of childrenOf(node)) this.walk(c, scope);
        return;
    }
  }
}

/** Direct child nodes of `node` (arrays + node-valued fields), for the generic walk. */
function childrenOf(node: Node): Node[] {
  const out: Node[] = [];
  for (const key of Object.keys(node) as (keyof Node)[]) {
    const v = node[key] as unknown;
    if (Array.isArray(v)) {
      for (const c of v) if (c && typeof c === 'object' && 'kind' in c) out.push(c as Node);
    } else if (v && typeof v === 'object' && 'kind' in (v as object)) {
      out.push(v as Node);
    }
  }
  return out;
}

/** All occurrence ranges of `className` within one parsed file, resolution-gated. */
export function classOccurrencesInAst(
  ast: ProgramNode,
  tokens: Token[],
  className: string,
  world: ClassWorld,
): Ranged[] {
  return new Collector(tokens, className, world).collect(ast);
}

/** Workspace-wide: occurrence ranges of `className`, keyed by file uri. */
export function classOccurrences(
  className: string,
  world: ClassWorld,
  files: ReadonlyArray<FileText>,
): Map<string, Ranged[]> {
  const result = new Map<string, Ranged[]>();
  for (const { uri, text } of files) {
    const ranges = classOccurrencesInAst(parse(text).ast, tokenize(text).tokens, className, world);
    if (ranges.length > 0) {
      result.set(uri, ranges);
    }
  }
  return result;
}

/** Collect (className → namespace) for every class/namespace defined in `symbols`. */
function collectClasses(symbols: SymbolNode[], container: string | undefined, out: Map<string, string | undefined>): void {
  for (const n of symbols) {
    if (n.kind === SymbolKind.Class) {
      if (!out.has(n.name)) out.set(n.name, container);
    }
    const next = n.kind === SymbolKind.Class || n.kind === SymbolKind.Namespace ? n.name : container;
    collectClasses(n.children, next, out);
  }
}

/** Build a ClassWorld from a file set (used by the eval + as a server fallback):
 *  workspace classes + namespaces come from parsing the files; kernel membership
 *  is supplied by `isKernelClass` (the cartridge on the server). */
export function buildClassWorldFromFiles(
  files: ReadonlyArray<FileText>,
  isKernelClass: (name: string) => boolean,
): ClassWorld {
  const classes = new Map<string, string | undefined>();
  for (const { text } of files) {
    collectClasses(buildSymbolTable(parse(text).ast), undefined, classes);
  }
  return {
    isKnownWorkspaceClass: (name) => classes.has(name),
    isKernelClass,
    namespaceOf: (name) => classes.get(name),
  };
}
