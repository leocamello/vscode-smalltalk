// Semantic-tokens provider (US-422).
//
// Colors Smalltalk by ROLE — instance var vs class var vs temp vs parameter vs
// known class vs unknown global vs selector part vs pseudo-variable — from the
// US-411 AST + symbol-table scopes. The one part that is distinctly ours and
// offline: a capitalized identifier is a `class` only when it resolves to a
// known class in workspace ∪ cartridge (AC2) — so kernel classes light up with
// no `gst`; an unknown capitalized name is a global. With no cartridge loaded we
// fall back to "capitalized ⇒ class" (AC4).
//
// Pure: vscode-languageserver-types only; never throws (the caller hands in
// already-parsed inputs). Emits the LSP delta-encoded integer stream via
// `encodeSemanticTokens`; the structured `RawSemanticToken[]` is what the unit
// tests + eval assert on (readability).

import { NodeKind, type DefinitionNode, type MethodDefinitionNode, type NameRef, type Node, type ProgramNode } from '../parser/ast';
import { SymbolKind, type SymbolNode } from '../parser/symbols';
import type { Position, Token } from '../parser/token';
import { childNodes } from '../parser/walk';

/** The token-type legend (standard LSP types, so themes color with no config). */
export const SEMANTIC_TOKEN_TYPES = ['class', 'property', 'variable', 'parameter', 'method', 'keyword'] as const;
/** The token-modifier legend. `static` ⇒ class variable; `defaultLibrary` ⇒ a
 *  class sourced from the cartridge (kernel) rather than the workspace. */
export const SEMANTIC_TOKEN_MODIFIERS = ['static', 'defaultLibrary'] as const;

export type SemanticTokenType = (typeof SEMANTIC_TOKEN_TYPES)[number];
export type SemanticTokenModifier = (typeof SEMANTIC_TOKEN_MODIFIERS)[number];

/** A classified token before LSP delta-encoding (one per identifier/selector). */
export interface RawSemanticToken {
  readonly line: number;
  readonly char: number;
  readonly length: number;
  readonly type: SemanticTokenType;
  readonly modifiers: SemanticTokenModifier[];
}

/** Lazy lookups the server resolves from the workspace index ∪ active cartridge. */
export interface SemanticTokenContext {
  /** Whether a kernel cartridge is loaded. When false, a capitalized identifier
   *  falls back to `class` (AC4) instead of being treated as an unknown global. */
  readonly hasCartridge: boolean;
  /** Where `name` resolves as a class, or undefined when it is not a known class. */
  readonly classOrigin: (name: string) => 'workspace' | 'cartridge' | undefined;
}

const PSEUDO_VARIABLES = new Set(['self', 'super', 'nil', 'true', 'false', 'thisContext']);

function isCapitalized(name: string): boolean {
  return /^[A-Z]/.test(name);
}

/** A class name from a `Foo` reference or a `Foo class` message; else undefined. */
function classNameOf(node: Node): string | undefined {
  if (node.kind === NodeKind.Variable) {
    return node.name;
  }
  if (node.kind === NodeKind.Message && node.messageType === 'unary' && node.selector === 'class') {
    return classNameOf(node.receiver);
  }
  return undefined;
}

/** The class a definition targets (the new class for `subclass:`, the existing
 *  class for `extend`/`class`-scope), mirroring the symbol builder + hover. */
function definitionClassName(node: DefinitionNode): string | undefined {
  if (node.name) {
    return node.name;
  }
  const def = node.definer;
  if (def.kind === NodeKind.Message) {
    return classNameOf(def.receiver);
  }
  if (def.kind === NodeKind.Variable) {
    return def.name;
  }
  return undefined;
}

/** className → (field name → instance/class variable), from the symbol table. */
function collectFields(symbols: SymbolNode[]): Map<string, Map<string, 'instance' | 'class'>> {
  const out = new Map<string, Map<string, 'instance' | 'class'>>();
  const walk = (nodes: SymbolNode[]): void => {
    for (const n of nodes) {
      if (n.kind === SymbolKind.Class) {
        const fields = out.get(n.name) ?? new Map<string, 'instance' | 'class'>();
        for (const child of n.children) {
          if (child.kind === SymbolKind.InstanceVariable) fields.set(child.name, 'instance');
          else if (child.kind === SymbolKind.ClassVariable) fields.set(child.name, 'class');
        }
        out.set(n.name, fields);
      }
      walk(n.children);
    }
  };
  walk(symbols);
  return out;
}

/** The first token in [from, to) whose exact text is `text`. */
function findToken(tokens: Token[], from: number, to: number, text: string): Token | undefined {
  for (const t of tokens) {
    if (t.start >= from && t.end <= to && t.text === text) {
      return t;
    }
  }
  return undefined;
}

/** The selector token(s) of a send/method/pragma, derived from the gaps between
 *  the start offset and each argument (so a nested selector isn't matched). */
function selectorTokens(
  selector: string,
  messageType: 'unary' | 'binary' | 'keyword',
  from: number,
  args: ReadonlyArray<{ start: number; end: number }>,
  end: number,
  tokens: Token[],
): Token[] {
  const out: Token[] = [];
  if (messageType === 'keyword') {
    const parts = selector.match(/[^:]+:/g) ?? [];
    let cursor = from;
    for (let i = 0; i < parts.length; i++) {
      const arg = args[i];
      const to = arg ? arg.start : end;
      const tok = findToken(tokens, cursor, to, parts[i] as string);
      if (tok) out.push(tok);
      cursor = arg ? arg.end : to;
    }
  } else if (messageType === 'binary') {
    const arg = args[0];
    const to = arg ? arg.start : end;
    const tok = findToken(tokens, from, to, selector);
    if (tok) out.push(tok);
  } else {
    const tok = findToken(tokens, from, end, selector);
    if (tok) out.push(tok);
  }
  return out;
}

/** The offset at which a method's body begins (first temp/pragma/statement), so
 *  selector extraction never reaches into the body. */
function methodBodyStart(m: MethodDefinitionNode): number {
  let start = m.end;
  for (const n of [...m.temporaries, ...m.pragmas, ...m.statements]) {
    if (n.start < start) start = n.start;
  }
  return start;
}

class Collector {
  private readonly out: RawSemanticToken[] = [];
  private readonly seen = new Set<string>();

  constructor(
    private readonly tokens: Token[],
    private readonly fields: Map<string, Map<string, 'instance' | 'class'>>,
    private readonly ctx: SemanticTokenContext,
  ) {}

  collect(program: ProgramNode): RawSemanticToken[] {
    const scope = new Map<string, 'parameter' | 'temporary'>();
    for (const t of program.temporaries) {
      scope.set(t.name, 'temporary');
      this.emitDecl(t, 'variable');
    }
    for (const stmt of program.statements) {
      this.walk(stmt, [scope], undefined);
    }
    return this.out;
  }

  private push(line: number, char: number, length: number, type: SemanticTokenType, modifiers: SemanticTokenModifier[] = []): void {
    if (length <= 0) return;
    const key = `${line}:${char}`;
    if (this.seen.has(key)) return;
    this.seen.add(key);
    this.out.push({ line, char, length, type, modifiers });
  }

  private emitRange(start: Position, end: Position, type: SemanticTokenType, modifiers: SemanticTokenModifier[] = []): void {
    if (start.line !== end.line) return; // identifiers/selectors never span lines
    this.push(start.line, start.character, end.character - start.character, type, modifiers);
  }

  private emitDecl(ref: NameRef, type: SemanticTokenType, modifiers: SemanticTokenModifier[] = []): void {
    this.emitRange(ref.startPos, ref.endPos, type, modifiers);
  }

  private emitToken(tok: Token, type: SemanticTokenType, modifiers: SemanticTokenModifier[] = []): void {
    this.emitRange(tok.startPos, tok.endPos, type, modifiers);
  }

  /** Classify a variable/identifier reference by scope, class field, then class/global. */
  private classifyVariable(
    name: string,
    scopes: Map<string, 'parameter' | 'temporary'>[],
    className: string | undefined,
  ): { type: SemanticTokenType; modifiers: SemanticTokenModifier[] } {
    if (PSEUDO_VARIABLES.has(name)) {
      return { type: 'keyword', modifiers: [] };
    }
    for (let i = scopes.length - 1; i >= 0; i--) {
      const bound = scopes[i]?.get(name);
      if (bound) {
        return { type: bound === 'parameter' ? 'parameter' : 'variable', modifiers: [] };
      }
    }
    if (className) {
      const field = this.fields.get(className)?.get(name);
      if (field === 'instance') return { type: 'property', modifiers: [] };
      if (field === 'class') return { type: 'property', modifiers: ['static'] };
    }
    const origin = this.ctx.classOrigin(name);
    if (origin === 'cartridge') return { type: 'class', modifiers: ['defaultLibrary'] };
    if (origin === 'workspace') return { type: 'class', modifiers: [] };
    if (isCapitalized(name)) {
      // Unknown capitalized: a class fallback with no cartridge (AC4), else a global.
      return this.ctx.hasCartridge ? { type: 'variable', modifiers: [] } : { type: 'class', modifiers: [] };
    }
    return { type: 'variable', modifiers: [] };
  }

  private emitSelector(
    selector: string,
    messageType: 'unary' | 'binary' | 'keyword',
    from: number,
    args: ReadonlyArray<{ start: number; end: number }>,
    end: number,
  ): void {
    if (selector === '') return;
    for (const tok of selectorTokens(selector, messageType, from, args, end, this.tokens)) {
      this.emitToken(tok, 'method');
    }
  }

  private walk(node: Node, scopes: Map<string, 'parameter' | 'temporary'>[], className: string | undefined): void {
    switch (node.kind) {
      case NodeKind.Variable: {
        const { type, modifiers } = this.classifyVariable(node.name, scopes, className);
        this.emitRange(node.startPos, node.endPos, type, modifiers);
        return;
      }
      case NodeKind.Message: {
        this.emitSelector(node.selector, node.messageType, node.receiver.end, node.arguments, node.end);
        for (const child of childNodes(node)) this.walk(child, scopes, className);
        return;
      }
      case NodeKind.Pragma: {
        this.emitSelector(node.selector, selectorTypeOf(node.selector), node.start, node.arguments, node.end);
        for (const child of childNodes(node)) this.walk(child, scopes, className);
        return;
      }
      case NodeKind.Definition: {
        const inner = definitionClassName(node) ?? className;
        this.walk(node.definer, scopes, className);
        for (const item of node.body) this.walk(item, scopes, inner);
        return;
      }
      case NodeKind.MethodDefinition: {
        const inner = node.target ? classNameOf(node.target) ?? className : className;
        this.emitSelector(
          node.selector,
          node.messageType,
          node.target ? node.target.end : node.start,
          node.parameters,
          methodBodyStart(node),
        );
        const local = new Map<string, 'parameter' | 'temporary'>();
        for (const p of node.parameters) {
          local.set(p.name, 'parameter');
          this.emitDecl(p, 'parameter');
        }
        for (const t of node.temporaries) {
          local.set(t.name, 'temporary');
          this.emitDecl(t, 'variable');
        }
        const childScopes = [...scopes, local];
        if (node.target) this.walk(node.target, scopes, className);
        for (const pr of node.pragmas) this.walk(pr, childScopes, inner);
        for (const s of node.statements) this.walk(s, childScopes, inner);
        return;
      }
      case NodeKind.Block: {
        const local = new Map<string, 'parameter' | 'temporary'>();
        for (const p of node.parameters) {
          local.set(p.name, 'parameter');
          this.emitDecl(p, 'parameter');
        }
        for (const t of node.temporaries) {
          local.set(t.name, 'temporary');
          this.emitDecl(t, 'variable');
        }
        const childScopes = [...scopes, local];
        for (const s of node.statements) this.walk(s, childScopes, className);
        return;
      }
      case NodeKind.CompileTimeConstant:
      case NodeKind.DynamicArray: {
        const local = new Map<string, 'parameter' | 'temporary'>();
        for (const t of node.temporaries) {
          local.set(t.name, 'temporary');
          this.emitDecl(t, 'variable');
        }
        const childScopes = node.temporaries.length > 0 ? [...scopes, local] : scopes;
        for (const child of childNodes(node)) this.walk(child, childScopes, className);
        return;
      }
      case NodeKind.InstanceVariables: {
        for (const name of node.names) {
          const field = className ? this.fields.get(className)?.get(name.name) : undefined;
          this.emitDecl(name, 'property', field === 'class' ? ['static'] : []);
        }
        return;
      }
      default: {
        for (const child of childNodes(node)) this.walk(child, scopes, className);
        return;
      }
    }
  }
}

/** Whether a selector reads as unary/binary/keyword (for pragmas, which carry no messageType). */
function selectorTypeOf(selector: string): 'unary' | 'binary' | 'keyword' {
  if (selector.includes(':')) return 'keyword';
  return /^[A-Za-z_]/.test(selector) ? 'unary' : 'binary';
}

/** Classify every identifier/selector in `program` into role-tagged tokens. */
export function collectSemanticTokens(
  program: ProgramNode,
  symbols: SymbolNode[],
  tokens: Token[],
  ctx: SemanticTokenContext,
): RawSemanticToken[] {
  return new Collector(tokens, collectFields(symbols), ctx).collect(program);
}

const TYPE_INDEX: Record<string, number> = Object.fromEntries(SEMANTIC_TOKEN_TYPES.map((t, i) => [t, i]));
const MODIFIER_INDEX: Record<string, number> = Object.fromEntries(SEMANTIC_TOKEN_MODIFIERS.map((m, i) => [m, i]));

/** LSP delta-encode classified tokens into the `(deltaLine, deltaStartChar,
 *  length, tokenType, tokenModifiers)` integer stream. */
export function encodeSemanticTokens(raw: RawSemanticToken[]): number[] {
  const sorted = [...raw].sort((a, b) => a.line - b.line || a.char - b.char);
  const data: number[] = [];
  let prevLine = 0;
  let prevChar = 0;
  for (const t of sorted) {
    const deltaLine = t.line - prevLine;
    const deltaChar = deltaLine === 0 ? t.char - prevChar : t.char;
    let mods = 0;
    for (const m of t.modifiers) mods |= 1 << (MODIFIER_INDEX[m] ?? 0);
    data.push(deltaLine, deltaChar, t.length, TYPE_INDEX[t.type] ?? 0, mods);
    prevLine = t.line;
    prevChar = t.char;
  }
  return data;
}

interface LspRange {
  readonly start: Position;
  readonly end: Position;
}

function withinRange(t: RawSemanticToken, range: LspRange): boolean {
  const afterStart = t.line > range.start.line || (t.line === range.start.line && t.char + t.length > range.start.character);
  const beforeEnd = t.line < range.end.line || (t.line === range.end.line && t.char < range.end.character);
  return afterStart && beforeEnd;
}

/** Encoded semantic tokens for a whole document (AC1). */
export function semanticTokensFull(
  program: ProgramNode,
  symbols: SymbolNode[],
  tokens: Token[],
  ctx: SemanticTokenContext,
): number[] {
  return encodeSemanticTokens(collectSemanticTokens(program, symbols, tokens, ctx));
}

/** Encoded semantic tokens within `range` (the large-file variant; spec §7). */
export function semanticTokensRange(
  program: ProgramNode,
  symbols: SymbolNode[],
  tokens: Token[],
  ctx: SemanticTokenContext,
  range: LspRange,
): number[] {
  return encodeSemanticTokens(collectSemanticTokens(program, symbols, tokens, ctx).filter((t) => withinRange(t, range)));
}
