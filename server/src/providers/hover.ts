// Hover provider (US-415).
//
// Surfaces, at the cursor, the facts the front end + indexes already know — so a
// developer understands a selector, class, variable, or numeric literal without
// navigating away. Classifies the node under the cursor (mirroring definition.ts's
// deepest-node walk, extended to literals and in-scope variables) and renders
// Markdown with code fences (AC5). Pure: vscode-languageserver-types only; never
// throws (the caller hands in already-parsed inputs).
//
// Prose is gated on PROVENANCE, not dialect: the frozen reference cartridge is
// facts-only (no comment), while an installed-kernel or workspace source may carry
// comments. The HoverContext resolvers return a comment only when provenance allows,
// so this provider just renders whatever it is given (spec.md §4a).

import { type Hover, type Range, MarkupKind } from 'vscode-languageserver-types';
import { NodeKind, type LiteralNode, type Node, type ProgramNode } from '../parser/ast';
import { SymbolKind, type SymbolNode } from '../parser/symbols';
import type { Position } from '../parser/token';
import { childNodes, visit } from '../parser/walk';
import type { Provenance } from '../kernel/model';

/** An implementor of a selector (workspace or active kernel). `comment` is present
 *  only when the source's provenance allows prose. */
export interface HoverImplementor {
  readonly className: string;
  readonly provenance: Provenance;
  readonly comment?: string;
}

/** Lazy lookups the server resolves from the workspace index + active kernel.
 *  Lazy (not precomputed arrays) because hover fires once per interaction. */
export interface HoverContext {
  /** Is `name` a known class (workspace or active kernel)? */
  readonly isClass: (name: string) => boolean;
  /** Immediate superclass of `name`, or undefined at the root / when unknown. */
  readonly superclassOf: (name: string) => string | undefined;
  /** Classes implementing `selector`, across workspace + active kernel. */
  readonly implementorsOf: (selector: string) => readonly HoverImplementor[];
  /** Class comment for `name`, only when provenance allows prose (else undefined). */
  readonly classComment?: (name: string) => string | undefined;
}

type Target =
  | { readonly kind: 'literal'; readonly node: LiteralNode }
  | { readonly kind: 'variable'; readonly name: string; readonly node: Node }
  | { readonly kind: 'selector'; readonly name: string };

/** The deepest node under `offset`, classified as a literal / variable / selector. */
function targetAt(ast: ProgramNode, offset: number): Target | undefined {
  let best: { target: Target; start: number; end: number } | undefined;
  const consider = (target: Target, start: number, end: number): void => {
    if (!best || start > best.start || (start === best.start && end < best.end)) {
      best = { target, start, end };
    }
  };
  visit(ast, (node) => {
    if (offset < node.start || offset > node.end) {
      return;
    }
    if (node.kind === NodeKind.Literal) {
      consider({ kind: 'literal', node }, node.start, node.end);
    } else if (node.kind === NodeKind.Message && node.selector !== '' && offset >= node.receiver.end) {
      consider({ kind: 'selector', name: node.selector }, node.start, node.end);
    } else if (node.kind === NodeKind.Variable) {
      consider({ kind: 'variable', name: node.name, node }, node.start, node.end);
    }
  });
  return best?.target;
}

const MD = (value: string): Hover['contents'] => ({ kind: MarkupKind.Markdown, value });

function rangeOf(node: { startPos: Position; endPos: Position }): Range {
  return {
    start: { line: node.startPos.line, character: node.startPos.character },
    end: { line: node.endPos.line, character: node.endPos.character },
  };
}

// --- AC1: selectors ---------------------------------------------------------

/** A human signature for a selector: `printString`, `+ aValue`, `at: a1 put: a2`. */
function signatureOf(selector: string): string {
  if (selector.includes(':')) {
    const parts = selector.match(/[^:]+:/g) ?? [selector];
    return parts.map((p, i) => `${p} a${i + 1}`).join(' ');
  }
  return /^[A-Za-z_]/.test(selector) ? selector : `${selector} aValue`;
}

function selectorKind(selector: string): string {
  if (selector.includes(':')) {
    return 'keyword selector';
  }
  return /^[A-Za-z_]/.test(selector) ? 'unary selector' : 'binary selector';
}

function selectorHover(selector: string, ctx: HoverContext): Hover {
  const lines: string[] = [`**\`${selector}\`** — ${selectorKind(selector)}`, '', '```smalltalk', signatureOf(selector), '```'];

  const implementors = ctx.implementorsOf(selector);
  if (implementors.length > 0) {
    const names = [...new Set(implementors.map((i) => i.className))].sort();
    const shown = names.slice(0, 12).map((n) => `\`${n}\``).join(', ');
    const more = names.length > 12 ? ` _(+${names.length - 12} more)_` : '';
    lines.push('', `**Implementors:** ${shown}${more}`);
  }

  // Prose only when provenance allows it (installed kernel / workspace).
  const withComment = implementors.find((i) => i.comment && i.comment.trim() !== '');
  if (withComment?.comment) {
    lines.push('', withComment.comment.trim());
  }

  return { contents: MD(lines.join('\n')) };
}

// --- AC2: classes -----------------------------------------------------------

/** Walk the immediate-superclass map transitively (visited set + depth cap). */
function superclassChain(name: string, ctx: HoverContext): string[] {
  const chain: string[] = [name];
  const seen = new Set<string>([name]);
  let current = name;
  for (let i = 0; i < 50; i++) {
    const next = ctx.superclassOf(current);
    // `nil` is the Smalltalk root sentinel (`nil subclass: Object`), not a class
    // to display — stop the chain at the dialect root.
    if (next === undefined || next === null || next === 'nil' || seen.has(next)) {
      break;
    }
    chain.push(next);
    seen.add(next);
    current = next;
  }
  return chain;
}

function classHover(name: string, node: Node, ctx: HoverContext): Hover {
  const chain = superclassChain(name, ctx);
  const lines: string[] = [`**\`${name}\`** — class`, '', '```text', chain.join(' → '), '```'];
  const comment = ctx.classComment?.(name);
  if (comment && comment.trim() !== '') {
    lines.push('', comment.trim());
  }
  return { contents: MD(lines.join('\n')), range: rangeOf(node) };
}

// --- AC3: variables ---------------------------------------------------------

const VAR_LABEL: Record<string, string> = {
  parameter: 'parameter',
  temporary: 'temporary',
  instanceVariable: 'instance variable',
  classVariable: 'class variable',
};

interface ScopeVar {
  readonly kind: keyof typeof VAR_LABEL;
  readonly declPos: Position;
}

/** Root → deepest chain of nodes whose range contains `offset`. */
function pathToOffset(root: Node, offset: number): Node[] {
  const path: Node[] = [];
  let node: Node | undefined = root;
  while (node) {
    path.push(node);
    node = childNodes(node).find((c) => c.start <= offset && offset <= c.end);
  }
  return path;
}

function classNameOf(node: Node): string | undefined {
  if (node.kind === NodeKind.Variable) {
    return node.name;
  }
  if (node.kind === NodeKind.Message && node.messageType === 'unary' && node.selector === 'class') {
    return classNameOf(node.receiver);
  }
  return undefined;
}

function enclosingClassName(path: Node[]): string | undefined {
  for (let i = path.length - 1; i >= 0; i--) {
    const node = path[i] as Node;
    if (node.kind === NodeKind.MethodDefinition && node.target) {
      const name = classNameOf(node.target);
      if (name) {
        return name;
      }
    } else if (node.kind === NodeKind.Definition) {
      if (node.name) {
        return node.name;
      }
      const def = node.definer;
      if (def.kind === NodeKind.Message) {
        const name = classNameOf(def.receiver);
        if (name) {
          return name;
        }
      } else if (def.kind === NodeKind.Variable) {
        return def.name;
      }
    }
  }
  return undefined;
}

function findClassSymbol(nodes: SymbolNode[], name: string): SymbolNode | undefined {
  for (const node of nodes) {
    if (node.kind === SymbolKind.Class && node.name === name) {
      return node;
    }
    const inner = findClassSymbol(node.children, name);
    if (inner) {
      return inner;
    }
  }
  return undefined;
}

/** Resolve `name` to an in-scope variable at `offset`: its kind + declaration
 *  position. Lexical params/temps (innermost first) shadow class fields. */
function resolveScopeVar(ast: ProgramNode, symbols: SymbolNode[], offset: number, name: string): ScopeVar | undefined {
  const path = pathToOffset(ast, offset);
  for (let i = path.length - 1; i >= 0; i--) {
    const node = path[i] as Node;
    if (node.kind === NodeKind.MethodDefinition || node.kind === NodeKind.Block) {
      for (const p of node.parameters) {
        if (p.name === name) {
          return { kind: 'parameter', declPos: p.startPos };
        }
      }
      for (const t of node.temporaries) {
        if (t.name === name) {
          return { kind: 'temporary', declPos: t.startPos };
        }
      }
    } else if (
      node.kind === NodeKind.Program ||
      node.kind === NodeKind.CompileTimeConstant ||
      node.kind === NodeKind.DynamicArray
    ) {
      for (const t of node.temporaries) {
        if (t.name === name) {
          return { kind: 'temporary', declPos: t.startPos };
        }
      }
    }
  }

  const className = enclosingClassName(path);
  if (className) {
    const cls = findClassSymbol(symbols, className);
    for (const child of cls?.children ?? []) {
      if (child.name !== name) {
        continue;
      }
      if (child.kind === SymbolKind.InstanceVariable) {
        return { kind: 'instanceVariable', declPos: child.selectionRange.startPos };
      }
      if (child.kind === SymbolKind.ClassVariable) {
        return { kind: 'classVariable', declPos: child.selectionRange.startPos };
      }
    }
  }
  return undefined;
}

function variableHover(name: string, node: Node, v: ScopeVar): Hover {
  const lines: string[] = [
    `**\`${name}\`** — ${VAR_LABEL[v.kind]}`,
    '',
    '```smalltalk',
    name,
    '```',
    '',
    `Declared at line ${v.declPos.line + 1}, column ${v.declPos.character + 1}`,
  ];
  return { contents: MD(lines.join('\n')), range: rangeOf(node) };
}

// --- AC4: numeric literals --------------------------------------------------

/** Decode a numeric literal's raw text to a human value line (radix → decimal,
 *  scaled decimal → value + scale). Returns undefined for non-numeric literals. */
function decodeNumeric(node: LiteralNode): string | undefined {
  const raw = node.value;
  if (node.literalKind === 'integer') {
    const radix = /^(-?)(\d+)r([0-9A-Za-z]+)$/.exec(raw);
    if (radix) {
      const base = parseInt(radix[2] as string, 10);
      const value = parseInt(radix[3] as string, base);
      if (base >= 2 && base <= 36 && !Number.isNaN(value)) {
        const signed = radix[1] === '-' ? -value : value;
        return `${raw} = ${signed} (base ${base})`;
      }
    }
    return `${raw} = ${raw}`;
  }
  if (node.literalKind === 'scaledDecimal') {
    const m = /^(-?[0-9]+(?:\.[0-9]+)?)s(\d*)$/.exec(raw);
    if (m) {
      const mantissa = m[1] as string;
      const scale = (m[2] as string) === '' ? `default` : (m[2] as string);
      return `${raw} = ${mantissa} (scale ${scale})`;
    }
    return `${raw}`;
  }
  if (node.literalKind === 'float') {
    const value = parseFloat(raw);
    return Number.isNaN(value) ? `${raw}` : `${raw} = ${value}`;
  }
  return undefined;
}

function literalHover(node: LiteralNode): Hover | null {
  const decoded = decodeNumeric(node);
  if (decoded === undefined) {
    return null; // strings/symbols/chars carry no extra hover today
  }
  const lines: string[] = [`**${node.literalKind} literal**`, '', '```text', decoded, '```'];
  return { contents: MD(lines.join('\n')), range: rangeOf(node) };
}

// --- entry point ------------------------------------------------------------

/** Markdown hover at byte `offset`, or null when nothing is hoverable there. */
export function hoverAt(
  offset: number,
  _text: string,
  _tokens: unknown,
  ast: ProgramNode,
  symbols: SymbolNode[],
  ctx: HoverContext,
): Hover | null {
  const target = targetAt(ast, offset);
  if (!target) {
    return null;
  }
  if (target.kind === 'literal') {
    return literalHover(target.node);
  }
  if (target.kind === 'selector') {
    return selectorHover(target.name, ctx);
  }
  // variable: an in-scope binding wins; otherwise a class reference.
  const scopeVar = resolveScopeVar(ast, symbols, offset, target.name);
  if (scopeVar) {
    return variableHover(target.name, target.node, scopeVar);
  }
  if (ctx.isClass(target.name)) {
    return classHover(target.name, target.node, ctx);
  }
  return null;
}
