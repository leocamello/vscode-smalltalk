// Completion provider (US-413, slice C / AC2–AC4 + AC7 provenance).
//
// Offers, at the cursor:
//   - SELECTOR context (after a receiver / cascade `;`): unary + keyword
//     selectors from the workspace and the active kernel, ranked
//     workspace > installed-kernel > bundled-kernel. Keyword selectors insert as
//     snippets (`at:put:` → `at:${1} put:${2}`).
//   - HEAD context (statement/expression start, after `^ := ( [ { | , binary` or a
//     keyword part): in-scope variables (temps/params/instance/class vars from the
//     symbol-table scopes) followed by class names.
// Context is decided from the token stream (robust at an incomplete cursor); the
// AST/symbol table supplies in-scope variables. Pure — vscode-languageserver-types
// only; never throws (the caller hands in already-parsed inputs).

import { type CompletionItem, CompletionItemKind, InsertTextFormat } from 'vscode-languageserver-types';
import { Provenance } from '../kernel/model';
import { NodeKind, type Node, type ProgramNode } from '../parser/ast';
import { SymbolKind, type SymbolNode } from '../parser/symbols';
import { TokenKind, type Token } from '../parser/token';
import { childNodes } from '../parser/walk';

/** A selector offered to completion, with where it came from. */
export interface SelectorCandidate {
  readonly selector: string;
  readonly provenance: Provenance;
}

/** A class name offered to completion, with where it came from. */
export interface ClassCandidate {
  readonly name: string;
  readonly provenance: Provenance;
}

type Context = 'selector' | 'head';

const IDENT = /[A-Za-z0-9_]/;

/** Token kinds that end a primary expression → a receiver precedes the cursor. */
const PRIMARY_END = new Set<TokenKind>([
  TokenKind.Identifier,
  TokenKind.RParen,
  TokenKind.RBracket,
  TokenKind.RBrace,
  TokenKind.String,
  TokenKind.Symbol,
  TokenKind.Char,
  TokenKind.Integer,
  TokenKind.Float,
  TokenKind.ScaledDecimal,
]);

function provenanceRank(p: Provenance): number {
  switch (p) {
    case Provenance.Workspace:
      return 1;
    case Provenance.InstalledKernel:
      return 2;
    case Provenance.BundledKernel:
      return 3;
    default:
      return 4;
  }
}

function provenanceLabel(p: Provenance): string {
  switch (p) {
    case Provenance.Workspace:
      return 'workspace';
    case Provenance.InstalledKernel:
      return 'kernel (installed)';
    case Provenance.BundledKernel:
      return 'kernel (reference)';
    default:
      return '';
  }
}

/** The identifier prefix immediately left of `offset`, plus where it starts. */
function prefixBefore(text: string, offset: number): { prefix: string; wordStart: number } {
  let i = offset;
  while (i > 0 && IDENT.test(text[i - 1] as string)) {
    i--;
  }
  return { prefix: text.slice(i, offset), wordStart: i };
}

/** The last non-trivia token ending at/before `wordStart`. */
function prevSignificantToken(tokens: Token[], wordStart: number): Token | undefined {
  let prev: Token | undefined;
  for (const t of tokens) {
    if (t.kind === TokenKind.Comment || t.kind === TokenKind.EOF) {
      continue;
    }
    if (t.end <= wordStart) {
      prev = t;
    } else {
      break;
    }
  }
  return prev;
}

function contextAt(prev: Token | undefined): Context {
  if (!prev) {
    return 'head';
  }
  if (prev.kind === TokenKind.Semicolon) {
    return 'selector'; // cascade — another message to the shared receiver
  }
  return PRIMARY_END.has(prev.kind) ? 'selector' : 'head';
}

/** Camel-hump initials: `OrderedCollection` → `oc`, `ifTrue:ifFalse:` → `itif`. */
function humpInitials(s: string): string {
  let out = '';
  for (let i = 0; i < s.length; i++) {
    const ch = s[i] as string;
    if (i === 0 || /[A-Z]/.test(ch) || (i > 0 && s[i - 1] === ':')) {
      out += ch.toLowerCase();
    }
  }
  return out;
}

/** Prefix or camel-hump match (empty prefix matches everything). */
function matches(candidate: string, prefix: string): boolean {
  if (prefix === '') {
    return true;
  }
  const p = prefix.toLowerCase();
  return candidate.toLowerCase().startsWith(p) || humpInitials(candidate).startsWith(p);
}

/** Identifier-led selectors only (unary/keyword); binary selectors aren't typed as words. */
function isOfferableSelector(selector: string): boolean {
  return /^[A-Za-z]/.test(selector);
}

function isKeywordSelector(selector: string): boolean {
  return selector.includes(':');
}

/** `at:put:` → `at:${1} put:${2}` (one tab stop per keyword part). */
function keywordSnippet(selector: string): string {
  const parts = selector.match(/[^:]+:/g) ?? [selector];
  return parts.map((part, i) => `${part}\${${i + 1}}`).join(' ');
}

/** Dedup, keeping the highest-confidence (lowest-rank) provenance per name. */
function bestByName<T extends { provenance: Provenance }>(
  candidates: T[],
  nameOf: (c: T) => string,
): Map<string, Provenance> {
  const best = new Map<string, Provenance>();
  for (const c of candidates) {
    const name = nameOf(c);
    const current = best.get(name);
    if (current === undefined || provenanceRank(c.provenance) < provenanceRank(current)) {
      best.set(name, c.provenance);
    }
  }
  return best;
}

interface ScopeVar {
  readonly name: string;
  readonly kind: 'temporary' | 'parameter' | 'instanceVariable' | 'classVariable';
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

/** A class name from a `Foo` reference or a `Foo class` message. */
function classNameOf(node: Node): string | undefined {
  if (node.kind === NodeKind.Variable) {
    return node.name;
  }
  if (node.kind === NodeKind.Message && node.messageType === 'unary' && node.selector === 'class') {
    return classNameOf(node.receiver);
  }
  return undefined;
}

/** The class owning the cursor, from a method's target or an enclosing definition. */
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

/** In-scope variables at `offset`: lexical temps/params + the enclosing class's vars. */
function scopeVariablesAt(ast: ProgramNode, symbols: SymbolNode[], offset: number): ScopeVar[] {
  const out: ScopeVar[] = [];
  const seen = new Set<string>();
  const add = (name: string, kind: ScopeVar['kind']): void => {
    if (!seen.has(name)) {
      seen.add(name);
      out.push({ name, kind });
    }
  };

  const path = pathToOffset(ast, offset);
  for (const node of path) {
    if (node.kind === NodeKind.MethodDefinition || node.kind === NodeKind.Block) {
      for (const p of node.parameters) {
        add(p.name, 'parameter');
      }
      for (const t of node.temporaries) {
        add(t.name, 'temporary');
      }
    } else if (
      node.kind === NodeKind.Program ||
      node.kind === NodeKind.CompileTimeConstant ||
      node.kind === NodeKind.DynamicArray
    ) {
      for (const t of node.temporaries) {
        add(t.name, 'temporary');
      }
    }
  }

  const className = enclosingClassName(path);
  if (className) {
    const cls = findClassSymbol(symbols, className);
    if (cls) {
      for (const child of cls.children) {
        if (child.kind === SymbolKind.InstanceVariable) {
          add(child.name, 'instanceVariable');
        } else if (child.kind === SymbolKind.ClassVariable) {
          add(child.name, 'classVariable');
        }
      }
    }
  }
  return out;
}

function selectorItem(selector: string, provenance: Provenance): CompletionItem {
  const item: CompletionItem = {
    label: selector,
    kind: CompletionItemKind.Method,
    detail: provenanceLabel(provenance),
    sortText: `${provenanceRank(provenance)}-${selector}`,
  };
  if (isKeywordSelector(selector)) {
    item.insertText = keywordSnippet(selector);
    item.insertTextFormat = InsertTextFormat.Snippet;
  }
  return item;
}

function classItem(name: string, provenance: Provenance): CompletionItem {
  return {
    label: name,
    kind: CompletionItemKind.Class,
    detail: provenanceLabel(provenance),
    sortText: `${provenanceRank(provenance)}-${name}`,
  };
}

function variableItem(v: ScopeVar): CompletionItem {
  const isField = v.kind === 'instanceVariable' || v.kind === 'classVariable';
  return {
    label: v.name,
    kind: isField ? CompletionItemKind.Field : CompletionItemKind.Variable,
    detail: v.kind,
    sortText: `0-${v.name}`, // in-scope variables rank above classes
  };
}

/** Completion items at byte `offset`. `selectors`/`classes` are the merged
 *  workspace + active-kernel candidates (each already provenance-tagged). */
export function completionsAt(
  offset: number,
  text: string,
  tokens: Token[],
  ast: ProgramNode,
  symbols: SymbolNode[],
  selectors: SelectorCandidate[],
  classes: ClassCandidate[],
): CompletionItem[] {
  const { prefix, wordStart } = prefixBefore(text, offset);
  const context = contextAt(prevSignificantToken(tokens, wordStart));
  const items: CompletionItem[] = [];

  if (context === 'selector') {
    for (const [selector, provenance] of bestByName(
      selectors.filter((c) => isOfferableSelector(c.selector)),
      (c) => c.selector,
    )) {
      if (matches(selector, prefix)) {
        items.push(selectorItem(selector, provenance));
      }
    }
    return items;
  }

  // head: in-scope variables first, then class names.
  for (const v of scopeVariablesAt(ast, symbols, offset)) {
    if (matches(v.name, prefix)) {
      items.push(variableItem(v));
    }
  }
  for (const [name, provenance] of bestByName(classes, (c) => c.name)) {
    if (matches(name, prefix)) {
      items.push(classItem(name, provenance));
    }
  }
  return items;
}
