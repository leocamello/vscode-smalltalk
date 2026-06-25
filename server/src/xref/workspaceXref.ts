// Workspace cross-reference index (US-423, Slice A).
//
// The cartridge ships the kernel half of the cross-reference graph (the
// precomputed `crossReference` tier: senders + implementors). The missing half
// is the *live* workspace: every message **send site** in the user's `.st`/`.gst`
// source — the answer to "Senders of #sel" for code that isn't in a cartridge.
//
// This index walks the US-411 AST and records, per selector, where it is SENT
// (with the enclosing method context + a cheap static receiver hint that powers
// honest ranking, never proof). Workspace *implementors* are NOT duplicated here
// — they already live in `WorkspaceIndex` as method symbols; the resolve layer
// (Slice B) reads them from there. Pure: parser + lexer only, no `vscode`.
//
// Kept fresh incrementally: `setFile`/`removeFile` patch only one document's
// slice of the global `selector -> sites` map, so a query is an O(1) lookup with
// no parsing/I-O (spec §5.2, AC5).

import { NodeKind, type MethodDefinitionNode, type MessageNode, type Node, type ProgramNode } from '../parser/ast';
import { parse } from '../parser/parser';
import { tokenize } from '../parser/lexer';
import { TokenKind, type Position, type Token } from '../parser/token';
import { childNodes } from '../parser/walk';
import type { MethodSide } from '../types/knowledge-base';

export interface XrefRange {
  readonly start: Position;
  readonly end: Position;
}

/** A best-effort static classification of a send's receiver. Never proof — it
 *  ranks *likely* responders above *possible* ones (AC6). `self`/`super` are
 *  closed-world hints; a capitalized name is the *literal* receiver text (a
 *  candidate class, resolved against the known-class set at merge time); `null`
 *  is an unknown/dynamic receiver (a local, an expression, a message result). */
export type ReceiverHint = 'self' | 'super' | string | null;

/** One message send found in workspace source — the answer to "Senders of". */
export interface WorkspaceSendSite {
  readonly uri: string;
  readonly selector: string;
  /** Range of the selector text (the jump/peek target). */
  readonly range: XrefRange;
  /** The enclosing method's class, when the send is inside a method definition. */
  readonly inClass?: string;
  readonly side?: MethodSide;
  /** The enclosing method's selector (for call-hierarchy outgoing grouping). */
  readonly inSelector?: string;
  readonly receiverHint: ReceiverHint;
}

interface WalkContext {
  readonly className?: string;
  readonly side: MethodSide;
  /** `true` inside a `Foo class [ … ]` scope, so short-form methods are class-side. */
  readonly classScope: boolean;
  readonly inSelector?: string;
}

/** The first keyword part of a selector: `at:put:` -> `at:`; unary/binary -> itself. */
function firstKeywordPart(selector: string): string {
  const colon = selector.indexOf(':');
  return colon < 0 ? selector : selector.slice(0, colon + 1);
}

/** Index of the first token with `start >= offset` (binary search; tokens are
 *  emitted in source order). Returns `tokens.length` when none qualify. */
function firstTokenAtOrAfter(tokens: readonly Token[], offset: number): number {
  let lo = 0;
  let hi = tokens.length;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    if ((tokens[mid] as Token).start < offset) {
      lo = mid + 1;
    } else {
      hi = mid;
    }
  }
  return lo;
}

/** The token bearing a send's selector: the first keyword/binary/unary token of
 *  the right shape at or after `anchor` and within `[anchor, end)`, matched by
 *  text so an interleaving `class`/`>>`/argument never steals the range. */
function selectorToken(
  tokens: readonly Token[],
  selector: string,
  messageType: 'unary' | 'binary' | 'keyword',
  anchor: number,
  end: number,
): Token | undefined {
  const wantKind =
    messageType === 'keyword' ? TokenKind.Keyword : messageType === 'binary' ? TokenKind.BinarySelector : TokenKind.Identifier;
  const wantText = messageType === 'keyword' ? firstKeywordPart(selector) : selector;
  for (let i = firstTokenAtOrAfter(tokens, anchor); i < tokens.length; i++) {
    const tok = tokens[i];
    if (tok === undefined || tok.start >= end) {
      break;
    }
    if (tok.kind === wantKind && tok.text === wantText) {
      return tok;
    }
  }
  return undefined;
}

function rangeOfToken(tok: Token): XrefRange {
  return { start: tok.startPos, end: tok.endPos };
}

/** A cheap static receiver hint (AC6) — `self`/`super`, or a capitalized literal
 *  receiver name (a candidate class), else `null` for a dynamic receiver. */
function receiverHintOf(receiver: Node): ReceiverHint {
  if (receiver.kind === NodeKind.Variable) {
    if (receiver.name === 'self') return 'self';
    if (receiver.name === 'super') return 'super';
    if (/^[A-Z]/.test(receiver.name)) return receiver.name;
  }
  return null;
}

/** The class a method belongs to: its explicit target (`Foo >> sel`, `Foo class
 *  >> sel`), else the enclosing definition's class. */
function methodClassName(node: MethodDefinitionNode, ctx: WalkContext): string | undefined {
  if (node.target && node.target.kind === NodeKind.Variable) {
    return node.target.name;
  }
  return ctx.className;
}

function recordSend(out: WorkspaceSendSite[], uri: string, node: MessageNode, tokens: readonly Token[], ctx: WalkContext): void {
  if (node.selector === '') {
    return;
  }
  const tok = selectorToken(tokens, node.selector, node.messageType, node.receiver.end, node.end);
  const range: XrefRange = tok ? rangeOfToken(tok) : { start: node.startPos, end: node.endPos };
  out.push({
    uri,
    selector: node.selector,
    range,
    ...(ctx.className !== undefined ? { inClass: ctx.className } : {}),
    ...(ctx.className !== undefined ? { side: ctx.side } : {}),
    ...(ctx.inSelector !== undefined ? { inSelector: ctx.inSelector } : {}),
    receiverHint: receiverHintOf(node.receiver),
  });
}

/** Collect every send site in `ast`, tracking enclosing-method context. */
function collectSends(uri: string, ast: ProgramNode, tokens: readonly Token[]): WorkspaceSendSite[] {
  const out: WorkspaceSendSite[] = [];
  const walk = (node: Node, ctx: WalkContext): void => {
    switch (node.kind) {
      case NodeKind.Definition: {
        const childCtx: WalkContext = {
          className: node.name ?? ctx.className,
          side: 'instance',
          classScope: node.definitionKind === 'classScope',
        };
        for (const item of node.body) {
          walk(item, childCtx);
        }
        return;
      }
      case NodeKind.MethodDefinition: {
        const className = methodClassName(node, ctx);
        const side: MethodSide = node.classSide || ctx.classScope ? 'class' : 'instance';
        const methodCtx: WalkContext = { className, side, classScope: false, inSelector: node.selector };
        for (const stmt of node.statements) {
          walk(stmt, methodCtx);
        }
        return;
      }
      case NodeKind.Message: {
        recordSend(out, uri, node, tokens, ctx);
        walk(node.receiver, ctx);
        for (const arg of node.arguments) {
          walk(arg, ctx);
        }
        return;
      }
      default:
        for (const child of childNodes(node)) {
          walk(child, ctx);
        }
    }
  };
  walk(ast, { side: 'instance', classScope: false });
  return out;
}

/** A live, incrementally-maintained `selector -> send sites` index over the
 *  workspace. Mirrors `WorkspaceIndex`: per-URI slices for cheap patching, plus a
 *  global selector map for O(1) queries. */
export class WorkspaceXref {
  private readonly byUri = new Map<string, WorkspaceSendSite[]>();
  private readonly senders = new Map<string, WorkspaceSendSite[]>();

  /** (Re)index one document's send sites from its text. */
  setFile(uri: string, text: string): void {
    this.removeFile(uri);
    const ast = parse(text).ast;
    const tokens = tokenize(text).tokens;
    const sites = collectSends(uri, ast, tokens);
    this.byUri.set(uri, sites);
    for (const site of sites) {
      const bucket = this.senders.get(site.selector);
      if (bucket) {
        bucket.push(site);
      } else {
        this.senders.set(site.selector, [site]);
      }
    }
  }

  /** Drop a document's send sites from both the per-URI slice and the global map. */
  removeFile(uri: string): void {
    const old = this.byUri.get(uri);
    if (!old) {
      return;
    }
    this.byUri.delete(uri);
    // Rebuild only the affected selector buckets (the ones this file touched).
    const touched = new Set(old.map((s) => s.selector));
    for (const selector of touched) {
      const remaining = (this.senders.get(selector) ?? []).filter((s) => s.uri !== uri);
      if (remaining.length > 0) {
        this.senders.set(selector, remaining);
      } else {
        this.senders.delete(selector);
      }
    }
  }

  clear(): void {
    this.byUri.clear();
    this.senders.clear();
  }

  /** All workspace send sites of `selector` (O(1) lookup; no copy). */
  sendersOf(selector: string): readonly WorkspaceSendSite[] {
    return this.senders.get(selector) ?? [];
  }

  /** Send sites inside one method (call-hierarchy outgoing calls, Slice D). */
  sendsFrom(uri: string, className: string | undefined, side: MethodSide, inSelector: string): readonly WorkspaceSendSite[] {
    return (this.byUri.get(uri) ?? []).filter(
      (s) => s.inSelector === inSelector && s.side === side && s.inClass === className,
    );
  }

  get size(): number {
    return this.byUri.size;
  }
}
