// Call hierarchy over the two-tier cross-reference engine (US-423, Slice D / AC4).
//
// Reuses the SAME index as references/senders — there is no second graph:
//   - incoming calls of a method  = senders of its selector (resolve.ts),
//   - outgoing calls of a method   = the sends inside its body (workspaceXref).
// Pure: server.ts supplies the AST/tokens for `prepare`, the merged senders for
// incoming, and the method's send sites for outgoing. Honest-union posture
// carries over — incoming is the lexical union of everyone who *might* call.

import {
  SymbolKind,
  type CallHierarchyIncomingCall,
  type CallHierarchyItem,
  type CallHierarchyOutgoingCall,
  type Range,
} from 'vscode-languageserver-types';
import { NodeKind, type Node, type ProgramNode } from '../parser/ast';
import { childNodes } from '../parser/walk';
import type { Token } from '../parser/token';
import type { MethodSide } from '../types/knowledge-base';
import type { ResolvedRef } from '../xref/resolve';
import {
  messageSelectorRange,
  messageSelectorTokenOf,
  methodSelectorRange,
  methodSelectorTokenOf,
  type WorkspaceSendSite,
  type XrefRange,
} from '../xref/workspaceXref';

/** What the cursor sits on: a method definition (full identity) or a send. */
export interface CallTarget {
  readonly kind: 'method' | 'send';
  readonly selector: string;
  readonly className?: string;
  readonly side?: MethodSide;
  readonly selectionRange: XrefRange;
  readonly range: XrefRange;
}

/** Identity round-tripped on `CallHierarchyItem.data` from prepare → in/out. */
export interface CallItemData {
  readonly selector: string;
  readonly className?: string;
  readonly side?: MethodSide;
  readonly uri: string;
}

interface WalkContext {
  readonly className?: string;
  readonly side: MethodSide;
  readonly classScope: boolean;
}

function toRange(r: XrefRange): Range {
  return { start: r.start, end: r.end };
}

/** Display label for a method/send item: `Speaker>>greet`, `Foo class>>new`, or
 *  the bare selector when the enclosing class is unknown (a script send). */
function label(selector: string, className?: string, side?: MethodSide): string {
  if (className === undefined) {
    return selector;
  }
  return `${className}${side === 'class' ? ' class' : ''}>>${selector}`;
}

/** Resolve the method definition or message send under `offset` (byte offset),
 *  tracking enclosing class/side so a prepared method carries full identity. */
export function resolveCallTarget(ast: ProgramNode, tokens: readonly Token[], offset: number): CallTarget | undefined {
  let found: CallTarget | undefined;
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
        const className = node.target && node.target.kind === NodeKind.Variable ? node.target.name : ctx.className;
        const side: MethodSide = node.classSide || ctx.classScope ? 'class' : 'instance';
        const tok = methodSelectorTokenOf(node, tokens);
        if (tok && offset >= tok.start && offset <= tok.end) {
          found = {
            kind: 'method',
            selector: node.selector,
            ...(className !== undefined ? { className } : {}),
            side,
            selectionRange: methodSelectorRange(node, tokens),
            range: { start: node.startPos, end: node.endPos },
          };
        }
        for (const stmt of node.statements) {
          walk(stmt, { className, side, classScope: false });
        }
        return;
      }
      case NodeKind.Message: {
        const tok = messageSelectorTokenOf(node, tokens);
        if (tok && offset >= tok.start && offset <= tok.end && node.selector !== '') {
          found = {
            kind: 'send',
            selector: node.selector,
            selectionRange: messageSelectorRange(node, tokens),
            range: { start: node.startPos, end: node.endPos },
          };
        }
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
  return found;
}

/** `prepareCallHierarchy`: one item for the method/send under the cursor. */
export function prepareCallHierarchy(target: CallTarget | undefined, uri: string): CallHierarchyItem[] {
  if (!target) {
    return [];
  }
  const data: CallItemData = {
    selector: target.selector,
    ...(target.className !== undefined ? { className: target.className } : {}),
    ...(target.side !== undefined ? { side: target.side } : {}),
    uri,
  };
  return [
    {
      name: label(target.selector, target.className, target.side),
      kind: SymbolKind.Method,
      uri,
      range: toRange(target.range),
      selectionRange: toRange(target.selectionRange),
      data,
    },
  ];
}

/** Incoming calls = senders of the selector, grouped by the calling method. The
 *  caller item points at its send site(s) (we hold send coordinates, not the
 *  caller's own def range — honest + cheap). */
export function incomingCalls(senders: readonly ResolvedRef[]): CallHierarchyIncomingCall[] {
  const groups = new Map<string, { ref: ResolvedRef; ranges: Range[] }>();
  for (const s of senders) {
    const key = `${s.uri}|${s.className ?? ''}|${s.side ?? ''}|${s.inSelector ?? ''}`;
    const range = toRange(s.range);
    const g = groups.get(key);
    if (g) {
      g.ranges.push(range);
    } else {
      groups.set(key, { ref: s, ranges: [range] });
    }
  }
  return [...groups.values()].map(({ ref, ranges }) => ({
    from: {
      name: ref.inSelector !== undefined ? label(ref.inSelector, ref.className, ref.side) : '(top level)',
      kind: SymbolKind.Method,
      uri: ref.uri,
      range: ranges[0] as Range,
      selectionRange: ranges[0] as Range,
    },
    fromRanges: ranges,
  }));
}

/** Outgoing calls = the sends inside the method's body, grouped by callee
 *  selector. Workspace-only: a cartridge method's body is not in the workspace. */
export function outgoingCalls(sends: readonly WorkspaceSendSite[]): CallHierarchyOutgoingCall[] {
  const groups = new Map<string, { site: WorkspaceSendSite; ranges: Range[] }>();
  for (const s of sends) {
    const range = toRange(s.range);
    const g = groups.get(s.selector);
    if (g) {
      g.ranges.push(range);
    } else {
      groups.set(s.selector, { site: s, ranges: [range] });
    }
  }
  return [...groups.values()].map(({ site, ranges }) => ({
    to: {
      name: site.selector,
      kind: SymbolKind.Method,
      uri: site.uri,
      range: ranges[0] as Range,
      selectionRange: ranges[0] as Range,
    },
    fromRanges: ranges,
  }));
}
