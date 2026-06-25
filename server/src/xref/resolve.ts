// Two-tier cross-reference merge engine (US-423, Slice B).
//
// Pure: takes already-read facts from the two tiers — the live workspace
// (send sites from `workspaceXref`, method defs from `workspaceIndex`) and the
// frozen cartridge (`crossReference`) — and produces a normalized, de-duplicated,
// honestly-ranked union (spec §5.2). No parsing, no I-O: O(1) probes upstream +
// O(k) merge here (AC5). The dynamic-dispatch posture is baked in: we RANK by a
// cheap receiver hint but never FILTER, and never collapse to a single target.

import type { ImplementorRef, MethodSide, SendSite } from '../types/knowledge-base';
import type { ReceiverHint, WorkspaceSendSite, XrefRange } from './workspaceXref';

/** Where a resolved reference came from — drives the per-row provenance badge. */
export interface RefProvenance {
  readonly kind: 'workspace' | 'cartridge';
  /** `workspace`, or the kernel source's identity label (the SAME vocabulary the
   *  status bar uses, e.g. `reference (gst 3.2.5)` / `installed (gst)`) so the
   *  panel and the status bar agree on what each source is called. */
  readonly label: string;
}

export const WORKSPACE_PROVENANCE: RefProvenance = { kind: 'workspace', label: 'workspace' };

/** Provenance for a cartridge (kernel) row — labelled with the source's identity
 *  (`cartridge.label`, the status-bar vocabulary), falling back to a plain
 *  `<dialect> <version>` when no label is supplied (tests). */
export function cartridgeProvenance(c: CartridgeId): RefProvenance {
  return { kind: 'cartridge', label: c.label ?? `${c.dialect} ${c.version}` };
}

export type RefKind = 'sender' | 'implementor';

/** One normalized cross-reference row — the common shape both tiers map onto. */
export interface ResolvedRef {
  readonly kind: RefKind;
  readonly uri: string;
  readonly range: XrefRange;
  readonly provenance: RefProvenance;
  /** Simple class name of the enclosing method (sender) / the implementor. */
  readonly className?: string;
  readonly side?: MethodSide;
  /** Enclosing method selector (senders only) — call-hierarchy outgoing grouping. */
  readonly inSelector?: string;
  /** Static receiver hint (senders only) — ranks likely above possible (AC6). */
  readonly receiverHint?: ReceiverHint;
}

/** A workspace method definition (an implementor), read from `WorkspaceIndex`. */
export interface WorkspaceMethodRef {
  readonly uri: string;
  readonly className?: string;
  readonly side: MethodSide;
  readonly range: XrefRange;
}

export interface CartridgeId {
  readonly dialect: string;
  readonly version: string;
  /** The source's status-bar identity label, e.g. `reference (gst 3.2.5)` /
   *  `installed (gst)`. Drives the per-row provenance badge (terminology parity). */
  readonly label?: string;
}

/** Everything the engine needs for one selector, read upstream (O(1) per tier). */
export interface XrefSources {
  readonly workspaceSenders: readonly WorkspaceSendSite[];
  readonly workspaceImplementors: readonly WorkspaceMethodRef[];
  readonly cartridgeSenders: readonly SendSite[];
  readonly cartridgeImplementors: readonly ImplementorRef[];
  readonly cartridge?: CartridgeId;
  /** Known-class predicate (workspace ∪ cartridge) for hint confidence ranking. */
  readonly isKnownClass?: (name: string) => boolean;
}

/** ClassId → simple name: `Smalltalk.OrderedCollection` → `OrderedCollection`. */
export function simpleName(classId: string): string {
  const dot = classId.lastIndexOf('.');
  return dot < 0 ? classId : classId.slice(dot + 1);
}

/** A read-only virtual URI for a cartridge fact (peek/jump target; Slice C renders it).
 *  `smalltalk-cartridge:/<dialect>/<version>/<ClassId>/<side>/<selector>`. */
export function cartridgeUri(c: CartridgeId, classId: string, side: MethodSide, selector: string): string {
  return `smalltalk-cartridge:/${c.dialect}/${c.version}/${encodeURIComponent(classId)}/${side}/${encodeURIComponent(selector)}`;
}

/** A single-line range at 0-based `line` (cartridge facts carry a line, not a span). */
function lineRange(line: number): XrefRange {
  return { start: { line, character: 0 }, end: { line, character: 0 } };
}

function workspaceSenderRef(s: WorkspaceSendSite): ResolvedRef {
  return {
    kind: 'sender',
    uri: s.uri,
    range: s.range,
    provenance: WORKSPACE_PROVENANCE,
    ...(s.inClass !== undefined ? { className: s.inClass } : {}),
    ...(s.side !== undefined ? { side: s.side } : {}),
    ...(s.inSelector !== undefined ? { inSelector: s.inSelector } : {}),
    receiverHint: s.receiverHint,
  };
}

function workspaceImplementorRef(m: WorkspaceMethodRef): ResolvedRef {
  return {
    kind: 'implementor',
    uri: m.uri,
    range: m.range,
    provenance: WORKSPACE_PROVENANCE,
    ...(m.className !== undefined ? { className: m.className } : {}),
    side: m.side,
  };
}

function cartridgeSenderRef(s: SendSite, c: CartridgeId): ResolvedRef {
  return {
    kind: 'sender',
    uri: cartridgeUri(c, s.inClass, s.side, s.inSelector),
    range: lineRange(s.line),
    provenance: cartridgeProvenance(c),
    className: simpleName(s.inClass),
    side: s.side,
    inSelector: s.inSelector,
    receiverHint: s.receiverHint ?? null,
  };
}

function cartridgeImplementorRef(r: ImplementorRef, selector: string, c: CartridgeId): ResolvedRef {
  return {
    kind: 'implementor',
    uri: cartridgeUri(c, r.inClass, r.side, selector),
    range: lineRange(0),
    provenance: cartridgeProvenance(c),
    className: simpleName(r.inClass),
    side: r.side,
  };
}

/** Hint confidence for ranking (AC6): closed-world / known-class likely (2) >
 *  unknown-but-capitalized possible (1) > dynamic (0). Never used to filter. */
function confidence(ref: ResolvedRef, isKnownClass?: (name: string) => boolean): number {
  const h = ref.receiverHint;
  if (h === 'self' || h === 'super') {
    return 2;
  }
  if (typeof h === 'string') {
    return isKnownClass?.(h) ? 2 : 1;
  }
  return 0;
}

/** Stable multi-key sort: workspace before cartridge, then hint confidence, then
 *  class name, then line (spec §5.2 step 6). */
function sortRefs(refs: ResolvedRef[], isKnownClass?: (name: string) => boolean): ResolvedRef[] {
  return [...refs].sort(
    (a, b) =>
      Number(b.provenance.kind === 'workspace') - Number(a.provenance.kind === 'workspace') ||
      confidence(b, isKnownClass) - confidence(a, isKnownClass) ||
      (a.className ?? '').localeCompare(b.className ?? '') ||
      a.range.start.line - b.range.start.line,
  );
}

/** Implementors of the selector: workspace ∪ cartridge, with `Workspace ≻
 *  Cartridge` shadowing the dev-box overlap (same class+side already live). */
export function resolveImplementors(sources: XrefSources, selector: string): ResolvedRef[] {
  const workspace = sources.workspaceImplementors.map(workspaceImplementorRef);
  const shadow = new Set(workspace.map((r) => `${r.className ?? ''}|${r.side ?? ''}`));
  const cartridge = sources.cartridge
    ? sources.cartridgeImplementors
        .map((r) => cartridgeImplementorRef(r, selector, sources.cartridge as CartridgeId))
        .filter((r) => !shadow.has(`${r.className ?? ''}|${r.side ?? ''}`))
    : [];
  return sortRefs([...workspace, ...cartridge]);
}

/** Senders of the selector: workspace ∪ cartridge, shadowing the dev-box overlap
 *  (same enclosing class+side+method already live), ranked by receiver hint. */
export function resolveSenders(sources: XrefSources, _selector: string): ResolvedRef[] {
  const workspace = sources.workspaceSenders.map(workspaceSenderRef);
  const shadowKey = (r: ResolvedRef): string => `${r.className ?? ''}|${r.side ?? ''}|${r.inSelector ?? ''}`;
  const shadow = new Set(workspace.map(shadowKey));
  const cartridge = sources.cartridge
    ? sources.cartridgeSenders
        .map((s) => cartridgeSenderRef(s, sources.cartridge as CartridgeId))
        .filter((r) => !shadow.has(shadowKey(r)))
    : [];
  return sortRefs([...workspace, ...cartridge], sources.isKnownClass);
}

/** The full reference union (implementors ∪ senders) for `textDocument/references`. */
export function resolveReferences(sources: XrefSources, selector: string): ResolvedRef[] {
  return [...resolveImplementors(sources, selector), ...resolveSenders(sources, selector)];
}
