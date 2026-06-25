// Branded "Senders of…" / "Implementors of…" command results (US-423, Slice C / AC2).
//
// Pure: maps the merged `ResolvedRef[]` (xref/resolve.ts) into a flat,
// client-renderable result — a header carrying the honest union/uncertainty
// contract + per-row provenance and (for senders) a receiver-hint badge. The
// client renders this as a tree; cartridge rows carry synthetic
// `smalltalk-cartridge:` URIs that a read-only virtual document resolves.
//
// The disclaimer is load-bearing, not decoration: Smalltalk dispatches
// dynamically, so we present a LEXICAL UNION and say so. Likely responders are
// ranked first (the engine already sorted them), but none are ever hidden.

import type { ResolvedRef } from '../xref/resolve';

export type XrefDirection = 'senders' | 'implementors';

export interface XrefResultRange {
  readonly start: { readonly line: number; readonly character: number };
  readonly end: { readonly line: number; readonly character: number };
}

/** One renderable row — a navigable site with a provenance/hint badge. */
export interface CrossReferenceRow {
  readonly uri: string;
  readonly range: XrefResultRange;
  /** Primary label: the calling method (sender) or the implementing class. */
  readonly label: string;
  /** Secondary badge text: provenance, plus a receiver hint for senders. */
  readonly detail: string;
  /** `workspace` or `cartridge:<dialect>@<version>` (per-row provenance, AC2). */
  readonly provenance: string;
  readonly kind: 'sender' | 'implementor';
}

export interface CrossReferenceResult {
  readonly direction: XrefDirection;
  readonly selector: string;
  /** Header title, e.g. `Senders of #do:`. */
  readonly title: string;
  /** The union/uncertainty contract (AC2) — shown on the header node. */
  readonly disclaimer: string;
  readonly count: number;
  readonly rows: CrossReferenceRow[];
}

/** `Class` / `Class class` for a class-side member; bare selector for a script. */
function qualified(className: string | undefined, side: 'instance' | 'class' | undefined): string | undefined {
  if (className === undefined) {
    return undefined;
  }
  return side === 'class' ? `${className} class` : className;
}

function rowLabel(ref: ResolvedRef): string {
  if (ref.kind === 'sender') {
    const cls = qualified(ref.className, ref.side);
    if (ref.inSelector === undefined) {
      return '(top level)';
    }
    return cls !== undefined ? `${cls} » ${ref.inSelector}` : ref.inSelector;
  }
  return qualified(ref.className, ref.side) ?? '(unknown)';
}

function rowDetail(ref: ResolvedRef): string {
  const parts = [ref.provenance.label];
  if (ref.kind === 'sender' && ref.receiverHint != null) {
    parts.push(`receiver: ${ref.receiverHint}`);
  }
  return parts.join(' · ');
}

const SENDERS_DISCLAIMER =
  'Lexical union of every send site across your workspace and the bundled cartridge. ' +
  'Smalltalk dispatches dynamically, so this cannot be narrowed to one runtime target — ' +
  'likely senders are ranked first, but none are hidden.';

const IMPLEMENTORS_DISCLAIMER =
  'Every class whose method table defines this selector (workspace ∪ cartridge). ' +
  "Dynamic dispatch chooses one at runtime by the receiver's class; all candidates are " +
  'listed, likely ones first.';

/** Package the merged refs (already deduped + ranked) for the command tree. */
export function buildCrossReference(
  direction: XrefDirection,
  selector: string,
  refs: readonly ResolvedRef[],
): CrossReferenceResult {
  const rows: CrossReferenceRow[] = refs.map((ref) => ({
    uri: ref.uri,
    range: { start: ref.range.start, end: ref.range.end },
    label: rowLabel(ref),
    detail: rowDetail(ref),
    provenance: ref.provenance.label,
    kind: ref.kind,
  }));
  return {
    direction,
    selector,
    title: `${direction === 'senders' ? 'Senders' : 'Implementors'} of #${selector}`,
    disclaimer: direction === 'senders' ? SENDERS_DISCLAIMER : IMPLEMENTORS_DISCLAIMER,
    count: rows.length,
    rows,
  };
}
