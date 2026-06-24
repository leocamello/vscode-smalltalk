// Trivial quick-fix code actions (US-414, Slice C / AC4).
//
// Where a fix is cheap and unambiguous, offer it. v1 scope: insert a missing
// `]` or `)` when the parser reported the corresponding "Expected …" diagnostic.
// Pure (vscode-languageserver-types only) and derived straight from the
// already-published parser diagnostics — no re-parse, no guessing.

import {
  CodeAction,
  CodeActionKind,
  type Diagnostic,
  type Position,
  type WorkspaceEdit,
} from 'vscode-languageserver-types';
import { DIAGNOSTIC_SOURCE, PARSE_DIAGNOSTIC_CODE } from './diagnostics';

// The parser phrases a missing closer as e.g. `Expected "]" to close method body`
// or `Expected ")"`. Capture the delimiter; only `]`/`)` are in scope for v1.
const MISSING_CLOSER = /Expected "(\]|\))"/;

function isParserDiagnostic(d: Diagnostic): boolean {
  return d.source === DIAGNOSTIC_SOURCE && d.code === PARSE_DIAGNOSTIC_CODE;
}

interface CloserGroup {
  readonly closer: string;
  readonly position: Position;
  readonly diagnostics: Diagnostic[];
}

/**
 * Quick fixes for the diagnostics in a code-action request `context`. The closer
 * is inserted at the diagnostic range **start** — the position the parser was at
 * when it expected the delimiter, i.e. *before* the unexpected token (inserting
 * at the range end would land the `)` after the offending token and not fix the
 * parse). The error-tolerant parser can report several missing closers at one
 * spot (nested unclosed `[`/`(`); those are grouped into a single action that
 * inserts all of them, so one click closes them all. Returns one `QuickFix` per
 * (position, closer) group; empty when none apply.
 */
export function toCodeActions(uri: string, diagnostics: readonly Diagnostic[]): CodeAction[] {
  const groups = new Map<string, CloserGroup>();
  for (const diag of diagnostics) {
    if (!isParserDiagnostic(diag)) {
      continue;
    }
    const match = MISSING_CLOSER.exec(diag.message);
    if (!match) {
      continue;
    }
    const closer = match[1]!;
    const position = diag.range.start;
    const key = `${closer}@${position.line}:${position.character}`;
    const group = groups.get(key);
    if (group) {
      group.diagnostics.push(diag);
    } else {
      groups.set(key, { closer, position, diagnostics: [diag] });
    }
  }

  const actions: CodeAction[] = [];
  for (const { closer, position, diagnostics: group } of groups.values()) {
    const insert = closer.repeat(group.length);
    const edit: WorkspaceEdit = {
      changes: { [uri]: [{ range: { start: position, end: position }, newText: insert }] },
    };
    actions.push({
      title: `Insert missing "${insert}"`,
      kind: CodeActionKind.QuickFix,
      diagnostics: group,
      edit,
      isPreferred: true,
    });
  }
  return actions;
}
