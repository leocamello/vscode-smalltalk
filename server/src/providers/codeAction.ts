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
  type WorkspaceEdit,
} from 'vscode-languageserver-types';
import { DIAGNOSTIC_SOURCE, PARSE_DIAGNOSTIC_CODE } from './diagnostics';

// The parser phrases a missing closer as e.g. `Expected "]" to close method body`
// or `Expected ")"`. Capture the delimiter; only `]`/`)` are in scope for v1.
const MISSING_CLOSER = /Expected "(\]|\))"/;

function isParserDiagnostic(d: Diagnostic): boolean {
  return d.source === DIAGNOSTIC_SOURCE && d.code === PARSE_DIAGNOSTIC_CODE;
}

/**
 * Quick fixes for the diagnostics in a code-action request `context`. Inserts the
 * missing delimiter at the position the parser expected it (the diagnostic range
 * end). Returns one `QuickFix` per fixable diagnostic; empty when none apply.
 */
export function toCodeActions(uri: string, diagnostics: readonly Diagnostic[]): CodeAction[] {
  const actions: CodeAction[] = [];
  for (const diag of diagnostics) {
    if (!isParserDiagnostic(diag)) {
      continue;
    }
    const match = MISSING_CLOSER.exec(diag.message);
    if (!match) {
      continue;
    }
    const closer = match[1]!;
    const edit: WorkspaceEdit = {
      changes: { [uri]: [{ range: { start: diag.range.end, end: diag.range.end }, newText: closer }] },
    };
    actions.push({
      title: `Insert missing "${closer}"`,
      kind: CodeActionKind.QuickFix,
      diagnostics: [diag],
      edit,
      isPreferred: true,
    });
  }
  return actions;
}
