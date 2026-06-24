// Trivial quick-fix code actions (US-414, Slice C / AC4).
//
// Where a fix is cheap and unambiguous, offer it. Scope: insert a missing closer
// — `]`, `)`, `}`, or `>` — when the parser reported the corresponding
// `Expected "X"` diagnostic, and insert the closing `'` for an unterminated
// string literal. Pure (vscode-languageserver-types only) and derived straight
// from the already-published parser diagnostics — no re-parse, no guessing.

import {
  CodeAction,
  CodeActionKind,
  type Diagnostic,
  type Position,
  type WorkspaceEdit,
} from 'vscode-languageserver-types';
import { DIAGNOSTIC_SOURCE, PARSE_DIAGNOSTIC_CODE } from './diagnostics';

// A missing closer is phrased `Expected "X" …` with X the *expected* token at the
// start of the message — e.g. `Expected "]" to close method body`, `Expected ")"`,
// `Expected "}" to close binding constant`, `Expected ">" to close attribute`.
// Anchored so we never match an opener (`Expected "[" …`), a separator
// (`Expected "." …`), or the ambiguous `Expected "." or "}" in dynamic array`.
const MISSING_CLOSER = /^Expected "([\])}>])"/;

const UNTERMINATED_STRING = 'Unterminated string literal';

function isParserDiagnostic(d: Diagnostic): boolean {
  return d.source === DIAGNOSTIC_SOURCE && d.code === PARSE_DIAGNOSTIC_CODE;
}

interface InsertGroup {
  readonly insert: string;
  readonly position: Position;
  readonly diagnostics: Diagnostic[];
}

/**
 * Quick fixes for the diagnostics in a code-action request. Two families:
 *
 * - **Missing closer** (`]`/`)`/`}`/`>`): insert at the diagnostic range **start**
 *   — the position the parser was at when it expected the delimiter, i.e. *before*
 *   the unexpected token (inserting at the range end lands the closer after that
 *   token and does not fix the parse). Several closers reported at one spot
 *   (nested unclosed `[`/`(`) are grouped into a single action that inserts all of
 *   them (`]]`), so one click closes them.
 * - **Unterminated string**: insert the closing `'` at the **end of the line where
 *   the string opened** (needs `text`) — closes the common single-line typo and,
 *   for a multi-line swallow, preserves the code the string ran over.
 *
 * Returns one `QuickFix` per (position, insert) group; empty when none apply.
 */
export function toCodeActions(uri: string, diagnostics: readonly Diagnostic[], text = ''): CodeAction[] {
  const lines = text.split('\n');
  const groups = new Map<string, InsertGroup>();

  const add = (insert: string, position: Position, diag: Diagnostic): void => {
    const key = `${insert}@${position.line}:${position.character}`;
    const group = groups.get(key);
    if (group) {
      group.diagnostics.push(diag);
    } else {
      groups.set(key, { insert, position, diagnostics: [diag] });
    }
  };

  for (const diag of diagnostics) {
    if (!isParserDiagnostic(diag)) {
      continue;
    }
    const closer = MISSING_CLOSER.exec(diag.message);
    if (closer) {
      add(closer[1]!, diag.range.start, diag);
    } else if (diag.message === UNTERMINATED_STRING) {
      const line = diag.range.start.line;
      add("'", { line, character: (lines[line] ?? '').length }, diag);
    }
  }

  const actions: CodeAction[] = [];
  for (const { insert, position, diagnostics: group } of groups.values()) {
    const newText = insert.repeat(group.length);
    const edit: WorkspaceEdit = {
      changes: { [uri]: [{ range: { start: position, end: position }, newText }] },
    };
    actions.push({
      title: `Insert missing "${newText}"`,
      kind: CodeActionKind.QuickFix,
      diagnostics: group,
      edit,
      isPreferred: true,
    });
  }
  return actions;
}
