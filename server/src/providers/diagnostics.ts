// Diagnostics provider mapping (US-414, Slice A / AC1).
//
// Converts the parser front end's LSP-free `LexDiagnostic` list (from
// `parse()`/`tokenize()`) into `vscode-languageserver` `Diagnostic`s for
// `textDocument/publishDiagnostics`. Pure (vscode-languageserver-types only) so
// it is unit- and eval-testable without a server runtime or VS Code.
//
// The parser tier is always on and needs no `gst` (ADR-0001). Severity is mapped
// faithfully from what the lexer/parser emit — they already distinguish Error
// from Warning deliberately. VS Code renders a diagnostic badge as
// `source(code)`, so `source: 'smalltalk'` + `code: 'parse'` displays as
// `smalltalk(parse)`.

import { DiagnosticSeverity as LspSeverity, type Diagnostic } from 'vscode-languageserver-types';
import { DiagnosticSeverity, type LexDiagnostic } from '../parser/token';

/** Diagnostic `source` for the language server's own (non-`gst`) diagnostics. */
export const DIAGNOSTIC_SOURCE = 'smalltalk';
/** Diagnostic `code` for the always-on parser tier (badge: `smalltalk(parse)`). */
export const PARSE_DIAGNOSTIC_CODE = 'parse';

function toLspSeverity(severity: DiagnosticSeverity): LspSeverity {
  return severity === DiagnosticSeverity.Warning ? LspSeverity.Warning : LspSeverity.Error;
}

/** Map the parser's diagnostics to LSP `Diagnostic`s for the parser tier. */
export function toDiagnostics(diags: readonly LexDiagnostic[]): Diagnostic[] {
  return diags.map((d) => ({
    range: { start: d.startPos, end: d.endPos },
    severity: toLspSeverity(d.severity),
    message: d.message,
    source: DIAGNOSTIC_SOURCE,
    code: PARSE_DIAGNOSTIC_CODE,
  }));
}
