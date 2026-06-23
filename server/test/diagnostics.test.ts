// Unit tests for the diagnostics provider mapping (US-414, Slice A / AC1). Runs
// in Node via tsx — uses only vscode-languageserver-types + the parser front end
// (no server runtime, no VS Code). Verifies LexDiagnostic → LSP Diagnostic.
import assert from 'node:assert/strict';
import { DiagnosticSeverity as LspSeverity, type Diagnostic } from 'vscode-languageserver-types';
import { parse } from '../src/parser/parser.ts';
import { DiagnosticSeverity, type LexDiagnostic } from '../src/parser/token.ts';
import { toDiagnostics, DIAGNOSTIC_SOURCE, PARSE_DIAGNOSTIC_CODE } from '../src/providers/diagnostics.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

const diagsFor = (src: string): Diagnostic[] => toDiagnostics(parse(src).diagnostics);

test('clean source yields no diagnostics', () => {
  assert.deepEqual(diagsFor('| x |\nx := 1.\nTranscript showCr: x printString.'), []);
});

test('unterminated block reports a parse diagnostic (badge smalltalk(parse))', () => {
  const diags = diagsFor('Object subclass: Foo [\n  bar [ ^1 \n');
  assert.ok(diags.length >= 1, 'expected at least one diagnostic for the missing ]');
  for (const d of diags) {
    assert.equal(d.source, DIAGNOSTIC_SOURCE, 'source must be smalltalk');
    assert.equal(d.code, PARSE_DIAGNOSTIC_CODE, 'code must be parse (renders as smalltalk(parse))');
    assert.equal(DIAGNOSTIC_SOURCE, 'smalltalk');
    assert.equal(PARSE_DIAGNOSTIC_CODE, 'parse');
  }
});

test('range is taken from the LexDiagnostic positions', () => {
  const lex: LexDiagnostic = {
    message: 'unterminated string',
    severity: DiagnosticSeverity.Error,
    start: 5,
    end: 9,
    startPos: { line: 2, character: 3 },
    endPos: { line: 2, character: 7 },
  };
  const [d] = toDiagnostics([lex]);
  assert.deepEqual(d.range, { start: { line: 2, character: 3 }, end: { line: 2, character: 7 } });
  assert.equal(d.message, 'unterminated string');
});

test('severity maps Error and Warning faithfully (as the parser emits)', () => {
  const base = { message: 'm', start: 0, end: 1, startPos: { line: 0, character: 0 }, endPos: { line: 0, character: 1 } };
  const [err] = toDiagnostics([{ ...base, severity: DiagnosticSeverity.Error }]);
  const [warn] = toDiagnostics([{ ...base, severity: DiagnosticSeverity.Warning }]);
  assert.equal(err.severity, LspSeverity.Error);
  assert.equal(warn.severity, LspSeverity.Warning);
});

console.log(`diagnostics provider: ${passed} checks passed.`);
