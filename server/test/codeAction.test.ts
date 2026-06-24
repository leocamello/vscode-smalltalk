// Unit tests for the quick-fix code-action provider (US-414, Slice C / AC4).
// Pure — drives the real parser → diagnostics → code actions, no VS Code.
import assert from 'node:assert/strict';
import { CodeActionKind, DiagnosticSeverity, type CodeAction, type Diagnostic } from 'vscode-languageserver-types';
import { parse } from '../src/parser/parser.ts';
import { toDiagnostics } from '../src/providers/diagnostics.ts';
import { toCodeActions } from '../src/providers/codeAction.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

const URI = 'file:///x.st';
const diagnosticsFor = (src: string): Diagnostic[] => toDiagnostics(parse(src).diagnostics);

function offsetOf(text: string, line: number, character: number): number {
  const lines = text.split('\n');
  let offset = 0;
  for (let i = 0; i < line; i++) {
    offset += lines[i]!.length + 1;
  }
  return offset + character;
}

/** Apply a single-edit quick fix to `src` and return the resulting text. */
function applyFix(src: string, fix: CodeAction): string {
  const edit = fix.edit!.changes![URI]![0]!;
  const at = offsetOf(src, edit.range.start.line, edit.range.start.character);
  return src.slice(0, at) + edit.newText + src.slice(at);
}

test('offers one quick fix that inserts both missing ] (nested unclosed brackets)', () => {
  const src = 'Object subclass: Foo [\n  bar [ ^1 \n';
  const diags = diagnosticsFor(src);
  assert.ok(diags.length >= 2, 'fixture should yield multiple Expected-] diagnostics');
  const actions = toCodeActions(URI, diags);
  assert.equal(actions.length, 1, 'same-spot closers collapse into one action');
  const fix = actions[0]!;
  assert.equal(fix.kind, CodeActionKind.QuickFix);
  assert.equal(fix.isPreferred, true);
  assert.equal(fix.edit!.changes![URI]![0]!.newText, ']]', 'inserts both needed closers');
  assert.equal(fix.diagnostics?.length, 2, 'the action references both diagnostics it fixes');
});

test('the ] fix, when applied, actually clears the parse', () => {
  const src = 'Object subclass: Foo [\n  bar [ ^1 \n';
  const fix = toCodeActions(URI, diagnosticsFor(src))[0]!;
  assert.equal(parse(applyFix(src, fix)).diagnostics.length, 0, 'applying the fix must yield valid code');
});

test('the ) fix inserts before the offending token and clears the parse', () => {
  // `(1 + 2.` — the `.` is where `)` was expected; the closer must go BEFORE it.
  const src = 'Object subclass: Calc [\n  compute [ | x | x := (1 + 2 * 3.\n    ^x ]\n]';
  const diags = diagnosticsFor(src);
  const fix = toCodeActions(URI, diags).find((a) => a.title.includes(')'))!;
  assert.ok(fix, 'expected an "insert missing )" action');
  const edit = fix.edit!.changes![URI]![0]!;
  assert.equal(edit.newText, ')');
  // Inserted at the diagnostic's range START (before the unexpected token), and it fixes the parse.
  const target = diags.find((d) => /Expected "\)"/.test(d.message))!;
  assert.deepEqual(edit.range.start, target.range.start);
  assert.equal(parse(applyFix(src, fix)).diagnostics.length, 0, 'applying the fix must yield valid code');
});

test('clean source offers no code actions', () => {
  const diags = diagnosticsFor('| x |\nx := 1.\nTranscript showCr: x printString.');
  assert.deepEqual(toCodeActions(URI, diags), []);
});

test('ignores non-parser diagnostics and unrelated parser messages', () => {
  const foreign: Diagnostic = {
    range: { start: { line: 0, character: 0 }, end: { line: 0, character: 1 } },
    severity: DiagnosticSeverity.Error,
    message: 'Expected "]"',
    source: 'gst', // not the parser tier
    code: 'compile',
  };
  const unrelated: Diagnostic = {
    range: { start: { line: 0, character: 0 }, end: { line: 0, character: 1 } },
    severity: DiagnosticSeverity.Error,
    message: 'Unterminated string literal',
    source: 'smalltalk',
    code: 'parse',
  };
  assert.deepEqual(toCodeActions(URI, [foreign, unrelated]), []);
});

console.log(`code actions: ${passed} checks passed.`);
