// Unit tests for the quick-fix code-action provider (US-414, Slice C / AC4).
// Pure — drives the real parser → diagnostics → code actions, no VS Code.
import assert from 'node:assert/strict';
import { CodeActionKind, DiagnosticSeverity, type Diagnostic } from 'vscode-languageserver-types';
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
const editsOf = (action: { edit?: { changes?: Record<string, { newText: string }[]> } }) =>
  action.edit?.changes?.[URI] ?? [];

test('offers a quick fix to insert a missing ]', () => {
  const diags = diagnosticsFor('Object subclass: Foo [\n  bar [ ^1 \n');
  const actions = toCodeActions(URI, diags);
  assert.ok(actions.length >= 1, 'expected at least one quick fix');
  const fix = actions.find((a) => a.title.includes(']'));
  assert.ok(fix, 'expected an "insert missing ]" action');
  assert.equal(fix.kind, CodeActionKind.QuickFix);
  assert.equal(fix.isPreferred, true);
  assert.equal(editsOf(fix)[0]?.newText, ']');
  // The edit attaches to the diagnostic it fixes.
  assert.equal(fix.diagnostics?.length, 1);
});

test('offers a quick fix to insert a missing )', () => {
  const diags = diagnosticsFor('| x |\nx := (1 + 2.\n');
  const actions = toCodeActions(URI, diags);
  const fix = actions.find((a) => a.title.includes(')'));
  assert.ok(fix, 'expected an "insert missing )" action');
  assert.equal(editsOf(fix)[0]?.newText, ')');
});

test('the insert is a zero-width edit at the diagnostic range end', () => {
  const diags = diagnosticsFor('| x |\nx := (1 + 2.\n');
  const fix = toCodeActions(URI, diags).find((a) => a.title.includes(')'))!;
  const target = diags.find((d) => /Expected "\)"/.test(d.message))!;
  const edit = editsOf(fix)[0]!;
  assert.deepEqual(edit.range.start, target.range.end);
  assert.deepEqual(edit.range.end, target.range.end);
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
    message: 'Unexpected EOF ""',
    source: 'smalltalk',
    code: 'parse',
  };
  assert.deepEqual(toCodeActions(URI, [foreign, unrelated]), []);
});

console.log(`code actions: ${passed} checks passed.`);
