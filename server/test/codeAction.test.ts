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
const actionsFor = (src: string): CodeAction[] => toCodeActions(URI, diagnosticsFor(src), src);

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

/** A closer/string quick fix should re-parse clean after it is applied. */
function assertFixClears(src: string, contains: string): void {
  const fix = actionsFor(src).find((a) => a.title.includes(contains));
  assert.ok(fix, `expected a quick fix containing ${contains}`);
  assert.equal(parse(applyFix(src, fix)).diagnostics.length, 0, `applying ${contains} must yield valid code`);
}

test('inserts both missing ] (nested unclosed brackets) in one action', () => {
  const src = 'Object subclass: Foo [\n  bar [ ^1 \n';
  const diags = diagnosticsFor(src);
  assert.ok(diags.length >= 2, 'fixture should yield multiple Expected-] diagnostics');
  const actions = toCodeActions(URI, diags, src);
  assert.equal(actions.length, 1, 'same-spot closers collapse into one action');
  const fix = actions[0]!;
  assert.equal(fix.kind, CodeActionKind.QuickFix);
  assert.equal(fix.isPreferred, true);
  assert.equal(fix.edit!.changes![URI]![0]!.newText, ']]', 'inserts both needed closers');
  assert.equal(fix.diagnostics?.length, 2, 'the action references both diagnostics it fixes');
  assert.equal(parse(applyFix(src, fix)).diagnostics.length, 0);
});

test('the ) fix inserts before the offending token and clears the parse', () => {
  const src = 'Object subclass: Calc [\n  compute [ | x | x := (1 + 2 * 3.\n    ^x ]\n]';
  const diags = diagnosticsFor(src);
  const fix = toCodeActions(URI, diags, src).find((a) => a.title.includes(')'))!;
  assert.ok(fix, 'expected an "insert missing )" action');
  const edit = fix.edit!.changes![URI]![0]!;
  assert.equal(edit.newText, ')');
  const target = diags.find((d) => /Expected "\)"/.test(d.message))!;
  assert.deepEqual(edit.range.start, target.range.start, 'inserted before the unexpected token');
  assert.equal(parse(applyFix(src, fix)).diagnostics.length, 0);
});

test('inserts a missing } (brace/dynamic array)', () => {
  assertFixClears('x := { 1. 2. 3', '}');
});

test('the } fix lands on the array line, not the line below (regression)', () => {
  // The array closer scan hits EOF on the next line; the squiggle/fix must stay
  // on the `{ … }` content line (manual-QA finding).
  const src = 'x := { 1. 2. 3\n';
  const fix = actionsFor(src).find((a) => a.title.includes('}'))!;
  assert.ok(fix, 'expected an insert-} action');
  assert.equal(fix.edit!.changes![URI]![0]!.range.start.line, 0, 'fix on the array line, not the empty line below');
  assert.equal(parse(applyFix(src, fix)).diagnostics.length, 0);
});

test('inserts a missing > (attribute)', () => {
  assertFixClears('Object subclass: Foo [\n  m [ <category: 1\n  ^1 ]\n]', '>');
});

test('the > fix lands on the pragma line, not the swallowed body (regression)', () => {
  // An unclosed pragma must not swallow the `^` body; the squiggle/fix stays on
  // the pragma line, not the `^0`/`]` lines below (manual-QA finding).
  const src = "Object subclass: S [\n  m [ <category: 'x'\n  ^0 ]\n]";
  const fix = actionsFor(src).find((a) => a.title.includes('>'))!;
  assert.ok(fix, 'expected an insert-> action');
  assert.equal(fix.edit!.changes![URI]![0]!.range.start.line, 1, 'fix on the pragma line, not the body below');
  assert.equal(parse(applyFix(src, fix)).diagnostics.length, 0);
});

test('inserts the closing quote for an unterminated string (at end of the open line)', () => {
  // Single line: the quote closes the string.
  assertFixClears("Transcript showCr: 'oops", "'");
  // Multi-line swallow: closing at end of the opening line preserves the following code.
  const src = "Transcript showCr: 'oops.\nx := 1";
  const fix = actionsFor(src).find((a) => a.title.includes("'"))!;
  assert.ok(fix, 'expected a close-string quick fix');
  const out = applyFix(src, fix);
  assert.ok(out.includes('x := 1'), 'the code the string ran over is preserved');
});

test('clean source offers no code actions', () => {
  assert.deepEqual(actionsFor('| x |\nx := 1.\nTranscript showCr: x printString.'), []);
});

test('a non-fixable parse error (truncated expression) offers no quick fix', () => {
  // `x := 1 +` -> "Unexpected EOF" : a real squiggle, but nothing to insert.
  const diags = diagnosticsFor('x := 1 +\n');
  assert.ok(diags.length >= 1, 'expected a diagnostic');
  assert.deepEqual(toCodeActions(URI, diags, 'x := 1 +\n'), []);
});

test('ignores non-parser diagnostics (gst tier, etc.)', () => {
  const foreign: Diagnostic = {
    range: { start: { line: 0, character: 0 }, end: { line: 0, character: 1 } },
    severity: DiagnosticSeverity.Error,
    message: 'Expected "]"',
    source: 'gst', // not the parser tier
    code: 'compile',
  };
  assert.deepEqual(toCodeActions(URI, [foreign], 'x'), []);
});

console.log(`code actions: ${passed} checks passed.`);
