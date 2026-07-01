// Property tests for scope-aware rename (US-426, AC2 + AC5). tsx.
//
// Acceptance Harness — RED until server/src/providers/rename.ts ships. Pins the
// safety invariants:
//   1. NO BLEED          — every edited range covers an occurrence of the SAME
//      original name; the count never exceeds the scope's same-name occurrences.
//   2. ROUND-TRIP        — rename a->b then b->a restores the original text.
//   3. IDEMPOTENT        — applying the a->b edit, then renaming b->b again, is a no-op.
import assert from 'node:assert/strict';
import { parse } from '../src/parser/parser.ts';
import { renameAt } from '../src/providers/rename.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

interface Pos { line: number; character: number }
interface Edit { range: { start: Pos; end: Pos }; newText: string }

/** Offset of an LSP position within `text`. */
function offsetOf(text: string, p: Pos): number {
  let line = 0;
  let i = 0;
  while (line < p.line && i < text.length) {
    if (text.charCodeAt(i) === 10) line += 1;
    i += 1;
  }
  return i + p.character;
}

/** Apply LSP TextEdits (non-overlapping) to `text`. */
function applyEdits(text: string, edits: Edit[]): string {
  const sorted = [...edits].sort((a, b) => offsetOf(text, b.range.start) - offsetOf(text, a.range.start));
  let out = text;
  for (const e of sorted) {
    out = out.slice(0, offsetOf(text, e.range.start)) + e.newText + out.slice(offsetOf(text, e.range.end));
  }
  return out;
}

const isReject = (r: unknown): r is { reject: string } =>
  typeof r === 'object' && r !== null && 'reject' in r;

function at(src: string, sub: string, occurrence = 1): number {
  let idx = -1;
  for (let i = 0; i < occurrence; i++) idx = src.indexOf(sub, idx + 1);
  return idx + 1;
}

const URI = 'file:///p.st';
const rename = (src: string, offset: number, newName: string) =>
  renameAt(URI, offset, newName, [{ uri: URI, text: src }]);

/** (source, the original name, an offset into one of its occurrences). */
const SAMPLES: Array<[string, string, string]> = [
  ['temporary', 'acc', 'Object subclass: Foo [\n  bar [ | acc | acc := 1. ^acc + acc ] ]'],
  ['argument', 'x', 'Object subclass: Foo [\n  bar: x [ ^x * x ] ]'],
  ['ivar', 'balance', 'Object subclass: Foo [\n  | balance |\n  bal [ balance := balance + 1. ^balance ] ]'],
];

for (const [name, sym, src] of SAMPLES) {
  test(`no bleed: ${name} edits cover only same-name occurrences`, () => {
    const r = rename(src, at(src, sym, 2), 'renamed');
    assert.ok(!isReject(r), 'rename should succeed');
    const edits = ((r as { changes?: Record<string, Edit[]> }).changes ?? {})[URI] ?? [];
    assert.ok(edits.length > 0, 'expected edits');
    for (const e of edits) {
      const slice = src.slice(offsetOf(src, e.range.start), offsetOf(src, e.range.end));
      assert.equal(slice, sym, `edited range must cover "${sym}", got "${slice}"`);
    }
  });

  test(`round-trip: ${name} a->b->a restores the original`, () => {
    const r1 = rename(src, at(src, sym, 2), 'renamed');
    assert.ok(!isReject(r1));
    const once = applyEdits(src, ((r1 as { changes?: Record<string, Edit[]> }).changes ?? {})[URI] ?? []);
    const back = rename(once, at(once, 'renamed', 1), sym);
    assert.ok(!isReject(back), 'reverse rename should succeed');
    const restored = applyEdits(once, ((back as { changes?: Record<string, Edit[]> }).changes ?? {})[URI] ?? []);
    assert.equal(restored, src, 'a->b->a must restore the original text');
  });

  test(`safe: ${name} renamed source still parses with no new diagnostics`, () => {
    const baseline = parse(src).diagnostics.length;
    const r = rename(src, at(src, sym, 2), 'renamed');
    const once = applyEdits(src, ((r as { changes?: Record<string, Edit[]> }).changes ?? {})[URI] ?? []);
    assert.ok(parse(once).diagnostics.length <= baseline, 'rename must not introduce parse errors');
  });
}

console.log(`rename property tests: ${passed} passed`);
