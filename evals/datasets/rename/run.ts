// Scope-aware rename output eval (US-426). Deterministic golden-dataset check via
// `npm run eval`. Drives the pure rename provider (no gst, no VS Code) and asserts,
// per case in cases.json: either the applied WorkspaceEdit yields the expected text
// per file (`expect`), or the rename is rejected matching `expectRejectMatches`.
// RED until server/src/providers/rename.ts ships.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { renameAt } from '../../../server/src/providers/rename.ts';
import { buildClassWorldFromFiles } from '../../../server/src/xref/classRefs.ts';

// A fixed kernel-class floor for the class-rename cases (US-428). The eval loads
// no cartridge, so we name the kernel classes the fixtures reference.
const EVAL_KERNEL = new Set(['Object', 'OrderedCollection', 'Collection', 'Array', 'String']);

interface Pos { line: number; character: number }
interface Edit { range: { start: Pos; end: Pos }; newText: string }
interface Case {
  name: string;
  files: Record<string, string>;
  cursor: { uri: string; find: string; occurrence?: number };
  newName: string;
  expect?: Record<string, string>;
  expectRejectMatches?: string;
}

const offsetOf = (text: string, p: Pos): number => {
  let line = 0;
  let i = 0;
  while (line < p.line && i < text.length) {
    if (text.charCodeAt(i) === 10) line += 1;
    i += 1;
  }
  return i + p.character;
};

const applyEdits = (text: string, edits: Edit[]): string => {
  const sorted = [...edits].sort((a, b) => offsetOf(text, b.range.start) - offsetOf(text, a.range.start));
  let out = text;
  for (const e of sorted) {
    out = out.slice(0, offsetOf(text, e.range.start)) + e.newText + out.slice(offsetOf(text, e.range.end));
  }
  return out;
};

const findOffset = (text: string, sub: string, occurrence = 1): number => {
  let idx = -1;
  for (let i = 0; i < occurrence; i++) idx = text.indexOf(sub, idx + 1);
  return idx + 1;
};

const here = path.dirname(fileURLToPath(import.meta.url));
const { cases } = JSON.parse(fs.readFileSync(path.join(here, 'cases.json'), 'utf8')) as { cases: Case[] };

let passed = 0;
let failed = 0;
for (const c of cases) {
  try {
    const files = Object.entries(c.files).map(([uri, text]) => ({ uri, text }));
    const cursorText = c.files[c.cursor.uri]!;
    const offset = findOffset(cursorText, c.cursor.find, c.cursor.occurrence ?? 1);
    const world = buildClassWorldFromFiles(files, (n) => EVAL_KERNEL.has(n));
    const result = renameAt(c.cursor.uri, offset, c.newName, files, world);

    if (c.expectRejectMatches !== undefined) {
      assert.ok(
        typeof result === 'object' && result !== null && 'reject' in result,
        'expected a rejection',
      );
      assert.match((result as { reject: string }).reject, new RegExp(c.expectRejectMatches, 'i'));
    } else {
      assert.ok(!('reject' in (result as object)), `unexpected rejection: ${JSON.stringify(result)}`);
      const changes = (result as { changes?: Record<string, Edit[]> }).changes ?? {};
      for (const [uri, original] of Object.entries(c.files)) {
        const got = applyEdits(original, changes[uri] ?? []);
        const want = c.expect![uri] ?? original;
        assert.equal(got, want, `mismatch in ${uri}\n--- expected ---\n${want}\n--- got ---\n${got}`);
      }
    }
    passed += 1;
    console.log(`  ok - ${c.name}`);
  } catch (e) {
    failed += 1;
    console.error(`  FAIL - ${c.name}: ${(e as Error).message}`);
  }
}

console.log(`rename eval: ${passed}/${cases.length} cases passed.`);
if (failed > 0) process.exit(1);
