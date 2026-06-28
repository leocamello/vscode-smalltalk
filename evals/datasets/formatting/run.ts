// Formatting output eval (US-416). Deterministic golden-dataset check, run in CI
// via `npm run eval`. Drives the pure formatter core (no gst, no VS Code) and
// asserts, per case in cases.json:
//   - formatSource(input, {...DEFAULT, ...options}) === expected   (golden output)
//   - formatSource(expected, options) === expected                 (idempotence: the golden is a fixed point)
//
// RED until server/src/format/formatter.ts ships (Acceptance Harness phase).
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { formatSource, DEFAULT_FORMAT_OPTIONS, type FormatOptions } from '../../../server/src/format/formatter.ts';

interface Case {
  name: string;
  input: string;
  expected: string;
  options?: Partial<FormatOptions>;
}

const here = path.dirname(fileURLToPath(import.meta.url));
const { cases } = JSON.parse(fs.readFileSync(path.join(here, 'cases.json'), 'utf8')) as { cases: Case[] };

let passed = 0;
let failed = 0;
for (const c of cases) {
  try {
    const opts = { ...DEFAULT_FORMAT_OPTIONS, ...(c.options ?? {}) };
    const got = formatSource(c.input, opts);
    assert.equal(got, c.expected, `formatted output mismatch\n--- expected ---\n${c.expected}\n--- got ---\n${got}`);
    // The golden must be a fixed point — formatting it again changes nothing.
    assert.equal(formatSource(c.expected, opts), c.expected, 'golden output must be idempotent (a fixed point)');
    passed += 1;
    console.log(`  ok - ${c.name}`);
  } catch (e) {
    failed += 1;
    console.error(`  FAIL - ${c.name}: ${(e as Error).message}`);
  }
}

console.log(`formatting eval: ${passed}/${cases.length} cases passed.`);
if (failed > 0) {
  process.exit(1);
}
