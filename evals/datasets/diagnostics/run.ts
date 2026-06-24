// Diagnostics output eval (US-414, Slice A). Deterministic golden-dataset check,
// run in CI via `npm run eval`. Drives the real parser front end + the
// diagnostics provider mapping (no gst, no VS Code) and asserts, per case in
// cases.json:
//   - expectClean:           no diagnostics at all
//   - expectMinCount:        at least N diagnostics
//   - expectCode:            every diagnostic carries this code (e.g. "parse")
//   - expectSeverity:        every diagnostic has this severity ("Error"|"Warning")
//   - expectOnLine:          some diagnostic starts on this 0-based line
//   - expectMessageIncludes: some diagnostic message contains this substring
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { DiagnosticSeverity, type Diagnostic } from 'vscode-languageserver-types';
import { parse } from '../../../server/src/parser/parser.ts';
import { toDiagnostics } from '../../../server/src/providers/diagnostics.ts';

interface Case {
  name: string;
  text: string;
  expectClean?: boolean;
  expectMinCount?: number;
  expectCode?: string;
  expectSeverity?: 'Error' | 'Warning';
  expectOnLine?: number;
  expectMessageIncludes?: string;
}

const here = path.dirname(fileURLToPath(import.meta.url));
const { cases } = JSON.parse(fs.readFileSync(path.join(here, 'cases.json'), 'utf8')) as { cases: Case[] };

const severityName = (s: Diagnostic['severity']): string =>
  s === DiagnosticSeverity.Error ? 'Error' : s === DiagnosticSeverity.Warning ? 'Warning' : String(s);

let passed = 0;
let failed = 0;
for (const c of cases) {
  try {
    const diags = toDiagnostics(parse(c.text).diagnostics);

    if (c.expectClean) {
      assert.equal(diags.length, 0, `expected no diagnostics, got ${diags.length}`);
    }
    if (c.expectMinCount !== undefined) {
      assert.ok(diags.length >= c.expectMinCount, `expected >= ${c.expectMinCount} diagnostics, got ${diags.length}`);
    }
    if (c.expectCode !== undefined) {
      for (const d of diags) assert.equal(d.code, c.expectCode, `every diagnostic code must be "${c.expectCode}"`);
    }
    if (c.expectSeverity !== undefined) {
      for (const d of diags) {
        assert.equal(severityName(d.severity), c.expectSeverity, `every diagnostic severity must be ${c.expectSeverity}`);
      }
    }
    if (c.expectOnLine !== undefined) {
      assert.ok(diags.some((d) => d.range.start.line === c.expectOnLine), `expected a diagnostic starting on line ${c.expectOnLine}`);
    }
    if (c.expectMessageIncludes !== undefined) {
      assert.ok(
        diags.some((d) => d.message.includes(c.expectMessageIncludes!)),
        `expected a diagnostic message containing "${c.expectMessageIncludes}"`,
      );
    }

    passed += 1;
    console.log(`  ok - ${c.name}`);
  } catch (e) {
    failed += 1;
    console.error(`  FAIL - ${c.name}: ${(e as Error).message}`);
  }
}

console.log(`diagnostics eval: ${passed}/${cases.length} cases passed.`);
if (failed > 0) {
  process.exit(1);
}
