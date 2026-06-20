// Kernel smoke test (US-411, slice 4 — the AC4 robustness gate).
//
// Parses every GNU Smalltalk 3.2.5 kernel source and asserts the front end:
//   - never throws (zero crashes), and
//   - keeps the total diagnostic count bounded,
//   - and builds a symbol table with classes for the corpus.
//
// The kernel corpus lives OUTSIDE the repo (../smalltalk-3.2.5/), so it is not
// present in CI — this test skips gracefully when the directory is absent and
// runs as a local gate. It is the gate that unblocks US-412+.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { parse } from '../src/parser/parser.ts';
import { buildSymbolTable, SymbolKind } from '../src/parser/symbols.ts';

const KERNEL_DIR = path.join(process.cwd(), '../smalltalk-3.2.5/kernel');

// Bounds: the whole corpus parses cleanly — zero crashes and zero diagnostics
// across all files — after the dotted-namespace (`A.B`) and implicit-receiver fixes.
const MAX_TOTAL_DIAGNOSTICS = 0;

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

if (!fs.existsSync(KERNEL_DIR)) {
  console.log(`kernel: skipped (corpus not found at ${path.relative(process.cwd(), KERNEL_DIR)}).`);
} else {
  const files = fs.readdirSync(KERNEL_DIR).filter((f) => f.endsWith('.st'));

  test(`parses all ${files.length} kernel files with zero crashes and zero diagnostics`, () => {
    let crashes = 0;
    let totalDiagnostics = 0;
    let cleanFiles = 0;
    let classesFound = 0;

    for (const file of files) {
      const src = fs.readFileSync(path.join(KERNEL_DIR, file), 'utf8');
      let result;
      try {
        result = parse(src);
      } catch (e) {
        crashes += 1;
        console.error(`  CRASH in ${file}: ${(e as Error).message}`);
        continue;
      }
      totalDiagnostics += result.diagnostics.length;
      if (result.diagnostics.length === 0) {
        cleanFiles += 1;
      }
      const syms = buildSymbolTable(result.ast);
      classesFound += syms.filter((s) => s.kind === SymbolKind.Class).length;
    }

    console.log(
      `    files=${files.length} crashes=${crashes} diagnostics=${totalDiagnostics}` +
        ` clean=${cleanFiles} classes=${classesFound}`,
    );
    assert.equal(crashes, 0, 'the parser must never throw on any kernel file');
    assert.ok(
      totalDiagnostics <= MAX_TOTAL_DIAGNOSTICS,
      `total diagnostics ${totalDiagnostics} exceeds bound ${MAX_TOTAL_DIAGNOSTICS}`,
    );
    assert.equal(cleanFiles, files.length, `expected all ${files.length} files clean (got ${cleanFiles})`);
    assert.ok(classesFound >= files.length, `expected at least one class per file (got ${classesFound})`);
  });
}

console.log(`kernel: ${passed} tests passed.`);
