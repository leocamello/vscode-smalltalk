// References output eval (US-423). Deterministic golden-dataset check, run in CI
// via `npm run eval`. Drives the real two-tier cross-reference engine over the
// committed bundled gst-3.2.5 cartridge (no corpus, no gst, no VS Code),
// assembling the XrefSources exactly as server.ts does, then packaging the
// result with buildCrossReference. Each case in cases.json marks the selector
// under the cursor with `▮` and pins the union count, per-row provenance, key
// labels, the Workspace ≻ Cartridge ranking, and the never-filter posture (AC6).
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { parse } from '../../../server/src/parser/parser.ts';
import { tokenize } from '../../../server/src/parser/lexer.ts';
import { SymbolKind } from '../../../server/src/parser/symbols.ts';
import { WorkspaceIndex } from '../../../server/src/providers/workspaceIndex.ts';
import { WorkspaceXref } from '../../../server/src/xref/workspaceXref.ts';
import {
  resolveImplementors,
  resolveSenders,
  type WorkspaceMethodRef,
  type XrefSources,
} from '../../../server/src/xref/resolve.ts';
import { resolveCallTarget } from '../../../server/src/providers/callHierarchy.ts';
import { buildCrossReference } from '../../../server/src/providers/crossReference.ts';
import { KernelIndexService } from '../../../server/src/kernel/kernelIndexService.ts';

interface Case {
  name: string;
  text: string;
  direction: 'senders' | 'implementors';
  expectMinCount?: number;
  expectProvenances?: string[];
  expectLabelsInclude?: string[];
  expectFirstProvenance?: string;
  expectFirstDetailIncludes?: string;
  expectTitleMatches?: string;
}

const here = path.dirname(fileURLToPath(import.meta.url));
const { cases } = JSON.parse(fs.readFileSync(path.join(here, 'cases.json'), 'utf8')) as { cases: Case[] };

// Bundled cartridge (deterministic; common-location probing disabled so an
// installed gst on the dev box can't change the golden results).
const kernel = new KernelIndexService(undefined, []);
kernel.configure({ kernelLibrary: 'bundled' });

const URI = 'file:///eval.st';

/** Assemble the two-tier sources for `selector` — mirrors server.ts buildXrefSources. */
function buildSources(index: WorkspaceIndex, xref: WorkspaceXref, selector: string): XrefSources {
  const entries = index.all();
  const workspaceImplementors: WorkspaceMethodRef[] = entries
    .filter((e) => e.kind === SymbolKind.Method && e.name === selector)
    .map((e) => ({
      uri: e.uri,
      side: e.classSide ? 'class' : 'instance',
      range: e.selectionRange,
      ...(e.containerName !== undefined ? { className: e.containerName } : {}),
    }));
  const workspaceClasses = new Set(
    entries.filter((e) => e.kind === SymbolKind.Class || e.kind === SymbolKind.Namespace).map((e) => e.name),
  );
  return {
    workspaceImplementors,
    workspaceSenders: xref.sendersOf(selector),
    cartridgeSenders: kernel.crossReferenceSenders(selector),
    cartridgeImplementors: kernel.crossReferenceImplementors(selector),
    ...(kernel.cartridgeId ? { cartridge: kernel.cartridgeId } : {}),
    isKnownClass: (name) => workspaceClasses.has(name) || kernel.hasClass(name),
  };
}

let passed = 0;
let failed = 0;
for (const c of cases) {
  try {
    const offset = c.text.indexOf('▮');
    assert.ok(offset >= 0, `case "${c.name}" must mark the selector with ▮`);
    const src = c.text.replace('▮', '');
    const index = new WorkspaceIndex();
    const xref = new WorkspaceXref();
    index.setFile(URI, src);
    xref.setFile(URI, src);

    const target = resolveCallTarget(parse(src).ast, tokenize(src).tokens, offset);
    assert.ok(target, `case "${c.name}": expected a selector at the marked position`);
    const sources = buildSources(index, xref, target.selector);
    const refs = c.direction === 'senders' ? resolveSenders(sources, target.selector) : resolveImplementors(sources, target.selector);
    const result = buildCrossReference(c.direction, target.selector, refs);

    assert.ok(result.disclaimer.length > 0, `case "${c.name}": result must carry the union/uncertainty disclaimer (AC2)`);
    if (c.expectTitleMatches !== undefined) {
      assert.match(result.title, new RegExp(c.expectTitleMatches), `case "${c.name}": title`);
    }
    if (c.expectMinCount !== undefined) {
      assert.ok(result.count >= c.expectMinCount, `case "${c.name}": expected >= ${c.expectMinCount} rows, got ${result.count}`);
    }
    for (const prov of c.expectProvenances ?? []) {
      assert.ok(result.rows.some((r) => r.provenance === prov), `case "${c.name}": expected a row with provenance "${prov}"`);
    }
    for (const label of c.expectLabelsInclude ?? []) {
      assert.ok(result.rows.some((r) => r.label === label), `case "${c.name}": expected a row labelled "${label}"`);
    }
    if (c.expectFirstProvenance !== undefined) {
      assert.equal(result.rows[0]?.provenance, c.expectFirstProvenance, `case "${c.name}": first row provenance (ranking)`);
    }
    if (c.expectFirstDetailIncludes !== undefined) {
      assert.ok(
        (result.rows[0]?.detail ?? '').includes(c.expectFirstDetailIncludes),
        `case "${c.name}": first row detail should include "${c.expectFirstDetailIncludes}" (AC6 ranking)`,
      );
    }
    passed += 1;
    console.log(`  ok - ${c.name}`);
  } catch (e) {
    failed += 1;
    console.error(`  FAIL - ${c.name}: ${(e as Error).message}`);
  }
}

console.log(`references eval: ${passed}/${cases.length} cases passed.`);
if (failed > 0) {
  process.exit(1);
}
