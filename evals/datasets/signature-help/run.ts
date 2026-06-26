// Signature-help output eval (US-425). Deterministic golden-dataset check, run in
// CI via `npm run eval`. Drives the real `signatureHelpAt` provider over the
// committed bundled gst-3.2.5 cartridge (no corpus, no gst, no VS Code),
// assembling the SignatureCandidate set exactly as server.ts does (workspace
// method selectors ∪ kernel keyword selectors). Each case marks the cursor with
// `▮` and pins the active parameter, the keyword-prefix union (selectors that must
// / must not appear), the active signature, and the null cases (non-keyword
// cursors / unknown selectors).
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { tokenize } from '../../../server/src/parser/lexer.ts';
import { SymbolKind } from '../../../server/src/parser/symbols.ts';
import { WorkspaceIndex } from '../../../server/src/providers/workspaceIndex.ts';
import { Provenance } from '../../../server/src/kernel/model.ts';
import { signatureHelpAt, type SignatureCandidate } from '../../../server/src/providers/signatureHelp.ts';
import { KernelIndexService } from '../../../server/src/kernel/kernelIndexService.ts';

interface Case {
  name: string;
  text: string;
  expectNull?: boolean;
  expectActiveParameter?: number;
  expectActiveSelector?: string;
  expectSelectorsInclude?: string[];
  expectSelectorsExclude?: string[];
}

const here = path.dirname(fileURLToPath(import.meta.url));
const { cases } = JSON.parse(fs.readFileSync(path.join(here, 'cases.json'), 'utf8')) as { cases: Case[] };

// Bundled cartridge (deterministic; common-location probing disabled so an
// installed gst on the dev box can't change the golden results).
const kernel = new KernelIndexService(undefined, []);
kernel.configure({ kernelLibrary: 'bundled' });

const URI = 'file:///eval.st';
const keywordsOf = (selector: string): string[] => selector.match(/[^:]+:/g) ?? [selector];
const isKeyword = (selector: string): boolean => selector.includes(':');

/** Assemble the candidate set — mirrors server.ts onSignatureHelp. */
function buildCandidates(index: WorkspaceIndex): SignatureCandidate[] {
  return [
    ...index
      .all()
      .filter((e) => e.kind === SymbolKind.Method && isKeyword(e.name))
      .map((e) => ({ selector: e.name, keywords: keywordsOf(e.name), provenance: Provenance.Workspace })),
    ...kernel
      .selectors()
      .filter((s) => isKeyword(s.selector))
      .map((s) => ({ selector: s.selector, keywords: keywordsOf(s.selector), provenance: s.provenance })),
  ];
}

let passed = 0;
let failed = 0;
for (const c of cases) {
  try {
    const offset = c.text.indexOf('▮');
    assert.ok(offset >= 0, `case "${c.name}" must mark the cursor with ▮`);
    const src = c.text.replace('▮', '');
    const index = new WorkspaceIndex();
    index.setFile(URI, src);

    const help = signatureHelpAt(offset, src, tokenize(src).tokens, buildCandidates(index));

    if (c.expectNull) {
      assert.equal(help, null, `case "${c.name}": expected no signature help`);
      passed += 1;
      console.log(`  ok - ${c.name}`);
      continue;
    }

    assert.ok(help, `case "${c.name}": expected signature help`);
    if (c.expectActiveParameter !== undefined) {
      assert.equal(help.activeParameter, c.expectActiveParameter, `case "${c.name}": active parameter`);
    }
    const selectors = new Set(help.signatures.map((s) => s.selector));
    for (const sel of c.expectSelectorsInclude ?? []) {
      assert.ok(selectors.has(sel), `case "${c.name}": expected selector "${sel}" in the union`);
    }
    for (const sel of c.expectSelectorsExclude ?? []) {
      assert.ok(!selectors.has(sel), `case "${c.name}": selector "${sel}" must NOT be in the union`);
    }
    if (c.expectActiveSelector !== undefined) {
      const active = help.signatures[help.activeSignature ?? 0];
      assert.equal(active?.selector, c.expectActiveSelector, `case "${c.name}": active signature`);
    }
    passed += 1;
    console.log(`  ok - ${c.name}`);
  } catch (e) {
    failed += 1;
    console.error(`  FAIL - ${c.name}: ${(e as Error).message}`);
  }
}

console.log(`signature-help eval: ${passed}/${cases.length} cases passed.`);
if (failed > 0) {
  process.exit(1);
}
