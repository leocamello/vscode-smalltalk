// Completion output eval (US-413). Deterministic golden-dataset check, run in CI
// via `npm run eval`. Drives the real completion provider over the committed
// bundled gst-3.2.5 kernel index (no corpus, no gst, no VS Code) and asserts the
// metric for each case in cases.json:
//   - expectTopK:       each label appears within the first k items (sortText order)
//   - expectSnippet:    a keyword selector inserts as a Snippet with the given text
//   - expectOrderBefore: label A is ranked before label B (workspace>kernel, var>class)
//   - expectAbsent:     labels must not appear (e.g. binary selectors for a typed word)
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { InsertTextFormat, type CompletionItem } from 'vscode-languageserver-types';
import { parse } from '../../../server/src/parser/parser.ts';
import { buildSymbolTable } from '../../../server/src/parser/symbols.ts';
import { tokenize } from '../../../server/src/parser/lexer.ts';
import { completionsAt, type ClassCandidate, type SelectorCandidate } from '../../../server/src/providers/completion.ts';
import { KernelIndexService } from '../../../server/src/kernel/kernelIndexService.ts';
import { Provenance } from '../../../server/src/kernel/model.ts';

interface Case {
  name: string;
  text: string;
  workspaceSelectors?: string[];
  workspaceClasses?: string[];
  expectTopK?: { k: number; labels: string[] };
  expectSnippet?: { label: string; insertText: string };
  expectOrderBefore?: [string, string][];
  expectAbsent?: string[];
}

const here = path.dirname(fileURLToPath(import.meta.url));
const { cases } = JSON.parse(fs.readFileSync(path.join(here, 'cases.json'), 'utf8')) as { cases: Case[] };

// Bundled kernel candidates (deterministic; common-location probing disabled).
const kernel = new KernelIndexService(undefined, []);
kernel.configure({ kernelLibrary: 'bundled' });
const kernelSelectors: SelectorCandidate[] = kernel.selectors().map((s) => ({ selector: s.selector, provenance: s.provenance }));
const kernelClasses: ClassCandidate[] = kernel.classes().map((c) => ({ name: c.name, provenance: c.provenance }));

const labelOf = (i: CompletionItem): string => (typeof i.label === 'string' ? i.label : i.label.label);

function itemsFor(c: Case): CompletionItem[] {
  const offset = c.text.indexOf('▮');
  assert.ok(offset >= 0, `case "${c.name}" must mark the cursor with ▮`);
  const text = c.text.replace('▮', '');
  const selectors: SelectorCandidate[] = [
    ...(c.workspaceSelectors ?? []).map((s) => ({ selector: s, provenance: Provenance.Workspace })),
    ...kernelSelectors,
  ];
  const classes: ClassCandidate[] = [
    ...(c.workspaceClasses ?? []).map((n) => ({ name: n, provenance: Provenance.Workspace })),
    ...kernelClasses,
  ];
  const ast = parse(text).ast;
  const items = completionsAt(offset, text, tokenize(text).tokens, ast, buildSymbolTable(ast), selectors, classes);
  // Sort as VS Code does: by sortText (falling back to label).
  return [...items].sort((a, b) => (a.sortText ?? labelOf(a)).localeCompare(b.sortText ?? labelOf(b)));
}

let passed = 0;
let failed = 0;
for (const c of cases) {
  try {
    const items = itemsFor(c);
    const labels = items.map(labelOf);

    if (c.expectTopK) {
      for (const label of c.expectTopK.labels) {
        const idx = labels.indexOf(label);
        assert.ok(idx >= 0, `expected "${label}" in completions`);
        assert.ok(idx < c.expectTopK.k, `expected "${label}" within top-${c.expectTopK.k} (got rank ${idx})`);
      }
    }
    if (c.expectSnippet) {
      const item = items.find((i) => labelOf(i) === c.expectSnippet!.label);
      assert.ok(item, `expected snippet item "${c.expectSnippet.label}"`);
      assert.equal(item.insertTextFormat, InsertTextFormat.Snippet, `"${c.expectSnippet.label}" must be a Snippet`);
      assert.equal(item.insertText, c.expectSnippet.insertText, `"${c.expectSnippet.label}" snippet text`);
    }
    for (const [a, b] of c.expectOrderBefore ?? []) {
      const ia = labels.indexOf(a);
      const ib = labels.indexOf(b);
      assert.ok(ia >= 0 && ib >= 0, `expected both "${a}" and "${b}" present`);
      assert.ok(ia < ib, `expected "${a}" ranked before "${b}" (got ${ia} vs ${ib})`);
    }
    for (const label of c.expectAbsent ?? []) {
      assert.ok(!labels.includes(label), `expected "${label}" to be absent`);
    }

    passed += 1;
    console.log(`  ok - ${c.name}`);
  } catch (e) {
    failed += 1;
    console.error(`  FAIL - ${c.name}: ${(e as Error).message}`);
  }
}

console.log(`completion eval: ${passed}/${cases.length} cases passed (kernel: ${kernelClasses.length} classes).`);
if (failed > 0) {
  process.exit(1);
}
