// Hover output eval (US-415). Deterministic golden-dataset check, run in CI via
// `npm run eval`. Drives the real hover provider over the committed bundled
// gst-3.2.5 kernel (no corpus, no gst, no VS Code) and asserts, per case in
// cases.json, that the rendered Markdown contains every `expectIncludes` substring
// and none of the `expectAbsent` ones. Bundled = facts-only, so prose never appears.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import type { MarkupContent } from 'vscode-languageserver-types';
import { parse } from '../../../server/src/parser/parser.ts';
import { buildSymbolTable } from '../../../server/src/parser/symbols.ts';
import { tokenize } from '../../../server/src/parser/lexer.ts';
import { hoverAt, type HoverContext, type HoverImplementor } from '../../../server/src/providers/hover.ts';
import { KernelIndexService } from '../../../server/src/kernel/kernelIndexService.ts';

interface Case {
  name: string;
  text: string;
  expectIncludes?: string[];
  expectAbsent?: string[];
}

const here = path.dirname(fileURLToPath(import.meta.url));
const { cases } = JSON.parse(fs.readFileSync(path.join(here, 'cases.json'), 'utf8')) as { cases: Case[] };

// Bundled kernel (deterministic; common-location probing disabled).
const kernel = new KernelIndexService(undefined, []);
kernel.configure({ kernelLibrary: 'bundled' });

const ctx: HoverContext = {
  isClass: (name) => kernel.hasClass(name),
  superclassOf: (name) => kernel.superclassOf(name),
  implementorsOf: (selector): HoverImplementor[] =>
    kernel.implementorsOf(selector).map((c) => ({ className: c.name, provenance: c.provenance })),
};

function markdownFor(c: Case): string {
  const offset = c.text.indexOf('▮');
  assert.ok(offset >= 0, `case "${c.name}" must mark the cursor with ▮`);
  const text = c.text.replace('▮', '');
  const ast = parse(text).ast;
  const hover = hoverAt(offset, text, tokenize(text).tokens, ast, buildSymbolTable(ast), ctx);
  assert.ok(hover, `case "${c.name}" expected a hover`);
  return (hover.contents as MarkupContent).value;
}

let passed = 0;
let failed = 0;
for (const c of cases) {
  try {
    const md = markdownFor(c);
    for (const sub of c.expectIncludes ?? []) {
      assert.ok(md.includes(sub), `expected hover to include "${sub}"\n--- got ---\n${md}`);
    }
    for (const sub of c.expectAbsent ?? []) {
      assert.ok(!md.includes(sub), `expected hover NOT to include "${sub}"`);
    }
    passed += 1;
    console.log(`  ok - ${c.name}`);
  } catch (e) {
    failed += 1;
    console.error(`  FAIL - ${c.name}: ${(e as Error).message}`);
  }
}

console.log(`hover eval: ${passed}/${cases.length} cases passed.`);
if (failed > 0) {
  process.exit(1);
}
