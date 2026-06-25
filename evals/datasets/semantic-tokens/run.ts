// Semantic-tokens output eval (US-422). Deterministic golden-dataset check, run
// in CI via `npm run eval`. Drives the real semantic-tokens provider over the
// committed bundled gst-3.2.5 kernel (no corpus, no gst, no VS Code) and asserts,
// per case in cases.json, that the token covering the `▮` position carries the
// expected role (`expectType`) and modifiers. This pins the cartridge-driven
// known-class vs unknown-global distinction (AC2) offline.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { parse } from '../../../server/src/parser/parser.ts';
import { buildSymbolTable } from '../../../server/src/parser/symbols.ts';
import { tokenize } from '../../../server/src/parser/lexer.ts';
import { SymbolKind, type SymbolNode } from '../../../server/src/parser/symbols.ts';
import {
  collectSemanticTokens,
  type RawSemanticToken,
  type SemanticTokenContext,
} from '../../../server/src/providers/semanticTokens.ts';
import { KernelIndexService } from '../../../server/src/kernel/kernelIndexService.ts';

interface Case {
  name: string;
  text: string;
  expectType: string;
  expectModifiers?: string[];
  expectAbsentModifiers?: string[];
}

const here = path.dirname(fileURLToPath(import.meta.url));
const { cases } = JSON.parse(fs.readFileSync(path.join(here, 'cases.json'), 'utf8')) as { cases: Case[] };

// Bundled kernel (deterministic; common-location probing disabled).
const kernel = new KernelIndexService(undefined, []);
kernel.configure({ kernelLibrary: 'bundled' });

/** Collect the class names a snippet itself defines (the workspace tier). */
function workspaceClasses(symbols: SymbolNode[], into = new Set<string>()): Set<string> {
  for (const s of symbols) {
    if (s.kind === SymbolKind.Class || s.kind === SymbolKind.Namespace) into.add(s.name);
    workspaceClasses(s.children, into);
  }
  return into;
}

/** Byte offset → {line, character}. */
function posAt(src: string, offset: number): { line: number; character: number } {
  let line = 0;
  let last = -1;
  for (let i = 0; i < offset; i++) {
    if (src[i] === '\n') {
      line += 1;
      last = i;
    }
  }
  return { line, character: offset - last - 1 };
}

function tokenAt(tokens: RawSemanticToken[], pos: { line: number; character: number }): RawSemanticToken | undefined {
  return tokens.find((t) => t.line === pos.line && t.char <= pos.character && pos.character < t.char + t.length);
}

let passed = 0;
let failed = 0;
for (const c of cases) {
  try {
    const offset = c.text.indexOf('▮');
    assert.ok(offset >= 0, `case "${c.name}" must mark the position with ▮`);
    const src = c.text.replace('▮', '');
    const ast = parse(src).ast;
    const symbols = buildSymbolTable(ast);
    const ws = workspaceClasses(symbols);
    const ctx: SemanticTokenContext = {
      hasCartridge: true,
      classOrigin: (name) => (ws.has(name) ? 'workspace' : kernel.hasClass(name) ? 'cartridge' : undefined),
    };
    const tokens = collectSemanticTokens(ast, symbols, tokenize(src).tokens, ctx);
    const tok = tokenAt(tokens, posAt(src, offset));
    assert.ok(tok, `case "${c.name}": expected a semantic token at the marked position`);
    assert.equal(tok.type, c.expectType, `case "${c.name}": expected type ${c.expectType}, got ${tok.type}`);
    for (const m of c.expectModifiers ?? []) {
      assert.ok(tok.modifiers.includes(m), `case "${c.name}": expected modifier "${m}"`);
    }
    for (const m of c.expectAbsentModifiers ?? []) {
      assert.ok(!tok.modifiers.includes(m), `case "${c.name}": expected NO modifier "${m}"`);
    }
    passed += 1;
    console.log(`  ok - ${c.name}`);
  } catch (e) {
    failed += 1;
    console.error(`  FAIL - ${c.name}: ${(e as Error).message}`);
  }
}

console.log(`semantic-tokens eval: ${passed}/${cases.length} cases passed.`);
if (failed > 0) {
  process.exit(1);
}
