// Unit + snapshot tests for the GST chunk method format and GST primaries
// (US-411, slice 3b). Runs in Node via tsx.
//
//   npm run test:parser                # run
//   npm run test:parser -- --update    # regenerate snapshots
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { parse } from '../src/parser/parser.ts';
import {
  NodeKind,
  type BindingConstantNode,
  type CompileTimeConstantNode,
  type DefinitionNode,
  type MethodDefinitionNode,
  type Node,
} from '../src/parser/ast.ts';
import { serializeAst } from './astDump.ts';

const UPDATE = process.argv.includes('--update');
const ROOT = process.cwd();
const FIXTURE_DIR = path.join(ROOT, 'docs/research/gst-syntax/test-cases');
const SNAPSHOT_DIR = path.join(ROOT, 'server/test/__snapshots__');

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

const stmts = (src: string): Node[] => parse(src).ast.statements;
const firstStmt = (src: string): Node => stmts(src)[0] as Node;

// --- GST primaries (binding / compile-time) ---------------------------------
test('binding constant #{...}', () => {
  const b = firstStmt('#{Transcript}') as BindingConstantNode;
  assert.equal(b.kind, NodeKind.BindingConstant);
  assert.equal(b.path, 'Transcript');
  const scoped = firstStmt('#{TestBindings::MyBoundClass}') as BindingConstantNode;
  assert.equal(scoped.path, 'TestBindings::MyBoundClass');
});

test('compile-time constant ##(...)', () => {
  const c = firstStmt('##( 1 + 2 )') as CompileTimeConstantNode;
  assert.equal(c.kind, NodeKind.CompileTimeConstant);
  assert.equal(c.statements.length, 1);
  const withTemps = firstStmt('##( | a b | a := 5. a + b )') as CompileTimeConstantNode;
  assert.deepEqual(withTemps.temporaries.map((t) => t.name), ['a', 'b']);
  assert.equal(withTemps.statements.length, 2);
  const empty = firstStmt('##( )') as CompileTimeConstantNode;
  assert.equal(empty.statements.length, 0);
});

// --- Chunk method format (AC3) ----------------------------------------------
test('methodsFor: chunk section yields method definitions', () => {
  const d = firstStmt("Foo methodsFor: 'cat'! bar ^1 ! baz: x ^x ! !") as DefinitionNode;
  assert.equal(d.kind, NodeKind.Definition);
  assert.equal(d.definitionKind, 'methodsFor');
  const methods = d.body.map((m) => m as MethodDefinitionNode);
  assert.deepEqual(methods.map((m) => m.selector), ['bar', 'baz:']);
  assert.deepEqual(methods.map((m) => m.messageType), ['unary', 'keyword']);
  assert.equal((methods[0]?.target as { name?: string }).name, 'Foo');
});

test('class-side chunk section', () => {
  const d = firstStmt("Foo class methodsFor: 'cat'! make ^self new ! !") as DefinitionNode;
  const m = d.body[0] as MethodDefinitionNode;
  assert.equal(m.classSide, true);
  assert.equal((m.target as { name?: string }).name, 'Foo');
});

test('section ends at the first doit chunk, not a method pattern', () => {
  const s = stmts("Foo methodsFor: 'cat'! bar ^1 ! Other subclass: #X [ ] ");
  const section = s[0] as DefinitionNode;
  assert.equal(section.definitionKind, 'methodsFor');
  assert.equal(section.body.length, 1); // only `bar`, not the subclass doit
  // The subclass doit is a separate top-level definition.
  assert.ok(
    s.some((n) => n.kind === NodeKind.Definition && (n as DefinitionNode).definitionKind === 'subclass'),
    'expected the subclass doit as a separate definition',
  );
});

// --- Robustness --------------------------------------------------------------
test('fixtures 05/10/14 never throw and return a Program', () => {
  for (const file of ['05_core_identifiers_pseudo_vars', '10_gst_specific_chunks_eval',
    '14_gst_specific_binding_compile_time_constants']) {
    const src = fs.readFileSync(path.join(FIXTURE_DIR, `${file}.st`), 'utf8');
    assert.equal(parse(src).ast.kind, NodeKind.Program, `${file} should parse to a Program`);
  }
});

// --- Snapshot over the binding/compile-time fixture (AC3 remainder) ----------
for (const name of ['14_gst_specific_binding_compile_time_constants']) {
  test(`chunk/primitives snapshot: ${name}`, () => {
    const src = fs.readFileSync(path.join(FIXTURE_DIR, `${name}.st`), 'utf8');
    const actual = serializeAst(src);
    const snapPath = path.join(SNAPSHOT_DIR, `${name}.ast.txt`);
    if (UPDATE) {
      fs.mkdirSync(SNAPSHOT_DIR, { recursive: true });
      fs.writeFileSync(snapPath, actual);
      console.log(`    (updated ${path.relative(ROOT, snapPath)})`);
      return;
    }
    if (!fs.existsSync(snapPath)) {
      throw new Error(`Missing snapshot ${path.relative(ROOT, snapPath)}; run: npm run test:parser -- --update`);
    }
    assert.equal(actual, fs.readFileSync(snapPath, 'utf8'), `AST snapshot drift for ${name}; review, then: npm run test:parser -- --update`);
  });
}

console.log(`chunk: ${passed} tests passed.`);
