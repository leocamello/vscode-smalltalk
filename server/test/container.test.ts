// Unit + snapshot tests for the GST brace container format (US-411, slice 3a).
// Runs in Node via tsx.
//
//   npm run test:parser                # run (lexer + parser + container suites)
//   npm run test:parser -- --update    # regenerate snapshots
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { parse } from '../src/parser/parser.ts';
import {
  NodeKind,
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

const firstStmt = (src: string): Node => {
  const s = parse(src).ast.statements;
  assert.ok(s.length >= 1, `expected a statement for ${JSON.stringify(src)}`);
  return s[0] as Node;
};
const asDef = (n: Node): DefinitionNode => {
  assert.equal(n.kind, NodeKind.Definition);
  return n as DefinitionNode;
};
const asMethod = (n: Node): MethodDefinitionNode => {
  assert.equal(n.kind, NodeKind.MethodDefinition);
  return n as MethodDefinitionNode;
};

// --- Class definitions (AC3) -------------------------------------------------
test('subclass definition with instance variables', () => {
  const d = asDef(firstStmt('Object subclass: #Foo [ | a b | ]'));
  assert.equal(d.definitionKind, 'subclass');
  assert.equal(d.name, 'Foo');
  const ivars = d.body[0];
  assert.equal(ivars?.kind, NodeKind.InstanceVariables);
  assert.deepEqual((ivars as { names: { name: string }[] }).names.map((n) => n.name), ['a', 'b']);
});

test('namespace, extend, and class-side scopes', () => {
  assert.equal(asDef(firstStmt('Namespace current: #X [ ]')).definitionKind, 'namespace');
  assert.equal(asDef(firstStmt('Namespace current: #X [ ]')).name, 'X');
  assert.equal(asDef(firstStmt('obj extend [ foo [ ^1 ] ]')).definitionKind, 'extend');
  assert.equal(asDef(firstStmt('Foo class [ Bar := 1 ]')).definitionKind, 'classScope');
});

test('nested method definitions live in the class body', () => {
  const d = asDef(firstStmt('Object subclass: #Foo [ Foo >> bar [ ^1 ] ]'));
  const m = asMethod(d.body[0] as Node);
  assert.equal(m.selector, 'bar');
});

// --- Method definitions (AC3) ------------------------------------------------
test('unary, class-side, keyword, and binary method patterns', () => {
  const unary = asMethod(firstStmt('Foo >> bar [ ^1 ]'));
  assert.equal(unary.messageType, 'unary');
  assert.equal(unary.selector, 'bar');
  assert.equal(unary.classSide, false);
  assert.equal((unary.target as { name?: string }).name, 'Foo');

  const cls = asMethod(firstStmt('Foo class >> make [ ^self new ]'));
  assert.equal(cls.classSide, true);

  const kw = asMethod(firstStmt('Foo >> at: k put: v [ ^k ]'));
  assert.equal(kw.messageType, 'keyword');
  assert.equal(kw.selector, 'at:put:');
  assert.deepEqual(kw.parameters.map((p) => p.name), ['k', 'v']);

  const bin = asMethod(firstStmt('Foo >> + other [ ^other ]'));
  assert.equal(bin.messageType, 'binary');
  assert.equal(bin.selector, '+');
  assert.deepEqual(bin.parameters.map((p) => p.name), ['other']);
});

test('method body with temporaries and a return', () => {
  const m = asMethod(firstStmt('Foo >> run [ | t | t := 1. ^t ]'));
  assert.deepEqual(m.temporaries.map((t) => t.name), ['t']);
  assert.equal(m.statements.length, 2);
  assert.equal(m.statements[1]?.kind, NodeKind.Return);
});

// --- Pragmas / attributes (AC3) ---------------------------------------------
test('method pragma and class-body attribute', () => {
  const m = asMethod(firstStmt('Foo >> prim [ <primitive: 80> ^1 ]'));
  assert.equal(m.pragmas.length, 1);
  assert.equal(m.pragmas[0]?.selector, 'primitive:');

  const d = asDef(firstStmt("Object subclass: #C [ <gst.classCategory: 'x'> | v | ]"));
  assert.equal(d.body[0]?.kind, NodeKind.Pragma);
  assert.equal(d.body[1]?.kind, NodeKind.InstanceVariables);
});

// --- Robustness --------------------------------------------------------------
test('container fixtures never throw and return a Program', () => {
  for (const file of ['10_gst_specific_chunks_eval', '11_gst_specific_namespace_class_def',
    '12_gst_specific_extend_scoped_methods', '13_gst_specific_attributes_array_constructor',
    '14_gst_specific_binding_compile_time_constants']) {
    const src = fs.readFileSync(path.join(FIXTURE_DIR, `${file}.st`), 'utf8');
    assert.equal(parse(src).ast.kind, NodeKind.Program, `${file} should parse to a Program`);
  }
});

// --- Snapshots over the brace-format fixtures (AC3) --------------------------
const SNAPSHOT_FIXTURES = [
  '11_gst_specific_namespace_class_def',
  '12_gst_specific_extend_scoped_methods',
  '13_gst_specific_attributes_array_constructor',
];

for (const name of SNAPSHOT_FIXTURES) {
  test(`container snapshot: ${name}`, () => {
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

console.log(`container: ${passed} tests passed.`);
