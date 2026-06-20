// Unit + snapshot tests for the GNU Smalltalk expression parser (US-411, slice 2).
// Runs in Node via tsx. Same idiom as lexer.test.ts.
//
//   npm run test:parser                # run (lexer + parser suites)
//   npm run test:parser -- --update    # regenerate the AST snapshots
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { parse } from '../src/parser/parser.ts';
import {
  NodeKind,
  type CascadeNode,
  type MessageNode,
  type Node,
  type AssignmentNode,
  type BlockNode,
} from '../src/parser/ast.ts';

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
const firstStmt = (src: string): Node => {
  const s = stmts(src);
  assert.ok(s.length >= 1, `expected a statement for ${JSON.stringify(src)}`);
  return s[0] as Node;
};
function asMessage(n: Node): MessageNode {
  assert.equal(n.kind, NodeKind.Message);
  return n as MessageNode;
}

// --- Precedence (AC2) --------------------------------------------------------
test('binary is left-associative: 1 + 2 * 3 => (1 + 2) * 3', () => {
  const m = asMessage(firstStmt('1 + 2 * 3'));
  assert.equal(m.selector, '*');
  const lhs = asMessage(m.receiver);
  assert.equal(lhs.selector, '+');
});

test('unary binds tighter than binary: 2 * 3 factorial', () => {
  const m = asMessage(firstStmt('2 * 3 factorial'));
  assert.equal(m.selector, '*');
  const arg = asMessage(m.arguments[0] as Node);
  assert.equal(arg.messageType, 'unary');
  assert.equal(arg.selector, 'factorial');
});

test('keyword is loosest and joins parts: a foo: 1 + 2 bar: 3', () => {
  const m = asMessage(firstStmt('a foo: 1 + 2 bar: 3'));
  assert.equal(m.messageType, 'keyword');
  assert.equal(m.selector, 'foo:bar:');
  assert.equal(m.arguments.length, 2);
  assert.equal(asMessage(m.arguments[0] as Node).selector, '+');
});

test('a lone pipe is a binary selector: true | false', () => {
  const m = asMessage(firstStmt('true | false'));
  assert.equal(m.messageType, 'binary');
  assert.equal(m.selector, '|');
});

// --- Assignment & return (AC2) ----------------------------------------------
test('assignment is right-associative: a := b := 1', () => {
  const a = firstStmt('a := b := 1') as AssignmentNode;
  assert.equal(a.kind, NodeKind.Assignment);
  assert.equal(a.target.name, 'a');
  const inner = a.value as AssignmentNode;
  assert.equal(inner.kind, NodeKind.Assignment);
  assert.equal(inner.target.name, 'b');
});

test('legacy underscore assignment parses', () => {
  const a = firstStmt('x _ 20') as AssignmentNode;
  assert.equal(a.kind, NodeKind.Assignment);
  assert.equal(a.target.name, 'x');
});

test('caret return statement', () => {
  const r = firstStmt('^ 42');
  assert.equal(r.kind, NodeKind.Return);
});

// --- Cascades (AC2) ----------------------------------------------------------
test('cascade keeps the first send receiver; segments are message chains', () => {
  const c = firstStmt('x foo; bar; baz: 1') as CascadeNode;
  assert.equal(c.kind, NodeKind.Cascade);
  assert.equal((c.receiver as { name?: string }).name, 'x');
  const segs = c.messages.map((m) => m as MessageNode);
  assert.deepEqual(segs.map((s) => s.selector), ['foo', 'bar', 'baz:']);
  assert.deepEqual(segs.map((s) => s.messageType), ['unary', 'unary', 'keyword']);
  // Each segment is rooted at the zero-width cascade-receiver marker.
  assert.equal(segs[0]?.receiver.kind, NodeKind.CascadeReceiver);
});

test('cascade segment can be a chain (yourself printNl)', () => {
  const c = firstStmt('d at: 1; yourself printNl') as CascadeNode;
  const last = c.messages.at(-1) as MessageNode;
  assert.equal(last.selector, 'printNl');
  assert.equal((last.receiver as MessageNode).selector, 'yourself');
});

// --- Blocks (AC2) ------------------------------------------------------------
test('block with params and temporaries', () => {
  const b = firstStmt('[ :x | | t | t := x. t ]') as BlockNode;
  assert.equal(b.kind, NodeKind.Block);
  assert.deepEqual(b.parameters.map((p) => p.name), ['x']);
  assert.deepEqual(b.temporaries.map((t) => t.name), ['t']);
  assert.equal(b.statements.length, 2);
});

test('empty temporaries: [ | | 1 ]', () => {
  const b = firstStmt('[ | | 1 ]') as BlockNode;
  assert.deepEqual(b.temporaries, []);
  assert.equal(b.statements.length, 1);
});

test('doubled-pipe separator with no temporaries: [ :a :b || a ]', () => {
  const b = firstStmt('[ :a :b || a ]') as BlockNode;
  assert.deepEqual(b.parameters.map((p) => p.name), ['a', 'b']);
  assert.deepEqual(b.temporaries, []);
  assert.equal(b.statements.length, 1);
  assert.equal((b.statements[0] as { name?: string }).name, 'a');
});

test('block with explicit return', () => {
  const b = firstStmt('[ ^42 ]') as BlockNode;
  assert.equal(b.statements[0]?.kind, NodeKind.Return);
});

// --- Collection primaries (AC2) ---------------------------------------------
test('literal array, byte array, dynamic array', () => {
  assert.equal(firstStmt('#(1 $a #sym)').kind, NodeKind.LiteralArray);
  assert.equal(firstStmt('#[1 2 255]').kind, NodeKind.ByteArray);
  const d = firstStmt('{ 1 + 1. 2 }');
  assert.equal(d.kind, NodeKind.DynamicArray);
  assert.equal((d as { elements: Node[] }).elements.length, 2);
});

// --- Recovery / never-throws (AC4 preview, required for AC2 robustness) ------
test('malformed input recovers with an Error node and never throws', () => {
  const r = parse(') + + .');
  assert.equal(r.ast.kind, NodeKind.Program);
  assert.ok(r.diagnostics.length >= 1, 'expected at least one diagnostic');
  // Recovery yields Error nodes somewhere in the tree (here, nested in a binary send).
  assert.ok(
    JSON.stringify(r.ast).includes(`"kind":"${NodeKind.Error}"`),
    'expected an Error node somewhere in the AST',
  );
});

test('parsing every fixture returns a Program and never throws', () => {
  for (const file of fs.readdirSync(FIXTURE_DIR)) {
    if (!file.endsWith('.st')) continue;
    const src = fs.readFileSync(path.join(FIXTURE_DIR, file), 'utf8');
    const { ast } = parse(src);
    assert.equal(ast.kind, NodeKind.Program, `${file} should parse to a Program`);
  }
});

// --- AST snapshots over the expression fixtures (AC2) ------------------------
const SNAPSHOT_FIXTURES = [
  '06_core_message_sends',
  '07_core_assignments_cascades',
  '08_core_blocks',
];

function names(list: ReadonlyArray<{ name: string }>): string {
  return list.map((n) => n.name).join(', ');
}

function dump(node: Node, indent: string, out: string[]): void {
  const at = `@${node.start}..${node.end}`;
  const child = indent + '  ';
  switch (node.kind) {
    case NodeKind.Program:
      out.push(`${indent}Program temps=[${names(node.temporaries)}] ${at}`);
      node.statements.forEach((s) => dump(s, child, out));
      break;
    case NodeKind.Block:
      out.push(`${indent}Block params=[${names(node.parameters)}] temps=[${names(node.temporaries)}] ${at}`);
      node.statements.forEach((s) => dump(s, child, out));
      break;
    case NodeKind.Return:
      out.push(`${indent}Return ${at}`);
      dump(node.value, child, out);
      break;
    case NodeKind.Assignment:
      out.push(`${indent}Assignment target='${node.target.name}' ${at}`);
      dump(node.value, child, out);
      break;
    case NodeKind.Message:
      out.push(`${indent}Message ${node.messageType} '${node.selector}' ${at}`);
      dump(node.receiver, child, out);
      node.arguments.forEach((a) => dump(a, child, out));
      break;
    case NodeKind.Cascade:
      out.push(`${indent}Cascade ${at}`);
      dump(node.receiver, child, out);
      node.messages.forEach((m) => {
        out.push(`${child};`);
        dump(m, child + '  ', out);
      });
      break;
    case NodeKind.CascadeReceiver:
      out.push(`${indent}CascadeReceiver ${at}`);
      break;
    case NodeKind.Variable:
      out.push(`${indent}Variable '${node.name}' ${at}`);
      break;
    case NodeKind.Literal:
      out.push(`${indent}Literal ${node.literalKind} ${JSON.stringify(node.value)} ${at}`);
      break;
    case NodeKind.LiteralArray:
    case NodeKind.ByteArray:
    case NodeKind.DynamicArray:
      out.push(`${indent}${node.kind} ${at}`);
      node.elements.forEach((e) => dump(e, child, out));
      break;
    case NodeKind.Error:
      out.push(`${indent}Error ${JSON.stringify(node.message)} ${at}`);
      break;
  }
}

function serialize(src: string): string {
  const { ast, diagnostics } = parse(src);
  const out: string[] = [];
  dump(ast, '', out);
  const diag = diagnostics.map((d) => `${d.severity} ${JSON.stringify(d.message)} @${d.start}..${d.end}`);
  return [...out, '', '--- diagnostics ---', ...diag, ''].join('\n');
}

for (const name of SNAPSHOT_FIXTURES) {
  test(`ast snapshot: ${name}`, () => {
    const src = fs.readFileSync(path.join(FIXTURE_DIR, `${name}.st`), 'utf8');
    const actual = serialize(src);
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

console.log(`parser: ${passed} tests passed.`);
