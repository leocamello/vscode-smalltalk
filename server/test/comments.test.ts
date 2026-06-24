// Unit tests for comment extraction (US-415 slice B). Runs in Node via tsx.
// Written BEFORE the extractor (Acceptance Harness): RED until
// server/src/parser/comments.ts ships. Extraction is best-effort and pure.
import assert from 'node:assert/strict';
import { parse } from '../src/parser/parser.ts';
import { tokenize } from '../src/parser/lexer.ts';
import { extractComments } from '../src/parser/comments.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

function commentsOf(src: string) {
  const ast = parse(src).ast;
  return extractComments(ast, tokenize(src).tokens);
}

test('class comment is read from a <comment: ...> pragma', () => {
  const c = commentsOf("Object subclass: Foo [ <comment: 'A demo class.'> bar [ ^1 ] ]");
  assert.equal(c.classComment('Foo'), 'A demo class.');
});

test("class comment unescapes doubled single quotes", () => {
  const c = commentsOf("Object subclass: Foo [ <comment: 'It''s fine.'> ]");
  assert.equal(c.classComment('Foo'), "It's fine.");
});

test('method comment is read from the leading "..." comment', () => {
  const c = commentsOf('Object subclass: Foo [ bar [ "Answer one." ^1 ] ]');
  assert.equal(c.methodComment('Foo', false, 'bar'), 'Answer one.');
});

test('class-side method comment is keyed on the class side', () => {
  const c = commentsOf('Object subclass: Foo [ Foo class >> make [ "Build a Foo." ^self new ] ]');
  assert.equal(c.methodComment('Foo', true, 'make'), 'Build a Foo.');
  assert.equal(c.methodComment('Foo', false, 'make'), undefined);
});

test('method comment unescapes doubled double quotes', () => {
  const c = commentsOf('Object subclass: Foo [ bar [ "say ""hi""." ^1 ] ]');
  assert.equal(c.methodComment('Foo', false, 'bar'), 'say "hi".');
});

test('no comment yields undefined (best-effort)', () => {
  const c = commentsOf('Object subclass: Foo [ bar [ ^1 ] ]');
  assert.equal(c.classComment('Foo'), undefined);
  assert.equal(c.methodComment('Foo', false, 'bar'), undefined);
});

console.log(`comments.test: ${passed} passed`);
