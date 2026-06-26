// Acceptance harness for US-423 — References + Senders/Implementors (Two-Tier Engine).
//
// TDD e2e: written BEFORE implementation. Each user-observable acceptance
// criterion from spec.md §4 gets a test that drives the real LSP providers via
// VS Code's built-in commands (`executeReferenceProvider`,
// `executeDefinitionProvider`, `prepareCallHierarchy` + `provideIncomingCalls`)
// and asserts on the result. RED until the providers ship. Run: npm run test:e2e
const assert = require('node:assert');
const vscode = require('vscode');

/** Poll `fn` until `ok(result)` or tries run out; returns the last result. */
async function waitFor(fn, ok, tries = 60) {
  let last;
  for (let i = 0; i < tries; i++) {
    last = await fn();
    if (ok(last)) return last;
    await new Promise((r) => setTimeout(r, 250));
  }
  return last;
}

/** Open an in-memory Smalltalk doc and return it after the server attaches. */
async function openSmalltalk(content) {
  const doc = await vscode.workspace.openTextDocument({ language: 'smalltalk', content });
  await vscode.window.showTextDocument(doc);
  return doc;
}

/** Position of the n-th occurrence of `needle` in `text`. */
function positionOfNth(doc, text, needle, occurrence = 0) {
  let from = -1;
  for (let i = 0; i <= occurrence; i++) {
    from = text.indexOf(needle, from + 1);
    if (from < 0) return undefined;
  }
  return doc.positionAt(from);
}

suite('US-423 references + senders/implementors (e2e)', () => {
  suiteSetup(async () => {
    const ext = vscode.extensions.getExtension('leocamello.vscode-smalltalk');
    assert.ok(ext, 'extension should be present');
    await waitFor(() => Promise.resolve(ext.isActive), (active) => active === true);
    assert.ok(ext.isActive, 'extension must be active');
  });

  // Two classes implement `greet`; a third method sends it. The union of
  // implementors + the send site is the cross-reference "everything that
  // responds to / sends #greet" (AC1).
  const SRC = [
    'Object subclass: Speaker [',
    '  greet [ ^self ]',
    ']',
    'Object subclass: Robot [',
    '  greet [ ^self ]',
    ']',
    'Object subclass: Stage [',
    '  run: who [ ^who greet ]',
    ']',
    '',
  ].join('\n');

  test('AC1: references returns the de-duplicated union of send-sites + definitions', async () => {
    const doc = await openSmalltalk(SRC);
    // Cursor on the `greet` send inside Stage>>run:.
    const pos = positionOfNth(doc, SRC, 'greet', 2); // 0,1 are the defs; 2 is the send
    assert.ok(pos, 'the send site is locatable');
    const refs = await waitFor(
      () => vscode.commands.executeCommand('vscode.executeReferenceProvider', doc.uri, pos),
      (r) => Array.isArray(r) && r.length >= 3,
    );
    assert.ok(Array.isArray(refs), 'references returns a Location array');
    // Two implementors (Speaker, Robot) + at least the one send site.
    assert.ok(refs.length >= 3, `union has both implementors and the send (got ${refs && refs.length})`);
  });

  test('AC3: go-to-definition on a message send returns plural targets (never collapses to one)', async () => {
    const doc = await openSmalltalk(SRC);
    const pos = positionOfNth(doc, SRC, 'greet', 2);
    const defs = await waitFor(
      () => vscode.commands.executeCommand('vscode.executeDefinitionProvider', doc.uri, pos),
      (d) => Array.isArray(d) && d.length >= 2,
    );
    assert.ok(Array.isArray(defs) && defs.length >= 2, `both implementors are offered (got ${defs && defs.length})`);
  });

  test('AC2: Implementors of… returns a tree result with a union disclaimer + per-row provenance', async () => {
    const doc = await openSmalltalk(SRC);
    const editor = await vscode.window.showTextDocument(doc);
    // Place the cursor on the `greet` send inside Stage>>run:.
    const pos = positionOfNth(doc, SRC, 'greet', 2);
    editor.selection = new vscode.Selection(pos, pos);
    const result = await waitFor(
      () => vscode.commands.executeCommand('smalltalk.implementorsOf'),
      (r) => r && Array.isArray(r.rows) && r.rows.length >= 2,
    );
    assert.ok(result, 'the command returns a structured result');
    assert.match(result.title, /Implementors of #greet/, 'header names the query');
    assert.ok(result.disclaimer && result.disclaimer.length > 0, 'header carries the union/uncertainty disclaimer (AC2)');
    assert.ok(result.rows.length >= 2, 'both implementors are listed (union)');
    assert.ok(result.rows.every((row) => typeof row.provenance === 'string' && row.provenance.length > 0), 'every row has provenance (AC2)');
    assert.ok(result.rows.some((row) => row.provenance === 'workspace'), 'workspace implementors are tagged workspace');
  });

  test('AC4: call hierarchy incoming calls = senders of the method', async () => {
    const doc = await openSmalltalk(SRC);
    // Prepare on the `greet` implementor in Speaker.
    const pos = positionOfNth(doc, SRC, 'greet', 0);
    const items = await waitFor(
      () => vscode.commands.executeCommand('vscode.prepareCallHierarchy', doc.uri, pos),
      (it) => Array.isArray(it) && it.length >= 1,
    );
    assert.ok(Array.isArray(items) && items.length >= 1, 'a call-hierarchy item is prepared on the method');
    const incoming = await vscode.commands.executeCommand('vscode.provideIncomingCalls', items[0]);
    assert.ok(Array.isArray(incoming) && incoming.length >= 1, 'Stage>>run: is an incoming caller (sender) of greet');
  });
});
