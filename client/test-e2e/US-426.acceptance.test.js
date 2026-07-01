// Acceptance harness for US-426 — Scope-aware Rename.
//
// TDD e2e: written BEFORE implementation. Each user-observable acceptance
// criterion from spec.md §4 gets a test here that FAILS FOR THE RIGHT REASON
// (red), then implementation drives it green. A passing test that was never red
// proves nothing — make it fail first.
//
// Routing rule — not every AC is e2e:
//   • user-observable (completion appears, status bar reads X, folding folds) → assert HERE
//   • internal contract / data invariant                                      → server/ unit or evals/ golden
//   • LSP protocol shape                                                       → server/test handshake
// If this story has NO user-observable surface, delete this file and record the
// routing in requirements-validation.md §3.5.
//
// The entries below are *pending* tests (no callback) — Mocha's TODO slot, so CI
// stays green until you write the assertion. In the Acceptance Harness phase:
// give each a body that drives a real VS Code command and asserts the expected
// behaviour, watch it go red, then implement to green. Run: npm run test:e2e
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

suite('US-426 acceptance (e2e)', () => {
  suiteSetup(async () => {
    const ext = vscode.extensions.getExtension('leocamello.vscode-smalltalk');
    assert.ok(ext, 'extension should be present');
    await waitFor(() => Promise.resolve(ext.isActive), (active) => active === true);
    assert.ok(ext.isActive, 'extension must be active');
  });

  /** Position just inside the `occurrence`-th appearance of `sub`. */
  function posOf(doc, sub, occurrence = 1) {
    const text = doc.getText();
    let idx = -1;
    for (let i = 0; i < occurrence; i++) idx = text.indexOf(sub, idx + 1);
    return doc.positionAt(idx + 1);
  }
  const renameEdit = (uri, pos, newName) =>
    vscode.commands.executeCommand('vscode.executeDocumentRenameProvider', uri, pos, newName);

  // AC2: renaming a temporary updates exactly its method's occurrences in the buffer.
  test('AC2: rename a temporary updates the buffer', async () => {
    const doc = await openSmalltalk('Object subclass: Foo [\n  bar [ | acc | acc := 1. ^acc ]\n  baz [ | acc | ^acc ] ]');
    const edit = await waitFor(
      () => renameEdit(doc.uri, posOf(doc, 'acc', 2), 'total'),
      (e) => e && e.size > 0,
    );
    assert.ok(edit && edit.size > 0, 'expected a rename WorkspaceEdit');
    await vscode.workspace.applyEdit(edit);
    const out = doc.getText();
    assert.match(out, /bar \[ \| total \| total := 1\. \^total \]/, "bar's acc renamed to total");
    assert.match(out, /baz \[ \| acc \| \^acc \]/, "baz's acc is untouched (scope-bounded)");
  });

  // AC1: a message selector cannot be renamed.
  test('AC1: renaming a selector is rejected (no edit)', async () => {
    const doc = await openSmalltalk('Object subclass: Foo [\n  bar [ ^self frobnicate ] ]');
    let threw = false;
    let edit;
    try {
      edit = await renameEdit(doc.uri, posOf(doc, 'frobnicate'), 'wobble');
    } catch {
      threw = true; // VS Code surfaces the prepareRename rejection as a thrown error
    }
    assert.ok(threw || !edit || edit.size === 0, 'selector rename must produce no edit');
  });

  // AC4: a colliding new name is refused (no edit applied).
  test('AC4: a colliding rename is refused', async () => {
    const doc = await openSmalltalk('Object subclass: Foo [\n  bar [ | acc total | acc := total ] ]');
    let threw = false;
    let edit;
    try {
      edit = await renameEdit(doc.uri, posOf(doc, 'acc', 2), 'total');
    } catch {
      threw = true;
    }
    assert.ok(threw || !edit || edit.size === 0, 'a colliding rename must produce no edit');
  });
});
