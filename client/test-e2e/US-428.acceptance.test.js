// Acceptance harness for US-428 — Class Rename.
//
// TDD e2e: written BEFORE implementation. Each user-observable acceptance
// criterion from spec.md §4 gets a test that FAILS FOR THE RIGHT REASON (red),
// then implementation drives it green.
//
// Class rename is an EDIT provider: we apply the returned WorkspaceEdit and
// assert the resulting BUFFER (VS Code minimizes whole-doc replaces, so reading
// `newText` off the edit is unreliable — memory: e2e-edit-provider-gotchas).
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

suite('US-428 acceptance (e2e)', () => {
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

  // A single doc that carries every in-file reference form + a shadowing local.
  const SRC = [
    'Object subclass: Shape [',
    '  parts [ ^OrderedCollection new ] ]',
    'Shape subclass: Circle [ ]',
    'Object subclass: Painter [',
    '  a [ ^Shape new ]',
    '  b [ ^#{Shape} new ]',
    '  c [ | Shape | ^Shape ] ]',
  ].join('\n');

  // AC2: renaming a class rewrites every resolved form and nothing else.
  test('AC2: rename a class updates declaration, receiver, superclass, and #{…}', async () => {
    const doc = await openSmalltalk(SRC);
    // The server must first index the doc's class before it is renameable.
    const edit = await waitFor(
      () => renameEdit(doc.uri, posOf(doc, 'Shape', 1), 'Polygon'),
      (e) => e && e.size > 0,
    );
    assert.ok(edit && edit.size > 0, 'expected a class-rename WorkspaceEdit');
    await vscode.workspace.applyEdit(edit);
    const out = doc.getText();
    assert.match(out, /Object subclass: Polygon \[/, 'declaration renamed');
    assert.match(out, /Polygon subclass: Circle \[/, 'superclass position renamed');
    assert.match(out, /a \[ \^Polygon new \]/, 'receiver send renamed');
    assert.match(out, /b \[ \^#\{Polygon\} new \]/, 'binding-constant class segment renamed');
    assert.match(out, /c \[ \| Shape \| \^Shape \]/, 'shadowing local Shape untouched (AC3)');
    assert.match(out, /OrderedCollection new/, 'kernel class untouched');
    assert.match(out, /subclass: Circle/, 'the new class Circle untouched');
  });

  // AC1: a kernel class cannot be renamed.
  test('AC1: renaming a kernel class is rejected (no edit)', async () => {
    const doc = await openSmalltalk(SRC);
    let threw = false;
    let edit;
    try {
      edit = await renameEdit(doc.uri, posOf(doc, 'OrderedCollection'), 'MyColl');
    } catch {
      threw = true; // prepareRename rejection surfaces as a thrown error
    }
    assert.ok(threw || !edit || edit.size === 0, 'a kernel-class rename must produce no edit');
  });

  // AC4: renaming into an existing class name is refused.
  test('AC4: renaming a class into an existing class name is refused', async () => {
    const doc = await openSmalltalk(SRC);
    let threw = false;
    let edit;
    try {
      edit = await waitFor(
        () => renameEdit(doc.uri, posOf(doc, 'Shape', 1), 'Circle'),
        (e) => e !== undefined,
      );
    } catch {
      threw = true;
    }
    assert.ok(threw || !edit || edit.size === 0, 'a colliding class rename must produce no edit');
  });
});
