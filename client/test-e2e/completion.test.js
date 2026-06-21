// End-to-end completion tests (US-413 slice C): launch VS Code with this
// extension and exercise the real `textDocument/completion` provider through
// `vscode.executeCompletionItemProvider`. Kernel completions are available with
// no workspace file (bundled, or installed when gst is present), so these assert
// stable kernel symbols + workspace symbols. Run via `npm run test:e2e`.
const assert = require('node:assert');
const vscode = require('vscode');

async function waitFor(fn, ok, tries = 60) {
  let last;
  for (let i = 0; i < tries; i++) {
    last = await fn();
    if (ok(last)) return last;
    await new Promise((r) => setTimeout(r, 250));
  }
  return last;
}

/** Open an in-memory Smalltalk doc and return its uri after the server attaches. */
async function openSmalltalk(content) {
  const doc = await vscode.workspace.openTextDocument({ language: 'smalltalk', content });
  await vscode.window.showTextDocument(doc);
  return doc;
}

const items = (list) => (Array.isArray(list) ? list : (list && list.items) || []);

suite('US-413 completion (e2e)', () => {
  suiteSetup(async () => {
    const ext = vscode.extensions.getExtension('leocamello.vscode-smalltalk');
    assert.ok(ext, 'extension should be present');
    await waitFor(() => Promise.resolve(ext.isActive), (active) => active === true);
    assert.ok(ext.isActive, 'extension must be active');
  });

  test('AC2 selector context offers a kernel selector after a receiver', async () => {
    const doc = await openSmalltalk('x print');
    const position = new vscode.Position(0, 'x print'.length); // after `print`
    const result = await waitFor(
      () => vscode.commands.executeCommand('vscode.executeCompletionItemProvider', doc.uri, position),
      (r) => items(r).some((i) => label(i) === 'printString'),
    );
    const printString = items(result).find((i) => label(i) === 'printString');
    assert.ok(printString, 'expected the kernel selector printString in selector context');
  });

  test('AC4 keyword selector inserts as a snippet', async () => {
    const doc = await openSmalltalk('x at');
    const position = new vscode.Position(0, 'x at'.length);
    const result = await waitFor(
      () => vscode.commands.executeCommand('vscode.executeCompletionItemProvider', doc.uri, position),
      (r) => items(r).some((i) => label(i) === 'at:put:'),
    );
    const atput = items(result).find((i) => label(i) === 'at:put:');
    assert.ok(atput, 'expected at:put: among completions');
    // A Snippet insert is mapped to a vscode.SnippetString (has `.value`).
    const insert = atput.insertText;
    const text = insert && insert.value !== undefined ? insert.value : insert;
    assert.ok(typeof text === 'string' && text.includes('${1}'), 'at:put: inserts as a snippet with tab stops');
  });

  test('AC3 head context offers a kernel class name', async () => {
    const doc = await openSmalltalk('Obj');
    const position = new vscode.Position(0, 'Obj'.length);
    const result = await waitFor(
      () => vscode.commands.executeCommand('vscode.executeCompletionItemProvider', doc.uri, position),
      (r) => items(r).some((i) => label(i) === 'Object'),
    );
    assert.ok(items(result).some((i) => label(i) === 'Object'), 'expected the kernel class Object in head context');
  });
});

suite('US-413 kernel library setting (e2e)', () => {
  const cfg = () => vscode.workspace.getConfiguration('smalltalk.completion');

  test('AC5 default kernelLibrary is auto', () => {
    assert.strictEqual(cfg().get('kernelLibrary'), 'auto');
  });

  test('AC5 off suppresses kernel completions; bundled restores them', async () => {
    const doc = await openSmalltalk('x print');
    const position = new vscode.Position(0, 'x print'.length);
    const original = cfg().inspect('kernelLibrary').workspaceValue;
    try {
      await cfg().update('kernelLibrary', 'off', vscode.ConfigurationTarget.Workspace);
      const off = await waitFor(
        () => vscode.commands.executeCommand('vscode.executeCompletionItemProvider', doc.uri, position),
        (r) => !items(r).some((i) => label(i) === 'printString'),
      );
      assert.ok(!items(off).some((i) => label(i) === 'printString'), 'off should suppress kernel selectors');

      await cfg().update('kernelLibrary', 'bundled', vscode.ConfigurationTarget.Workspace);
      const bundled = await waitFor(
        () => vscode.commands.executeCommand('vscode.executeCompletionItemProvider', doc.uri, position),
        (r) => items(r).some((i) => label(i) === 'printString'),
      );
      assert.ok(items(bundled).some((i) => label(i) === 'printString'), 'bundled should provide kernel selectors');
    } finally {
      await cfg().update('kernelLibrary', original, vscode.ConfigurationTarget.Workspace);
    }
  });
});

function label(item) {
  return typeof item.label === 'string' ? item.label : item.label && item.label.label;
}
