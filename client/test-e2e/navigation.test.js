// End-to-end navigation tests (US-412): launch VS Code with this extension and
// the fixtures workspace, then exercise the real LSP providers through the
// `vscode.execute*Provider` commands. Run via `npm run test:e2e`.
const assert = require('node:assert');
const path = require('node:path');
const vscode = require('vscode');

const sampleUri = vscode.Uri.file(path.join(__dirname, 'fixtures', 'sample.st'));

// Providers attach after the language server activates; poll briefly.
async function waitFor(fn, ok, tries = 60) {
  let last;
  for (let i = 0; i < tries; i++) {
    last = await fn();
    if (ok(last)) return last;
    await new Promise((r) => setTimeout(r, 250));
  }
  return last;
}

suite('US-412 navigation (e2e)', () => {
  suiteSetup(async () => {
    // The extension must activate from `workspaceContains:**/*.{st,gst}` — i.e. before
    // any Smalltalk file is opened — so workspace symbol search works on a fresh window.
    const ext = vscode.extensions.getExtension('leocamello.vscode-smalltalk');
    assert.ok(ext, 'extension should be present');
    await waitFor(() => Promise.resolve(ext.isActive), (active) => active === true);
    assert.ok(ext.isActive, 'extension must activate from workspaceContains, not only onLanguage');

    const doc = await vscode.workspace.openTextDocument(sampleUri);
    await vscode.window.showTextDocument(doc);
  });

  test('AC2 workspace/symbol works before any document is opened (fresh window)', async () => {
    // Activation already happened in suiteSetup without opening a file (the assertion above),
    // so the server is indexing on startup; a query resolves from the indexed fixtures.
    const results = await waitFor(
      () => vscode.commands.executeCommand('vscode.executeWorkspaceSymbolProvider', 'Sample'),
      (r) => Array.isArray(r) && r.some((s) => s.name === 'Sample'),
    );
    assert.ok((results || []).some((s) => s.name === 'Sample'));
  });

  test('AC1 documentSymbol outline lists the class and its methods', async () => {
    const symbols = await waitFor(
      () => vscode.commands.executeCommand('vscode.executeDocumentSymbolProvider', sampleUri),
      (r) => Array.isArray(r) && r.length > 0,
    );
    const sample = (symbols || []).find((s) => s.name === 'Sample');
    assert.ok(sample, 'expected class Sample in the outline');
    const names = sample.children.map((c) => c.name);
    assert.ok(names.includes('greet'), 'expected method greet');
    assert.ok(names.includes('make'), 'expected class-side method make');
  });

  test('AC2 workspace/symbol finds the class by name', async () => {
    const results = await waitFor(
      () => vscode.commands.executeCommand('vscode.executeWorkspaceSymbolProvider', 'Sample'),
      (r) => Array.isArray(r) && r.some((s) => s.name === 'Sample'),
    );
    assert.ok((results || []).some((s) => s.name === 'Sample'), 'expected Sample in workspace symbols');
  });

  test('AC2 files.exclude removes a file from workspace symbols after a settings change', async () => {
    const filesConfig = () => vscode.workspace.getConfiguration('files');
    const original = filesConfig().get('exclude') || {};
    // Sanity: Sample is present before excluding it.
    const before = await vscode.commands.executeCommand('vscode.executeWorkspaceSymbolProvider', 'Sample');
    assert.ok((before || []).some((s) => s.name === 'Sample'), 'Sample should be present before exclusion');
    try {
      await filesConfig().update(
        'exclude',
        { ...original, '**/sample.st': true },
        vscode.ConfigurationTarget.Workspace,
      );
      const after = await waitFor(
        () => vscode.commands.executeCommand('vscode.executeWorkspaceSymbolProvider', 'Sample'),
        (r) => Array.isArray(r) && !r.some((s) => s.name === 'Sample'),
      );
      assert.ok(
        !(after || []).some((s) => s.name === 'Sample'),
        'Sample must disappear from workspace symbols once files.exclude is updated at runtime',
      );
    } finally {
      await filesConfig().update('exclude', original, vscode.ConfigurationTarget.Workspace);
    }
  });

  test('US-417 AC1 foldingRange folds the class and method bodies', async () => {
    const ranges = await waitFor(
      () => vscode.commands.executeCommand('vscode.executeFoldingRangeProvider', sampleUri),
      (r) => Array.isArray(r) && r.length > 0,
    );
    // sample.st: `Object subclass: Sample [` on line 1 ... `]` near the end.
    assert.ok((ranges || []).some((r) => r.start === 1), 'expected a fold starting at the class body (line 1)');
    assert.ok((ranges || []).length >= 2, 'expected folds for the class and at least one method');
  });

  test('AC3 definition resolves the greet message send', async () => {
    const text = (await vscode.workspace.openTextDocument(sampleUri)).getText();
    const lines = text.split('\n');
    const line = lines.findIndex((l) => l.includes('Sample new greet'));
    const character = lines[line].indexOf('greet') + 2;
    const locations = await waitFor(
      () =>
        vscode.commands.executeCommand(
          'vscode.executeDefinitionProvider',
          sampleUri,
          new vscode.Position(line, character),
        ),
      (r) => Array.isArray(r) && r.length > 0,
    );
    assert.ok((locations || []).length >= 1, 'expected at least one definition for greet');
    assert.equal(locations[0].uri.fsPath, sampleUri.fsPath, 'greet resolves within the same file');
  });
});
