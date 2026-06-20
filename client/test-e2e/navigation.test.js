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
    const doc = await vscode.workspace.openTextDocument(sampleUri);
    await vscode.window.showTextDocument(doc);
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
