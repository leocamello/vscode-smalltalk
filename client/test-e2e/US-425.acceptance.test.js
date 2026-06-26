// Acceptance harness for US-425 — Signature help.
//
// TDD e2e: written BEFORE implementation. The user-observable AC from spec.md §4
// (the keyword-message signature popup with active-parameter tracking) is driven
// here through VS Code's real `executeSignatureHelpProvider`, over the bundled
// kernel cartridge — no gst (AC2).
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

/** Signature help at `pos`, polling until `ok(help)`. */
async function signatureHelp(uri, pos, ok) {
  return waitFor(
    () => vscode.commands.executeCommand('vscode.executeSignatureHelpProvider', uri, pos),
    (help) => ok(help ?? { signatures: [] }),
  );
}

suite('US-425 signature help (e2e)', () => {
  suiteSetup(async () => {
    const ext = vscode.extensions.getExtension('leocamello.vscode-smalltalk');
    assert.ok(ext, 'extension should be present');
    await waitFor(() => Promise.resolve(ext.isActive), (active) => active === true);
    assert.ok(ext.isActive, 'extension must be active');
  });

  test('AC1/AC2: keyword send pops signature help with the active parameter tracked (no gst)', async () => {
    const text = 'x at: 1 put: ';
    const doc = await openSmalltalk(text);
    const pos = new vscode.Position(0, text.length); // after `put: `
    const help = await signatureHelp(doc.uri, pos, (h) => (h.signatures ?? []).length > 0);
    assert.ok(help && help.signatures.length > 0, 'a keyword send yields signature help');
    assert.equal(help.activeParameter, 1, 'the second keyword (put:) is the active parameter');
    assert.ok(
      help.signatures.some((s) => /at:.*put:/.test(s.label)),
      'the at:put: kernel signature is offered (from the bundled cartridge)',
    );
  });

  test('AC1: a non-keyword (unary) cursor offers no signature help', async () => {
    const text = 'x printString ';
    const doc = await openSmalltalk(text);
    const pos = new vscode.Position(0, text.length);
    // Give the server a beat, then assert the popup stays empty.
    const help = await waitFor(
      () => vscode.commands.executeCommand('vscode.executeSignatureHelpProvider', doc.uri, pos),
      () => true,
      4,
    );
    assert.ok(!help || (help.signatures ?? []).length === 0, 'unary send → no signature help');
  });
});
