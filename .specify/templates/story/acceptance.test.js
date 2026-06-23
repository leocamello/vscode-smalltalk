// Acceptance harness for {{US_ID}} — {{TITLE}}.
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

suite('{{US_ID}} acceptance (e2e)', () => {
  suiteSetup(async () => {
    const ext = vscode.extensions.getExtension('leocamello.vscode-smalltalk');
    assert.ok(ext, 'extension should be present');
    await waitFor(() => Promise.resolve(ext.isActive), (active) => active === true);
    assert.ok(ext.isActive, 'extension must be active');
  });

  // One pending test per user-observable AC. Replace each TODO with a red
  // assertion derived from spec.md §4, e.g.:
  //
  //   test('AC1: kernel selector is offered after a receiver', async () => {
  //     const doc = await openSmalltalk('x print');
  //     const pos = new vscode.Position(0, 'x print'.length);
  //     const result = await waitFor(
  //       () => vscode.commands.executeCommand('vscode.executeCompletionItemProvider', doc.uri, pos),
  //       (r) => (r?.items ?? r ?? []).some((i) => i.label === 'printString'),
  //     );
  //     assert.ok((result?.items ?? result ?? []).some((i) => i.label === 'printString'));
  //   });
  test('AC1: <observable behaviour from spec.md §4>');
  // test('AC2: …');
});
