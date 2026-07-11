// Acceptance harness for US-902 — Product Polish & Open VSX.
//
// TDD e2e: written BEFORE implementation. Each user-observable acceptance
// criterion from spec.md §4 gets a test here that FAILS FOR THE RIGHT REASON
// (red), then implementation drives it green. A passing test that was never red
// proves nothing — make it fail first.
//
// Routing rule — not every AC is e2e (requirements-validation.md §3.5):
//   • AC2 formatting graduated to always-on  → user-observable, asserted HERE
//   • AC1/AC3/AC4/AC5 manifest facts          → server/test/manifest.test.ts
//   • AC6 cancellation contract               → server/test/cancellation.test.ts
//   • AC4 trust-gate decision                 → client/test/trustGate.test.ts (+ manual QA for the UX)
//   • AC7/AC8/AC9 CI/docs                      → static / review
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

const FMT_OPTS = { tabSize: 4, insertSpaces: true };

/** Run document formatting, apply the returned edits, and return the new buffer text.
 *  (VS Code minimizes a whole-document replace into small diffs, so we apply them to
 *  observe the effect rather than read newText directly — same pattern as US-416.) */
async function formatDocAndApply(doc) {
  const edits = await waitFor(
    () => vscode.commands.executeCommand('vscode.executeFormatDocumentProvider', doc.uri, FMT_OPTS),
    (r) => Array.isArray(r) && r.length > 0,
    240,
  );
  const we = new vscode.WorkspaceEdit();
  we.set(doc.uri, edits ?? []);
  await vscode.workspace.applyEdit(we);
  return { edits: edits ?? [], text: doc.getText() };
}

suite('US-902 acceptance (e2e)', () => {
  suiteSetup(async () => {
    const ext = vscode.extensions.getExtension('leocamello.vscode-smalltalk');
    assert.ok(ext, 'extension should be present');
    await waitFor(() => Promise.resolve(ext.isActive), (active) => active === true);
    assert.ok(ext.isActive, 'extension must be active');
  });

  // AC2 — formatting graduated to always-on. With NO `smalltalk.format.enable`
  // anywhere (the setting is removed), Format Document must still normalize a
  // loosely-spaced buffer. This is the inverse of US-416's off-by-default check:
  // pre-implementation it is RED (formatting is gated off by default and returns
  // no edits); once the gate is removed it is GREEN.
  suite('formatting is always available (AC2)', () => {
    suiteSetup(async () => {
      // Ensure no leftover Global value from a prior US-416 run masks the default.
      await vscode.workspace
        .getConfiguration('smalltalk.format')
        .update('enable', undefined, vscode.ConfigurationTarget.Global);
    });

    test('AC2: Format Document normalizes spacing with no format.enable set', async function () {
      this.timeout(90000);
      const doc = await openSmalltalk('foo:=Account new.');
      const { edits, text } = await formatDocAndApply(doc);
      assert.ok(edits.length > 0, 'formatting must produce edits without any enable flag');
      assert.equal(text, 'foo := Account new.', 'assignment spacing must be normalized');
    });

    test('AC2: formatting is idempotent (a second pass is a no-op)', async function () {
      this.timeout(90000);
      const doc = await openSmalltalk('a:=1.\n\n\nb:=2.');
      await formatDocAndApply(doc); // first pass normalizes
      const again = await vscode.commands.executeCommand(
        'vscode.executeFormatDocumentProvider', doc.uri, FMT_OPTS,
      );
      assert.ok(!again || again.length === 0, 'a formatted document must not change on re-format');
    });
  });
});
