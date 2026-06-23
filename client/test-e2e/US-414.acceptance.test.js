// Acceptance harness for US-414 — Diagnostics.
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

suite('US-414 acceptance (e2e)', () => {
  suiteSetup(async () => {
    const ext = vscode.extensions.getExtension('leocamello.vscode-smalltalk');
    assert.ok(ext, 'extension should be present');
    await waitFor(() => Promise.resolve(ext.isActive), (active) => active === true);
    assert.ok(ext.isActive, 'extension must be active');
  });

  // AC1 — parser diagnostics are published on a malformed document, with code
  // `parse` and source `smalltalk` (badge `smalltalk(parse)`), as the server
  // pushes them via textDocument/publishDiagnostics. Routed to e2e because the
  // squiggle is the user-observable surface (the mapping/range is unit+eval).
  test('AC1: malformed source publishes a smalltalk(parse) diagnostic', async () => {
    const doc = await openSmalltalk('Object subclass: Foo [\n  bar [ ^1 \n');
    const diags = await waitFor(
      () => Promise.resolve(vscode.languages.getDiagnostics(doc.uri)),
      (d) => Array.isArray(d) && d.length > 0,
    );
    assert.ok(diags.length > 0, 'expected at least one diagnostic on malformed input');
    const parseDiag = diags.find((d) => d.source === 'smalltalk' && d.code === 'parse');
    assert.ok(parseDiag, 'expected a diagnostic with source "smalltalk" and code "parse"');
    assert.equal(parseDiag.severity, vscode.DiagnosticSeverity.Error);
  });

  // AC1 — a clean document carries no parser diagnostics (and editing a
  // malformed doc to a valid one clears them).
  test('AC1: a clean document publishes no parser diagnostics', async () => {
    const doc = await openSmalltalk('| x |\nx := 1.\nTranscript showCr: x printString.\n');
    // Give the debounced publish a chance to run, then assert none from our source.
    await new Promise((r) => setTimeout(r, 600));
    const ours = vscode.languages
      .getDiagnostics(doc.uri)
      .filter((d) => d.source === 'smalltalk' && d.code === 'parse');
    assert.equal(ours.length, 0, 'expected no parser diagnostics on clean source');
  });
});
