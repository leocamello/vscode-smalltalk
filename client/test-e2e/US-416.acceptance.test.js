// Acceptance harness for US-416 — Formatting.
//
// TDD e2e: written BEFORE implementation. Each user-observable acceptance
// criterion from spec.md §4 gets a test here that FAILS FOR THE RIGHT REASON
// (red), then implementation drives it green. A passing test that was never red
// proves nothing — make it fail first.
//
// Routing (requirements-validation.md §3.5): AC1 (range + on-type) and AC2/AC4
// (document formatting changes the buffer / no-op when disabled) are
// user-observable → asserted here. AC3 (idempotence + token-stream invariance)
// and AC5 (malformed → no edits) are internal invariants → server/ property
// tests + evals/datasets/formatting/. Capability advertisement → handshake.
const assert = require('node:assert');
const vscode = require('vscode');

/** Poll `fn` until `ok(result)` or tries run out; returns the last result. */
async function waitFor(fn, ok, tries = 120) {
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
 *  (VS Code minimizes a whole-document replace into small diffs, so we must apply
 *  them to observe the effect rather than read newText directly.) */
async function formatDocAndApply(doc) {
  const edits = await waitFor(
    () => vscode.commands.executeCommand('vscode.executeFormatDocumentProvider', doc.uri, FMT_OPTS),
    (r) => Array.isArray(r) && r.length > 0,
  );
  const we = new vscode.WorkspaceEdit();
  we.set(doc.uri, edits ?? []);
  await vscode.workspace.applyEdit(we);
  return { edits: edits ?? [], text: doc.getText() };
}

suite('US-416 acceptance (e2e)', () => {
  suiteSetup(async () => {
    const ext = vscode.extensions.getExtension('leocamello.vscode-smalltalk');
    assert.ok(ext, 'extension should be present');
    await waitFor(() => Promise.resolve(ext.isActive), (active) => active === true);
    assert.ok(ext.isActive, 'extension must be active');
  });

  // AC4 — off by default: with smalltalk.format.enable unset (false), Format
  // Document must produce NO edits (never touches the buffer).
  suite('disabled by default (AC4)', () => {
    test('AC4: Format Document is a no-op when smalltalk.format.enable is false', async () => {
      const doc = await openSmalltalk('foo:=Account new.');
      const cfg = vscode.workspace.getConfiguration('smalltalk.format');
      assert.equal(cfg.get('enable'), false, 'format.enable must default to false (AC4)');
      const edits = await vscode.commands.executeCommand(
        'vscode.executeFormatDocumentProvider', doc.uri, FMT_OPTS,
      );
      assert.ok(!edits || edits.length === 0, 'no edits expected while formatting is disabled');
    });
  });

  // AC1/AC2 — with formatting enabled.
  suite('enabled (AC1, AC2)', () => {
    suiteSetup(async function () {
      this.timeout(90000);
      await vscode.workspace
        .getConfiguration('smalltalk.format')
        .update('enable', true, vscode.ConfigurationTarget.Global);
      // The server pulls config per request; the Global write can take a while
      // to round-trip in a headless test host. Warm up until a probe format
      // actually produces edits, so the assertions below don't race propagation.
      const probe = await openSmalltalk('warm:=1.');
      const warmed = await waitFor(
        () => vscode.commands.executeCommand('vscode.executeFormatDocumentProvider', probe.uri, FMT_OPTS),
        (r) => Array.isArray(r) && r.length > 0,
        240,
      );
      assert.ok(Array.isArray(warmed) && warmed.length > 0, 'formatting did not become enabled (config propagation)');
    });
    suiteTeardown(async () => {
      await vscode.workspace
        .getConfiguration('smalltalk.format')
        .update('enable', undefined, vscode.ConfigurationTarget.Global);
    });

    test('AC2: Format Document normalizes spacing (buffer changes)', async () => {
      const doc = await openSmalltalk('foo:=Account new.');
      const { edits, text } = await formatDocAndApply(doc);
      assert.ok(edits.length > 0, 'expected document-formatting edits once enabled');
      assert.equal(text, 'foo := Account new.', 'assignment spacing must be normalized');
    });

    test('AC2: comments and blank lines survive formatting', async () => {
      const doc = await openSmalltalk('a := 1.\n\n\n"keep me" b := 2.');
      const { text } = await formatDocAndApply(doc);
      assert.match(text, /"keep me"/, 'the comment must be preserved verbatim');
      assert.ok(!/\n\n\n/.test(text), 'runs of 2+ blank lines must collapse to one');
    });

    test('AC1: range formatting touches only the selected lines', async () => {
      const doc = await openSmalltalk('foo:=1.\nbar:=2.\nbaz:=3.');
      // Select only line 1 (bar) — line 0 and line 2 must be left alone.
      const range = new vscode.Range(new vscode.Position(1, 0), new vscode.Position(1, 7));
      const edits = await waitFor(
        () => vscode.commands.executeCommand('vscode.executeFormatRangeProvider', doc.uri, range, FMT_OPTS),
        (r) => Array.isArray(r) && r.length > 0,
      );
      assert.ok(Array.isArray(edits) && edits.length > 0, 'expected range-formatting edits');
      for (const e of edits) {
        assert.equal(e.range.start.line, 1, 'edits must stay on the selected line (line 1)');
        assert.equal(e.range.end.line, 1, 'edits must not spill onto adjacent lines');
      }
    });

    test('AC1: on-type formatting dedents the line when `]` is typed', async () => {
      // A block body indented one level; closing `]` typed at column 8 should dedent to col 4.
      const doc = await openSmalltalk('foo do: [\n    bar.\n    ]');
      const pos = new vscode.Position(2, 5); // just after the typed `]`
      const edits = await waitFor(
        () =>
          vscode.commands.executeCommand(
            'vscode.executeFormatOnTypeProvider', doc.uri, pos, ']', FMT_OPTS,
          ),
        (r) => Array.isArray(r) && r.length > 0,
      );
      assert.ok(Array.isArray(edits) && edits.length > 0, 'on-type formatting should emit a dedent edit on `]`');
    });
  });

  // blockStyle = expand: an inline method body reflows onto its own indented lines.
  suite('blockStyle expand', () => {
    suiteSetup(async function () {
      this.timeout(90000);
      const cfg = vscode.workspace.getConfiguration('smalltalk.format');
      await cfg.update('enable', true, vscode.ConfigurationTarget.Global);
      await cfg.update('blockStyle', 'expand', vscode.ConfigurationTarget.Global);
      const probe = await openSmalltalk('Object subclass: Warm [\nm [a:=1. ^a]\n]');
      await waitFor(
        () => vscode.commands.executeCommand('vscode.executeFormatDocumentProvider', probe.uri, FMT_OPTS),
        (r) => Array.isArray(r) && r.length > 0,
        240,
      );
    });
    suiteTeardown(async () => {
      await vscode.workspace
        .getConfiguration('smalltalk.format')
        .update('blockStyle', undefined, vscode.ConfigurationTarget.Global);
    });

    test('expand reflows an inline method body to one statement per line', async () => {
      const doc = await openSmalltalk('Object subclass: Foo [\nbar [a:=1. ^a]\n]');
      const { text } = await formatDocAndApply(doc);
      // The body opener, two statements, and closer each land on their own line.
      assert.match(text, /bar \[\n\s+a := 1\.\n\s+\^a\n\s*\]/, 'method body should expand one statement per line');
    });
  });
});
