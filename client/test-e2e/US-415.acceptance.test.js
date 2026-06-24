// Acceptance harness for US-415 — Hover.
//
// TDD e2e: written BEFORE implementation. Each user-observable acceptance
// criterion from spec.md §4 gets a test that drives the real
// `textDocument/hover` provider via `vscode.executeHoverProvider` and asserts
// the rendered Markdown. These are RED until the provider ships. Run: npm run test:e2e
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

/** Flatten an executeHoverProvider result to a single Markdown string. */
function hoverText(hovers) {
  const list = Array.isArray(hovers) ? hovers : [];
  let out = '';
  for (const h of list) {
    for (const c of h.contents ?? []) {
      out += typeof c === 'string' ? c : (c.value ?? '');
      out += '\n';
    }
  }
  return out;
}

/** Hover at `position`, polling until the markdown satisfies `ok` (server warm-up). */
async function hoverAt(uri, position, ok) {
  return waitFor(
    () => vscode.commands.executeCommand('vscode.executeHoverProvider', uri, position),
    (r) => ok(hoverText(r)),
  );
}

suite('US-415 hover (e2e)', () => {
  suiteSetup(async () => {
    const ext = vscode.extensions.getExtension('leocamello.vscode-smalltalk');
    assert.ok(ext, 'extension should be present');
    await waitFor(() => Promise.resolve(ext.isActive), (active) => active === true);
    assert.ok(ext.isActive, 'extension must be active');
  });

  test('AC1: hover on a selector shows its signature and an implementor', async () => {
    const doc = await openSmalltalk('x printString');
    const pos = new vscode.Position(0, 'x print'.length); // inside `printString`
    const md = hoverText(await hoverAt(doc.uri, pos, (t) => /printString/.test(t) && /Object/.test(t)));
    assert.match(md, /printString/, 'signature should name the selector');
    assert.match(md, /Object/, 'implementor list should include the kernel class Object');
  });

  test('AC2: hover on a class shows its superclass chain', async () => {
    const doc = await openSmalltalk('OrderedCollection');
    const pos = new vscode.Position(0, 2);
    const md = hoverText(await hoverAt(doc.uri, pos, (t) => /SequenceableCollection/.test(t)));
    assert.match(md, /OrderedCollection/, 'chain should start at the class');
    assert.match(md, /SequenceableCollection/, 'chain should include the immediate superclass');
    assert.match(md, /Object/, 'chain should reach the root Object');
    assert.match(md, /→|->/, 'chain should be rendered as a chain');
  });

  test('AC3: hover on a variable shows its kind', async () => {
    const doc = await openSmalltalk('Object subclass: Foo [ | count | bar [ ^count ] ]');
    const pos = new vscode.Position(0, 'Object subclass: Foo [ | count | bar [ ^co'.length); // on `count` use
    const md = hoverText(await hoverAt(doc.uri, pos, (t) => /instance variable/i.test(t)));
    assert.match(md, /instance variable/i, 'should report the variable kind');
  });

  test('AC4: hover on a numeric literal renders its decoded value', async () => {
    const doc = await openSmalltalk('16rFF');
    const pos = new vscode.Position(0, 2);
    const md = hoverText(await hoverAt(doc.uri, pos, (t) => /255/.test(t)));
    assert.match(md, /255/, 'radix-16 FF should decode to 255');
  });

  test('AC5: hover content is Markdown with a code fence', async () => {
    const doc = await openSmalltalk('x printString');
    const pos = new vscode.Position(0, 'x print'.length);
    const md = hoverText(await hoverAt(doc.uri, pos, (t) => t.includes('```')));
    assert.ok(md.includes('```'), 'hover Markdown should contain a fenced code block');
  });

  // Slice B: workspace comment prose (the user's own source — no licence gate, so
  // this is hermetic; the installed-kernel prose path needs a real gst and is unit-tested).
  test('AC1/AC2: workspace class + method comments surface in hover', async () => {
    const folder = vscode.workspace.workspaceFolders[0].uri;
    const uri = vscode.Uri.joinPath(folder, 'hover.st');
    const doc = await vscode.workspace.openTextDocument(uri);
    await vscode.window.showTextDocument(doc);
    const text = doc.getText();

    // Class hover on the `Widget` reference (last line) shows its <comment:> (AC2).
    const classPos = doc.positionAt(text.lastIndexOf('Widget') + 2);
    const classMd = hoverText(await hoverAt(uri, classPos, (t) => /A widget for the US-415/.test(t)));
    assert.match(classMd, /A widget for the US-415 hover tests\./, 'class comment surfaces (AC2)');

    // Selector hover on the `poke` send shows the method's leading comment (AC1).
    const methodPos = doc.positionAt(text.indexOf('poke', text.indexOf('new poke')) + 1);
    const methodMd = hoverText(await hoverAt(uri, methodPos, (t) => /Poke the widget\./.test(t)));
    assert.match(methodMd, /Poke the widget\./, 'method comment surfaces (AC1)');
  });
});
