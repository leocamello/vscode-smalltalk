// Acceptance harness for US-422 — Semantic Tokens (Cartridge-Aware).
//
// TDD e2e: written BEFORE implementation. Each user-observable acceptance
// criterion from spec.md §4 gets a test that drives the real
// `textDocument/semanticTokens` provider via
// `vscode.executeDocumentSemanticTokensProvider` and asserts on the decoded
// token stream. RED until the provider ships. Run: npm run test:e2e
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

/** Decode an LSP SemanticTokens `data` buffer + legend into absolute tokens:
 *  [{ line, char, length, type, modifiers: string[] }]. */
function decode(legend, tokens) {
  const data = (tokens && tokens.data) || [];
  const out = [];
  let line = 0;
  let char = 0;
  for (let i = 0; i + 4 < data.length; i += 5) {
    const dl = data[i];
    const dc = data[i + 1];
    line += dl;
    char = dl === 0 ? char + dc : dc;
    const modBits = data[i + 4];
    const modifiers = legend.tokenModifiers.filter((_, b) => (modBits & (1 << b)) !== 0);
    out.push({ line, char, length: data[i + 2], type: legend.tokenTypes[data[i + 3]], modifiers });
  }
  return out;
}

/** Fetch + decode semantic tokens for `uri`, polling until `ok(decoded)`. */
async function semanticTokens(uri, ok) {
  const legend = await vscode.commands.executeCommand(
    'vscode.provideDocumentSemanticTokensLegend',
    uri,
  );
  return waitFor(
    async () => {
      const raw = await vscode.commands.executeCommand(
        'vscode.provideDocumentSemanticTokens',
        uri,
      );
      return raw && legend ? decode(legend, raw) : [];
    },
    (decoded) => ok(decoded),
  );
}

/** The token covering the n-th occurrence of `needle` in `text`. */
function tokenForNth(decoded, doc, text, needle, occurrence = 0) {
  let from = -1;
  for (let i = 0; i <= occurrence; i++) {
    from = text.indexOf(needle, from + 1);
    if (from < 0) return undefined;
  }
  const pos = doc.positionAt(from);
  return decoded.find(
    (t) => t.line === pos.line && t.char <= pos.character && pos.character < t.char + t.length,
  );
}

suite('US-422 semantic tokens (e2e)', () => {
  suiteSetup(async () => {
    const ext = vscode.extensions.getExtension('leocamello.vscode-smalltalk');
    assert.ok(ext, 'extension should be present');
    await waitFor(() => Promise.resolve(ext.isActive), (active) => active === true);
    assert.ok(ext.isActive, 'extension must be active');
  });

  test('AC2: a kernel class is classified as a class (the offline cartridge consumer)', async () => {
    const text = 'OrderedCollection new';
    const doc = await openSmalltalk(text);
    const decoded = await semanticTokens(doc.uri, (d) =>
      d.some((t) => t.type === 'class'),
    );
    const tok = tokenForNth(decoded, doc, text, 'OrderedCollection');
    assert.ok(tok, 'the class reference is tokenized');
    assert.equal(tok.type, 'class', 'OrderedCollection is colored as a class with no gst (AC2)');
  });

  test('AC1/AC3: roles + selector parts + pseudo-variables are classified', async () => {
    const text = 'Object subclass: Foo [ | count | at: x put: y [ ^self ] ]';
    const doc = await openSmalltalk(text);
    const decoded = await semanticTokens(doc.uri, (d) =>
      d.some((t) => t.type === 'keyword'),
    );

    const ivar = tokenForNth(decoded, doc, text, 'count');
    assert.ok(ivar, 'the instance variable declaration is tokenized');
    assert.equal(ivar.type, 'property', 'an instance variable is a property (AC1)');

    const at = tokenForNth(decoded, doc, text, 'at:');
    assert.ok(at, 'the keyword selector part is tokenized');
    assert.equal(at.type, 'method', 'keyword-message part is a method (AC3)');

    const self = tokenForNth(decoded, doc, text, 'self');
    assert.ok(self, 'the pseudo-variable is tokenized');
    assert.equal(self.type, 'keyword', 'self is a pseudo-variable / keyword (AC3)');
  });
});
