// Verifies the bundled server (dist/server.js) speaks LSP: spawn it as a plain
// Node process (no gst, no VS Code) and drive a real session.
//   - initialize/shutdown handshake + advertised capabilities (US-410)
//   - textDocument/documentSymbol returns an outline (US-412 slice A)
//   - workspace/symbol searches an indexed folder (US-412 slice B)
import { spawn } from 'node:child_process';
import { fileURLToPath, pathToFileURL } from 'node:url';
import assert from 'node:assert/strict';
import path from 'node:path';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const serverPath = path.join(root, 'dist', 'server.js');
const fixtureDir = path.join(root, 'docs', 'research', 'gst-syntax', 'test-cases');

const child = spawn(process.execPath, [serverPath, '--stdio'], {
  stdio: ['pipe', 'pipe', 'inherit'],
});
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function send(msg) {
  const json = JSON.stringify(msg);
  child.stdin.write(`Content-Length: ${Buffer.byteLength(json)}\r\n\r\n${json}`);
}
function respond(id, result) {
  send({ jsonrpc: '2.0', id, result });
}

let buffer = Buffer.alloc(0);
const queue = [];
let waiter = null;
// Latest pushed parser diagnostics, by document uri (US-414 slice A). Captured
// out of band so they don't interfere with the request/response (id) flow.
const diagnosticsByUri = new Map();

child.stdout.on('data', (chunk) => {
  buffer = Buffer.concat([buffer, chunk]);
  for (;;) {
    const headerEnd = buffer.indexOf('\r\n\r\n');
    if (headerEnd === -1) break;
    const header = buffer.subarray(0, headerEnd).toString('ascii');
    const match = /Content-Length: (\d+)/i.exec(header);
    const bodyStart = headerEnd + 4;
    const len = match ? Number(match[1]) : 0;
    if (buffer.length < bodyStart + len) break;
    const body = buffer.subarray(bodyStart, bodyStart + len).toString('utf8');
    buffer = buffer.subarray(bodyStart + len);
    const msg = JSON.parse(body);
    // Auto-answer server→client requests (e.g. workspace/configuration) so the
    // server's initialization (and folder indexing) can proceed.
    if (msg.method && msg.id !== undefined) {
      const result = msg.method === 'workspace/configuration'
        ? (msg.params?.items ?? [{}]).map(() => ({ exclude: {} }))
        : null;
      respond(msg.id, result);
      continue;
    }
    // Server→client notification: capture pushed diagnostics, don't let them
    // disturb the id-based receive flow.
    if (msg.method === 'textDocument/publishDiagnostics') {
      diagnosticsByUri.set(msg.params.uri, msg.params.diagnostics ?? []);
      continue;
    }
    if (waiter) {
      const resolve = waiter;
      waiter = null;
      resolve(msg);
    } else {
      queue.push(msg);
    }
  }
});

function receive() {
  return queue.length ? Promise.resolve(queue.shift()) : new Promise((r) => (waiter = r));
}
async function receiveId(id) {
  for (;;) {
    const msg = await receive();
    if (msg.id === id) return msg;
  }
}
function fail(message) {
  console.error(`Server LSP test FAILED: ${message}`);
  child.kill();
  process.exit(1);
}

const timeout = setTimeout(() => fail('timed out waiting for the server'), 15000);

// --- initialize (with a workspace folder so the index can populate) ---
send({
  jsonrpc: '2.0',
  id: 1,
  method: 'initialize',
  params: {
    processId: process.pid,
    rootUri: null,
    workspaceFolders: [{ uri: pathToFileURL(fixtureDir).href, name: 'fixtures' }],
    capabilities: { workspace: { configuration: true, workspaceFolders: true } },
  },
});

const initResult = await receiveId(1);
assert.ok(initResult.result?.capabilities, 'initialize must return capabilities');
const caps = initResult.result.capabilities;
assert.equal(caps.textDocumentSync, 2, 'expected incremental textDocumentSync (2)');
assert.equal(caps.documentSymbolProvider, true, 'expected documentSymbolProvider (US-412)');
assert.equal(caps.workspaceSymbolProvider, true, 'expected workspaceSymbolProvider (US-412)');
assert.equal(caps.definitionProvider, true, 'expected definitionProvider (US-412)');
assert.equal(caps.referencesProvider, true, 'expected referencesProvider (US-423)');
assert.equal(caps.callHierarchyProvider, true, 'expected callHierarchyProvider (US-423)');
assert.equal(caps.foldingRangeProvider, true, 'expected foldingRangeProvider (US-417)');
assert.equal(caps.documentHighlightProvider, true, 'expected documentHighlightProvider (US-417)');
assert.ok(caps.completionProvider, 'expected completionProvider (US-413)');
assert.ok(caps.codeActionProvider, 'expected codeActionProvider (US-414)');
assert.equal(caps.hoverProvider, true, 'expected hoverProvider (US-415)');
assert.ok(caps.signatureHelpProvider, 'expected signatureHelpProvider (US-425)');
assert.ok(
  caps.signatureHelpProvider.triggerCharacters?.includes(':'),
  'signature help triggers on `:` (US-425)',
);
assert.ok(caps.semanticTokensProvider, 'expected semanticTokensProvider (US-422)');
const stLegend = caps.semanticTokensProvider.legend;
assert.ok(Array.isArray(stLegend?.tokenTypes) && stLegend.tokenTypes.length > 0, 'semantic tokens legend advertises token types');
assert.ok(stLegend.tokenTypes.includes('class'), 'legend includes the `class` token type (AC2)');
assert.ok(caps.semanticTokensProvider.full, 'semantic tokens advertises the `full` document request (AC1)');

// Formatting capabilities (US-416) — document + range + on-type advertised.
assert.equal(caps.documentFormattingProvider, true, 'expected documentFormattingProvider (US-416)');
assert.equal(caps.documentRangeFormattingProvider, true, 'expected documentRangeFormattingProvider (US-416)');
assert.ok(caps.documentOnTypeFormattingProvider, 'expected documentOnTypeFormattingProvider (US-416)');
assert.equal(
  caps.documentOnTypeFormattingProvider.firstTriggerCharacter,
  ']',
  'on-type formatting first trigger character is `]` (dedent on block close)',
);

// Scope-aware rename (US-426) — advertised with prepareRename support.
assert.ok(caps.renameProvider, 'expected renameProvider (US-426)');
assert.equal(
  caps.renameProvider.prepareProvider,
  true,
  'rename advertises prepareProvider (prepareRename gates renameable positions)',
);

send({ jsonrpc: '2.0', method: 'initialized', params: {} });

// --- documentSymbol (US-412 slice A) ---
const uri = 'file:///docsymbol-test.st';
send({
  jsonrpc: '2.0',
  method: 'textDocument/didOpen',
  params: {
    textDocument: { uri, languageId: 'smalltalk', version: 1, text: 'Object subclass: Foo [ | a | bar [ ^1 ] ]' },
  },
});
send({ jsonrpc: '2.0', id: 2, method: 'textDocument/documentSymbol', params: { textDocument: { uri } } });
const symResult = await receiveId(2);
assert.ok(Array.isArray(symResult.result), 'documentSymbol must return an array');
const foo = symResult.result.find((s) => s.name === 'Foo');
assert.ok(foo, 'expected class Foo in the document symbols');
assert.equal(foo.kind, 5, 'Foo must be a Class (SymbolKind 5)');
assert.deepEqual(
  (foo.children ?? []).map((s) => s.name).sort(),
  ['a', 'bar'],
  'Foo must contain field `a` and method `bar`',
);

// --- workspace/symbol (US-412 slice B) — poll until the folder index is ready ---
let wsResult = [];
for (let i = 0; i < 25 && wsResult.length === 0; i++) {
  send({ jsonrpc: '2.0', id: 100 + i, method: 'workspace/symbol', params: { query: 'MySimpleClass' } });
  wsResult = (await receiveId(100 + i)).result ?? [];
  if (wsResult.length === 0) await sleep(100);
}
const mySimple = wsResult.find((s) => s.name === 'MySimpleClass');
assert.ok(mySimple, 'workspace/symbol must find MySimpleClass from the indexed fixtures');
assert.equal(mySimple.kind, 5, 'MySimpleClass must be a Class (5)');
assert.match(mySimple.location.uri, /11_/, 'MySimpleClass must resolve to fixture 11');

// --- textDocument/definition (US-412 slice C) — open a doc with a def + a use ---
// In-workspace URI (under the fixture folder): index-backed features only index
// documents that live under a workspace folder (a file opened from outside, e.g.
// a kernel source, must not pollute the workspace tier — US-423).
const defUri = pathToFileURL(path.join(fixtureDir, 'definition-test.st')).href;
send({
  jsonrpc: '2.0',
  method: 'textDocument/didOpen',
  params: {
    textDocument: {
      uri: defUri,
      languageId: 'smalltalk',
      version: 1,
      text: 'Object subclass: Greeter [ greet [ ^1 ] ]\nGreeter new greet',
    },
  },
});
// Cursor on the `greet` send (line 1); poll until the debounced index has the doc.
let defResult = [];
for (let i = 0; i < 25 && defResult.length === 0; i++) {
  send({
    jsonrpc: '2.0',
    id: 200 + i,
    method: 'textDocument/definition',
    params: { textDocument: { uri: defUri }, position: { line: 1, character: 14 } },
  });
  defResult = (await receiveId(200 + i)).result ?? [];
  if (defResult.length === 0) await sleep(100);
}
assert.ok(defResult.length >= 1, 'definition must resolve the `greet` send');
assert.equal(defResult[0].uri, defUri, 'definition resolves within the same file');
assert.equal(defResult[0].range.start.line, 0, '`greet` is defined on line 0');

// --- class rename (US-428) — prepareRename accepts a workspace class, rejects a
// non-renameable class (kernel `Object` / unknown); rename returns a WorkspaceEdit.
// Reuses the open `defUri` doc: `Object subclass: Greeter [ … ]` (Greeter is indexed).
const greeterCol = 'Object subclass: '.length + 1; // inside `Greeter` on line 0
send({
  jsonrpc: '2.0',
  id: 700,
  method: 'textDocument/prepareRename',
  params: { textDocument: { uri: defUri }, position: { line: 0, character: greeterCol } },
});
const prepClass = await receiveId(700);
assert.ok(prepClass.result && prepClass.result.start, 'prepareRename accepts the workspace class Greeter (returns a range)');

send({
  jsonrpc: '2.0',
  id: 701,
  method: 'textDocument/prepareRename',
  params: { textDocument: { uri: defUri }, position: { line: 0, character: 2 } }, // inside `Object`
});
const prepKernel = await receiveId(701);
assert.ok(prepKernel.error, 'prepareRename rejects the kernel/non-workspace class Object (error response)');

send({
  jsonrpc: '2.0',
  id: 702,
  method: 'textDocument/rename',
  params: { textDocument: { uri: defUri }, position: { line: 0, character: greeterCol }, newName: 'Welcomer' },
});
const renClass = await receiveId(702);
assert.ok(renClass.result, 'rename of a workspace class returns a WorkspaceEdit');
const renEdits = renClass.result.changes || renClass.result.documentChanges;
assert.ok(renEdits, 'the class-rename WorkspaceEdit carries edits (changes or documentChanges)');

// --- textDocument/references + plural definition (US-423) ---
// Two classes implement `greet`; a third method sends it. references is the
// union (2 defs + 1 send); definition on the send is plural (both impls).
const xrefUri = pathToFileURL(path.join(fixtureDir, 'xref-test.st')).href;
const xrefText = [
  'Object subclass: Speaker [',
  '  greet [ ^self ]',
  ']',
  'Object subclass: Robot [',
  '  greet [ ^self ]',
  ']',
  'Object subclass: Stage [',
  '  run: who [ ^who greet ]',
  ']',
].join('\n');
send({
  jsonrpc: '2.0',
  method: 'textDocument/didOpen',
  params: { textDocument: { uri: xrefUri, languageId: 'smalltalk', version: 1, text: xrefText } },
});
// Cursor on the `greet` send inside Stage>>run: (line 7). Poll for the debounced index.
const sendLine = 7;
const sendChar = xrefText.split('\n')[sendLine].indexOf('greet');
let refResult = [];
for (let i = 0; i < 25 && refResult.length < 3; i++) {
  send({
    jsonrpc: '2.0',
    id: 300 + i,
    method: 'textDocument/references',
    params: { textDocument: { uri: xrefUri }, position: { line: sendLine, character: sendChar }, context: { includeDeclaration: true } },
  });
  refResult = (await receiveId(300 + i)).result ?? [];
  if (refResult.length < 3) await sleep(100);
}
assert.ok(refResult.length >= 3, `references returns the union of both implementors + the send (got ${refResult.length})`);

// Plural go-to-definition on the same send: both implementors, never collapsed (AC3).
send({
  jsonrpc: '2.0',
  id: 320,
  method: 'textDocument/definition',
  params: { textDocument: { uri: xrefUri }, position: { line: sendLine, character: sendChar } },
});
const xrefDef = (await receiveId(320)).result ?? [];
assert.ok(xrefDef.length >= 2, `definition on a send is plural — both implementors (got ${xrefDef.length})`);

// --- call hierarchy (US-423 AC4) — prepare on Speaker>>greet, then incoming/outgoing ---
const greetDefLine = 1; // `  greet [ ^self ]` inside Speaker
const greetDefChar = xrefText.split('\n')[greetDefLine].indexOf('greet');
let prepItems = [];
for (let i = 0; i < 25 && prepItems.length === 0; i++) {
  send({
    jsonrpc: '2.0',
    id: 340 + i,
    method: 'textDocument/prepareCallHierarchy',
    params: { textDocument: { uri: xrefUri }, position: { line: greetDefLine, character: greetDefChar } },
  });
  prepItems = (await receiveId(340 + i)).result ?? [];
  if (prepItems.length === 0) await sleep(100);
}
assert.ok(prepItems.length >= 1, 'prepareCallHierarchy yields an item on the method definition');
assert.equal(prepItems[0].name, 'Speaker>>greet', 'the prepared item names the method');

send({ jsonrpc: '2.0', id: 360, method: 'callHierarchy/incomingCalls', params: { item: prepItems[0] } });
const incoming = (await receiveId(360)).result ?? [];
assert.ok(incoming.length >= 1, 'incoming calls = senders of greet (Stage>>run:)');
assert.ok(
  incoming.some((c) => c.from.name.includes('run:')),
  'Stage>>run: is an incoming caller of greet',
);

// Outgoing of Stage>>run: — prepare on the method, then ask for its sends.
const runDefLine = 7; // `  run: who [ ^who greet ]`
const runDefChar = xrefText.split('\n')[runDefLine].indexOf('run:');
send({
  jsonrpc: '2.0',
  id: 380,
  method: 'textDocument/prepareCallHierarchy',
  params: { textDocument: { uri: xrefUri }, position: { line: runDefLine, character: runDefChar } },
});
const runItems = (await receiveId(380)).result ?? [];
assert.ok(runItems.length >= 1 && runItems[0].name.includes('run:'), 'prepared Stage>>run:');
send({ jsonrpc: '2.0', id: 381, method: 'callHierarchy/outgoingCalls', params: { item: runItems[0] } });
const outgoing = (await receiveId(381)).result ?? [];
assert.ok(
  outgoing.some((c) => c.to.name === 'greet'),
  'outgoing calls of run: include the send of greet',
);

// --- a document opened from OUTSIDE the workspace must NOT be indexed (US-423) ---
// (Regression: clicking a kernel cross-reference row opens that file; it must not
// then mislabel the kernel's classes as `workspace` and shadow the cartridge.)
// A valid absolute path on every OS that is NOT under the workspace folder
// (the repo root sits above the fixture folder). A literal `file:///tmp/…` would
// be a non-absolute path on Windows and get misread as an untitled buffer.
const outsideUri = pathToFileURL(path.join(root, 'outside-workspace-xyz.st')).href;
send({
  jsonrpc: '2.0',
  method: 'textDocument/didOpen',
  params: {
    textDocument: { uri: outsideUri, languageId: 'smalltalk', version: 1, text: 'Object subclass: OutsiderZZZ [ widgetize [ ^1 ] ]' },
  },
});
let outsideHits = [];
for (let i = 0; i < 8; i++) {
  await sleep(120); // let the debounced index settle
  send({ jsonrpc: '2.0', id: 400 + i, method: 'workspace/symbol', params: { query: 'OutsiderZZZ' } });
  outsideHits = (await receiveId(400 + i)).result ?? [];
}
assert.equal(outsideHits.length, 0, 'a class from a file opened outside the workspace is not indexed as workspace');

// --- textDocument/foldingRange (US-417 slice A) ---
const foldUri = 'file:///folding-test.st';
send({
  jsonrpc: '2.0',
  method: 'textDocument/didOpen',
  params: {
    textDocument: {
      uri: foldUri,
      languageId: 'smalltalk',
      version: 1,
      text: 'Object subclass: Folder [\n    items [\n        ^1\n    ]\n]',
    },
  },
});
send({ jsonrpc: '2.0', id: 4, method: 'textDocument/foldingRange', params: { textDocument: { uri: foldUri } } });
const foldResult = (await receiveId(4)).result ?? [];
assert.ok(
  foldResult.some((r) => r.startLine === 0 && r.endLine === 4),
  'class body should fold lines 0..4',
);
assert.ok(
  foldResult.some((r) => r.startLine === 1 && r.endLine === 3),
  'method body should fold lines 1..3',
);

// --- textDocument/documentHighlight (US-417 slice B) ---
const hlUri = 'file:///highlight-test.st';
send({
  jsonrpc: '2.0',
  method: 'textDocument/didOpen',
  params: { textDocument: { uri: hlUri, languageId: 'smalltalk', version: 1, text: 'a foo. b foo' } },
});
// Cursor on the first `foo` send (char 3); both sends should highlight.
send({
  jsonrpc: '2.0',
  id: 5,
  method: 'textDocument/documentHighlight',
  params: { textDocument: { uri: hlUri }, position: { line: 0, character: 3 } },
});
const hlResult = (await receiveId(5)).result ?? [];
assert.equal(hlResult.length, 2, 'both `foo` sends should be highlighted');

// --- textDocument/completion (US-413 slice C) ---
// Kernel completions (bundled, or installed when gst is present) are available
// without any workspace file; assert a stable kernel selector + class appear.
const cmpUri = 'file:///completion-test.st';
send({
  jsonrpc: '2.0',
  method: 'textDocument/didOpen',
  params: {
    textDocument: { uri: cmpUri, languageId: 'smalltalk', version: 1, text: 'x print' },
  },
});
// Cursor right after `print` (selector context after the receiver `x`).
send({
  jsonrpc: '2.0',
  id: 6,
  method: 'textDocument/completion',
  params: { textDocument: { uri: cmpUri }, position: { line: 0, character: 7 } },
});
const cmpRaw = (await receiveId(6)).result ?? [];
const cmpItems = Array.isArray(cmpRaw) ? cmpRaw : (cmpRaw.items ?? []);
assert.ok(cmpItems.length > 0, 'completion should return kernel selectors after a receiver');
assert.ok(
  cmpItems.some((i) => i.label === 'printString'),
  'expected the kernel selector printString among completions',
);
const printItem = cmpItems.find((i) => i.label === 'printString');
assert.match(printItem.detail ?? '', /kernel/, 'kernel completion should carry kernel provenance in detail');

// Head context: a class name from the kernel.
const cmpHeadUri = 'file:///completion-head-test.st';
send({
  jsonrpc: '2.0',
  method: 'textDocument/didOpen',
  params: { textDocument: { uri: cmpHeadUri, languageId: 'smalltalk', version: 1, text: 'Obj' } },
});
send({
  jsonrpc: '2.0',
  id: 7,
  method: 'textDocument/completion',
  params: { textDocument: { uri: cmpHeadUri }, position: { line: 0, character: 3 } },
});
const cmpHeadRaw = (await receiveId(7)).result ?? [];
const cmpHeadItems = Array.isArray(cmpHeadRaw) ? cmpHeadRaw : (cmpHeadRaw.items ?? []);
assert.ok(
  cmpHeadItems.some((i) => i.label === 'Object'),
  'expected the kernel class Object in head-context completion',
);

// --- textDocument/signatureHelp (US-425) ---
// At a keyword-send cursor, the bundled kernel supplies the matching keyword
// selector(s) with the active parameter tracked — no gst, no workspace file.
const sigUri = 'file:///signature-test.st';
const sigText = 'x at: 1 put: ';
send({
  jsonrpc: '2.0',
  method: 'textDocument/didOpen',
  params: { textDocument: { uri: sigUri, languageId: 'smalltalk', version: 1, text: sigText } },
});
send({
  jsonrpc: '2.0',
  id: 11,
  method: 'textDocument/signatureHelp',
  params: { textDocument: { uri: sigUri }, position: { line: 0, character: sigText.length } },
});
const sigResult = (await receiveId(11)).result;
assert.ok(sigResult?.signatures?.length > 0, 'signature help returns keyword signatures at a keyword send');
assert.equal(sigResult.activeParameter, 1, 'the second keyword (put:) is the active parameter');
assert.ok(
  sigResult.signatures.some((s) => s.selector === 'at:put:'),
  'expected the kernel selector at:put: among the signatures',
);
const atPut = sigResult.signatures.find((s) => s.selector === 'at:put:');
assert.equal(atPut.parameters?.length, 2, 'at:put: exposes one parameter per keyword part');

// A non-keyword cursor (unary send) yields no signature help.
const sigNoUri = 'file:///signature-none-test.st';
send({
  jsonrpc: '2.0',
  method: 'textDocument/didOpen',
  params: { textDocument: { uri: sigNoUri, languageId: 'smalltalk', version: 1, text: 'x printString ' } },
});
send({
  jsonrpc: '2.0',
  id: 12,
  method: 'textDocument/signatureHelp',
  params: { textDocument: { uri: sigNoUri }, position: { line: 0, character: 14 } },
});
const sigNone = (await receiveId(12)).result;
assert.ok(
  sigNone === null || (sigNone.signatures ?? []).length === 0,
  'a unary-send cursor returns no signature help',
);

// --- textDocument/hover (US-415) ---
// Selector hover: signature + an implementor (Object implements printString in
// both the bundled floor and an installed kernel), rendered as Markdown.
const hovUri = 'file:///hover-test.st';
send({
  jsonrpc: '2.0',
  method: 'textDocument/didOpen',
  params: { textDocument: { uri: hovUri, languageId: 'smalltalk', version: 1, text: 'x printString' } },
});
send({
  jsonrpc: '2.0',
  id: 8,
  method: 'textDocument/hover',
  params: { textDocument: { uri: hovUri }, position: { line: 0, character: 8 } }, // inside printString
});
const hovResult = (await receiveId(8)).result;
assert.ok(hovResult?.contents, 'hover must return contents for a selector');
const hovValue = typeof hovResult.contents === 'string' ? hovResult.contents : hovResult.contents.value ?? '';
assert.match(hovValue, /printString/, 'selector hover names the selector');
assert.match(hovValue, /Object/, 'selector hover lists the kernel implementor Object');
assert.match(hovValue, /```/, 'hover content is Markdown with a code fence (AC5)');

// Literal hover: a radix integer decodes to its decimal value.
const hovLitUri = 'file:///hover-literal-test.st';
send({
  jsonrpc: '2.0',
  method: 'textDocument/didOpen',
  params: { textDocument: { uri: hovLitUri, languageId: 'smalltalk', version: 1, text: '16rFF' } },
});
send({
  jsonrpc: '2.0',
  id: 9,
  method: 'textDocument/hover',
  params: { textDocument: { uri: hovLitUri }, position: { line: 0, character: 2 } },
});
const hovLit = (await receiveId(9)).result;
const hovLitValue = typeof hovLit?.contents === 'string' ? hovLit.contents : hovLit?.contents?.value ?? '';
assert.match(hovLitValue, /255/, 'radix literal hover decodes 16rFF to 255');

// --- textDocument/semanticTokens/full (US-422) ---
// A kernel class is classified as a `class` token off the bundled cartridge
// (no gst). Decode the delta-encoded stream and assert OrderedCollection's role.
const stUri = 'file:///semantic-tokens-test.st';
const stText = 'OrderedCollection new';
send({
  jsonrpc: '2.0',
  method: 'textDocument/didOpen',
  params: { textDocument: { uri: stUri, languageId: 'smalltalk', version: 1, text: stText } },
});
send({
  jsonrpc: '2.0',
  id: 10,
  method: 'textDocument/semanticTokens/full',
  params: { textDocument: { uri: stUri } },
});
const stResult = (await receiveId(10)).result;
assert.ok(Array.isArray(stResult?.data) && stResult.data.length > 0, 'semanticTokens/full returns a non-empty token stream');
assert.equal(stResult.data.length % 5, 0, 'semantic tokens data is a flat run of 5-tuples');
// Decode and find the token at column 0 (OrderedCollection); assert its type is `class`.
const classTypeIndex = stLegend.tokenTypes.indexOf('class');
let stLine = 0;
let stChar = 0;
let foundClassAtZero = false;
for (let i = 0; i + 4 < stResult.data.length; i += 5) {
  const dl = stResult.data[i];
  const dc = stResult.data[i + 1];
  stLine += dl;
  stChar = dl === 0 ? stChar + dc : dc;
  if (stLine === 0 && stChar === 0 && stResult.data[i + 3] === classTypeIndex) {
    foundClassAtZero = true;
  }
}
assert.ok(foundClassAtZero, 'OrderedCollection (col 0) is classified as a `class` from the bundled cartridge (AC2)');

// --- textDocument/publishDiagnostics (US-414 slice A) — parser tier ---
// Open a malformed doc; the server publishes parser diagnostics (debounced).
const diagUri = 'file:///diagnostics-test.st';
send({
  jsonrpc: '2.0',
  method: 'textDocument/didOpen',
  params: {
    textDocument: { uri: diagUri, languageId: 'smalltalk', version: 1, text: 'Object subclass: Foo [\n  bar [ ^1 \n' },
  },
});
let diagList = [];
for (let i = 0; i < 25 && diagList.length === 0; i++) {
  await sleep(100);
  diagList = diagnosticsByUri.get(diagUri) ?? [];
}
assert.ok(diagList.length > 0, 'malformed source must publish at least one parser diagnostic');
const parseDiag = diagList.find((d) => d.source === 'smalltalk' && d.code === 'parse');
assert.ok(parseDiag, 'expected a diagnostic with source "smalltalk" and code "parse" (badge smalltalk(parse))');
assert.equal(parseDiag.severity, 1, 'a missing `]` is an Error (LSP severity 1)');

// A clean doc carries no parser diagnostics (the earlier completion doc `x print`).
const cleanDiags = diagnosticsByUri.get(cmpUri);
assert.ok(
  cleanDiags === undefined || cleanDiags.every((d) => d.code !== 'parse'),
  'a clean document must not carry parser diagnostics',
);

send({ jsonrpc: '2.0', id: 3, method: 'shutdown' });
await receiveId(3);
send({ jsonrpc: '2.0', method: 'exit' });

clearTimeout(timeout);
console.log('Server LSP OK: capabilities, documentSymbol, workspace/symbol, definition, references, callHierarchy, foldingRange, documentHighlight, completion, signatureHelp, hover, semanticTokens, diagnostics, shutdown clean.');
child.on('close', () => process.exit(0));
setTimeout(() => process.exit(0), 500);
