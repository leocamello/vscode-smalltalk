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
assert.equal(caps.textDocumentSync.change, 2, 'expected incremental textDocumentSync (change: 2)');
assert.ok(caps.textDocumentSync.save, 'expected save sync enabled (US-414 gst-on-save)');
assert.equal(caps.documentSymbolProvider, true, 'expected documentSymbolProvider (US-412)');
assert.equal(caps.workspaceSymbolProvider, true, 'expected workspaceSymbolProvider (US-412)');
assert.equal(caps.definitionProvider, true, 'expected definitionProvider (US-412)');
assert.equal(caps.foldingRangeProvider, true, 'expected foldingRangeProvider (US-417)');
assert.equal(caps.documentHighlightProvider, true, 'expected documentHighlightProvider (US-417)');
assert.ok(caps.completionProvider, 'expected completionProvider (US-413)');
assert.ok(caps.codeActionProvider, 'expected codeActionProvider (US-414)');

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
const defUri = 'file:///definition-test.st';
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
console.log('Server LSP OK: capabilities, documentSymbol, workspace/symbol, definition, foldingRange, documentHighlight, completion, diagnostics, shutdown clean.');
child.on('close', () => process.exit(0));
setTimeout(() => process.exit(0), 500);
