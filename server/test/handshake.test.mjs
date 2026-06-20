// Verifies the bundled server (dist/server.js) speaks LSP: spawn it as a plain
// Node process (no gst, no VS Code) and complete an initialize/shutdown handshake.
// Covers US-410 AC4 (server advertises capabilities) and AC5 (no Smalltalk runtime).
import { spawn } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import assert from 'node:assert/strict';
import path from 'node:path';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const serverPath = path.join(root, 'dist', 'server.js');

const child = spawn(process.execPath, [serverPath, '--stdio'], {
  stdio: ['pipe', 'pipe', 'inherit'],
});

function send(msg) {
  const json = JSON.stringify(msg);
  child.stdin.write(`Content-Length: ${Buffer.byteLength(json)}\r\n\r\n${json}`);
}

let buffer = Buffer.alloc(0);
const queue = [];
let waiter = null;

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
  console.error(`Server handshake FAILED: ${message}`);
  child.kill();
  process.exit(1);
}

const timeout = setTimeout(() => fail('timed out waiting for the server'), 10000);

send({
  jsonrpc: '2.0',
  id: 1,
  method: 'initialize',
  params: { processId: process.pid, rootUri: null, capabilities: {} },
});

const initResult = await receiveId(1);
assert.ok(initResult.result?.capabilities, 'initialize must return capabilities');
assert.equal(
  initResult.result.capabilities.textDocumentSync,
  2,
  'expected incremental textDocumentSync (2)',
);
assert.equal(
  initResult.result.capabilities.documentSymbolProvider,
  true,
  'expected documentSymbolProvider (US-412)',
);

send({ jsonrpc: '2.0', method: 'initialized', params: {} });

// US-412: open a brace-format document and request its outline from the real server.
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

send({ jsonrpc: '2.0', id: 3, method: 'shutdown' });
await receiveId(3);
send({ jsonrpc: '2.0', method: 'exit' });

clearTimeout(timeout);
console.log('Server LSP OK: capabilities advertised, documentSymbol works, shutdown clean.');
child.on('close', () => process.exit(0));
setTimeout(() => process.exit(0), 500);
