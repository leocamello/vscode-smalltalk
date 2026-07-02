// No-telemetry guard (US-901, AC3). tsx.
//
// The extension is fully offline: it performs NO network I/O and ships NO
// telemetry (CLAUDE.md / README). This is a durable regression guard, not a
// one-time audit — it scans the server + client sources for network/telemetry
// surface and fails on any hit. `gst` (child_process) and the LSP connection
// (stdio) are NOT network I/O and are intentionally not on the denylist.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

const here = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(here, '../..');

// Network/telemetry surface. Each entry is [label, regex]. Kept specific to
// imports + call sites so prose/URLs in comments don't false-positive.
const DENYLIST: Array<[string, RegExp]> = [
  ['node http/https/net/tls/dgram import', /\bfrom\s+['"](?:node:)?(?:https?|http2|net|tls|dgram)['"]/],
  ['node http/https/net/tls/dgram require', /\brequire\(\s*['"](?:node:)?(?:https?|http2|net|tls|dgram)['"]\s*\)/],
  ['fetch() call', /\bfetch\s*\(/],
  ['XMLHttpRequest', /\bXMLHttpRequest\b/],
  ['WebSocket', /\bWebSocket\b/],
  ['vscode TelemetryLogger/Sender', /\bTelemetry(?:Logger|Sender|Reporter)\b/],
  ['telemetry package', /extension-telemetry|applicationinsights|@vscode\/telemetry/i],
];

/** All *.ts source files under a dir (excludes test/ + node_modules). */
function sourceFiles(dir: string): string[] {
  const out: string[] = [];
  const walk = (d: string): void => {
    for (const ent of fs.readdirSync(d, { withFileTypes: true })) {
      const full = path.join(d, ent.name);
      if (ent.isDirectory()) {
        if (ent.name === 'node_modules' || ent.name === 'test' || ent.name === 'test-e2e') continue;
        walk(full);
      } else if (ent.name.endsWith('.ts') && !ent.name.endsWith('.test.ts')) {
        out.push(full);
      }
    }
  };
  walk(dir);
  return out;
}

// The matcher itself has teeth: it must flag known-bad code, so a green result
// on the real sources means something.
test('denylist matches known-bad samples (matcher has teeth)', () => {
  const bad = [
    "import http from 'node:http'",
    "const net = require('net')",
    'await fetch(url)',
    'new XMLHttpRequest()',
    'const r = new TelemetryReporter(key)',
    "import x from '@vscode/extension-telemetry'",
  ];
  for (const sample of bad) {
    assert.ok(DENYLIST.some(([, re]) => re.test(sample)), `expected a denylist hit for: ${sample}`);
  }
});

test('server + client sources contain no network/telemetry surface', () => {
  const files = [
    ...sourceFiles(path.join(repoRoot, 'server', 'src')),
    ...sourceFiles(path.join(repoRoot, 'client', 'src')),
  ];
  assert.ok(files.length > 0, 'found source files to scan');
  const hits: string[] = [];
  for (const file of files) {
    const text = fs.readFileSync(file, 'utf8');
    for (const [label, re] of DENYLIST) {
      if (re.test(text)) hits.push(`${path.relative(repoRoot, file)} — ${label}`);
    }
  }
  assert.deepEqual(hits, [], `network/telemetry surface found:\n${hits.join('\n')}`);
});

console.log(`noTelemetry.test: ${passed} passed.`);
