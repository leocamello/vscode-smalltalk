// Trust-gate decision for Run Current File (US-902, AC4). tsx — no VS Code.
//
// Running a file spawns `gst` on the workspace's content with a possibly
// workspace-scoped `gnuSmalltalkPath` — code execution. In an untrusted
// (Restricted Mode) workspace that must be refused. The *decision* is a pure
// helper so it's unit-testable without the `vscode` module; `runCurrentFile`
// consumes it and shows the message / Manage-Trust action.
import assert from 'node:assert/strict';
import { RESTRICTED_RUN_MESSAGE, runBlockedByTrust } from '../src/commands/trustGate.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

test('an untrusted workspace blocks Run Current File', () => {
  assert.equal(runBlockedByTrust(false), true);
});

test('a trusted workspace allows Run Current File', () => {
  assert.equal(runBlockedByTrust(true), false);
});

test('the refusal message names the risk and points at trust', () => {
  assert.match(RESTRICTED_RUN_MESSAGE, /trust/i);
  assert.match(RESTRICTED_RUN_MESSAGE, /gst|GNU Smalltalk|execut/i);
});

console.log(`trustGate: ${passed} tests passed.`);
