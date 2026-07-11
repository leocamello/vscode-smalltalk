// Manifest polish guards (US-902, 1.0.0). tsx.
//
// Several 1.0 acceptance criteria are *manifest facts* — best pinned by a static
// assertion over package.json rather than an e2e round-trip (routing:
// requirements-validation.md §3.5). This locks them in as regression guards:
//   AC1  no `preview` flag
//   AC2  no `smalltalk.format.enable` setting (formatting graduated to always-on)
//   AC3  `smalltalk.gnuSmalltalkPath` is the first setting (lowest `order`)
//   AC4  `capabilities.untrustedWorkspaces` limited + restrictedConfigurations
//   AC5  `capabilities.virtualWorkspaces` limited + described
//   AC7  Open VSX tooling wired (`ovsx` dep + `deploy:ovsx` script)
//   AC9  version has reached 1.0 (major >= 1)
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
const pkg = JSON.parse(fs.readFileSync(path.join(repoRoot, 'package.json'), 'utf8'));
const props: Record<string, { order?: number }> = pkg.contributes?.configuration?.properties ?? {};

test('AC1: no `preview` flag on the manifest', () => {
  assert.ok(!('preview' in pkg), 'package.json must not carry "preview" at 1.0');
});

test('AC2: `smalltalk.format.enable` setting is removed (formatting always available)', () => {
  assert.ok(
    !('smalltalk.format.enable' in props),
    'the format.enable gate must be gone — formatting is graduated to always-on',
  );
  // The other format knobs survive.
  for (const knob of ['smalltalk.format.indentSize', 'smalltalk.format.cascades', 'smalltalk.format.blockStyle']) {
    assert.ok(knob in props, `${knob} must remain`);
  }
});

test('AC3: `smalltalk.gnuSmalltalkPath` is the first setting (strictly lowest order)', () => {
  const orders = Object.entries(props).map(([k, v]) => [k, v.order] as const);
  for (const [k, o] of orders) {
    assert.equal(typeof o, 'number', `${k} needs an explicit \`order\` so the UI sort is deterministic`);
  }
  const gstOrder = props['smalltalk.gnuSmalltalkPath']?.order;
  assert.equal(typeof gstOrder, 'number', 'gnuSmalltalkPath must declare an order');
  for (const [k, o] of orders) {
    if (k === 'smalltalk.gnuSmalltalkPath') continue;
    assert.ok((o as number) > (gstOrder as number), `gnuSmalltalkPath.order (${gstOrder}) must precede ${k} (${o})`);
  }
});

test('AC4: untrustedWorkspaces declared limited + restricts the gst path setting', () => {
  const uw = pkg.capabilities?.untrustedWorkspaces;
  assert.ok(uw, 'capabilities.untrustedWorkspaces must be declared');
  assert.equal(uw.supported, 'limited', 'static intelligence works untrusted; execution does not → "limited"');
  assert.ok(
    Array.isArray(uw.restrictedConfigurations) && uw.restrictedConfigurations.includes('smalltalk.gnuSmalltalkPath'),
    'a workspace-scoped gnuSmalltalkPath must be restricted in untrusted workspaces',
  );
});

test('AC5: virtualWorkspaces declared limited + described', () => {
  const vw = pkg.capabilities?.virtualWorkspaces;
  assert.ok(vw, 'capabilities.virtualWorkspaces must be declared');
  assert.equal(vw.supported, 'limited', 'on-disk features (indexing, Run) need a filesystem → "limited"');
  assert.ok(typeof vw.description === 'string' && vw.description.length > 0, 'virtualWorkspaces needs a description');
});

test('AC7: Open VSX publishing is wired (ovsx dep + deploy:ovsx script)', () => {
  const devDeps = pkg.devDependencies ?? {};
  assert.ok('ovsx' in devDeps, 'ovsx must be a devDependency for the CI publish step');
  assert.ok(
    typeof pkg.scripts?.['deploy:ovsx'] === 'string' && pkg.scripts['deploy:ovsx'].includes('ovsx'),
    'a `deploy:ovsx` script must invoke ovsx publish',
  );
});

test('AC9: version has reached 1.0 (major >= 1)', () => {
  const major = Number(String(pkg.version).split('.')[0]);
  assert.ok(major >= 1, `expected a 1.x release version, got ${pkg.version}`);
});

console.log(`manifest.test: ${passed} passed.`);
