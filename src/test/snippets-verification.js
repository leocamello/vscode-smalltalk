// Snippet coverage guard (US-427 / AC3).
//
// Keeps `snippets/snippets.json` honest as the static selector surface (the
// block-bearing-templates half of the division of labour pinned in ADR-0004):
//   1. valid JSON;
//   2. unique `prefix` across every snippet (no trigger collisions);
//   3. every snippet whose KEY is a keyword selector (contains ':') resolves in
//      GST Cartridge #01 — so we never template a non-existent selector
//      (this caught `valuesDo:`/`withIndexDo:` during US-427);
//   4. a prefix snapshot (sorted `prefix -> key`) so any change to an existing
//      prefix is a visible, reviewed diff.
//
// Plain Node, no deps — mirrors grammar-verification.js (snapshot via --update).

const fs = require('fs');
const path = require('path');

const repoRoot = path.join(__dirname, '../..');
const snippetsPath = path.join(repoRoot, 'snippets/snippets.json');
const cartridgePath = path.join(repoRoot, 'server/data/cartridges/gst-3.2.5-cartridge.json');
const snapshotPath = path.join(__dirname, 'snapshots/snippets.prefixes.snap');

/** Every selector defined by any class in the cartridge (instance + class side). */
function cartridgeSelectors(cartridge) {
  const set = new Set();
  for (const cls of Object.values(cartridge.classes || {})) {
    for (const m of cls.instanceMethods || []) {
      if (m.selector) set.add(m.selector);
    }
    for (const m of cls.classMethods || []) {
      if (m.selector) set.add(m.selector);
    }
  }
  return set;
}

function run() {
  const errors = [];

  let snippets;
  try {
    snippets = JSON.parse(fs.readFileSync(snippetsPath, 'utf8'));
  } catch (e) {
    console.error(`snippets.json is not valid JSON: ${e.message}`);
    process.exit(1);
  }

  const cartridge = JSON.parse(fs.readFileSync(cartridgePath, 'utf8'));
  const selectors = cartridgeSelectors(cartridge);

  // (2) Unique prefixes.
  const byPrefix = new Map();
  for (const [key, snip] of Object.entries(snippets)) {
    const prefix = snip.prefix;
    if (byPrefix.has(prefix)) {
      errors.push(`Duplicate prefix "${prefix}" — used by "${byPrefix.get(prefix)}" and "${key}".`);
    } else {
      byPrefix.set(prefix, key);
    }
  }

  // (3) Keyword-selector keys must resolve in the cartridge.
  for (const key of Object.keys(snippets)) {
    if (key.includes(':') && !selectors.has(key)) {
      errors.push(`Snippet key "${key}" is a keyword selector not found in GST Cartridge #01.`);
    }
  }

  // (4) Prefix snapshot (sorted `prefix -> key`).
  const lines = [...byPrefix.entries()]
    .sort((a, b) => a[0].localeCompare(b[0]))
    .map(([prefix, key]) => `${prefix} -> ${key}`);
  const snapshotContent = lines.join('\n');

  if (process.argv.includes('--update')) {
    fs.mkdirSync(path.dirname(snapshotPath), { recursive: true });
    fs.writeFileSync(snapshotPath, snapshotContent + '\n');
    console.log(`Updated snapshot: ${path.relative(repoRoot, snapshotPath)}`);
  } else if (fs.existsSync(snapshotPath)) {
    const expected = fs.readFileSync(snapshotPath, 'utf8').replace(/\r\n/g, '\n').trimEnd();
    if (expected !== snapshotContent) {
      errors.push('Prefix snapshot mismatch — review the diff and re-run with --update if intended.');
    }
  } else {
    errors.push(`Missing prefix snapshot at ${path.relative(repoRoot, snapshotPath)}. Run with --update to create.`);
  }

  if (errors.length > 0) {
    for (const e of errors) console.error(`✗ ${e}`);
    console.error(`${errors.length} snippet check failure(s).`);
    process.exit(1);
  }
  console.log(`All snippet checks passed (${byPrefix.size} snippets, ${selectors.size} cartridge selectors).`);
}

run();
