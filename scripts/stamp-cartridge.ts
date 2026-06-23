// Cartridge contentHash stamper (US-430, slice D / AC5; EPIC-005, ADR-0003).
//
//   npm run stamp:cartridge [-- <cartridge.json>]
//
// The build-time step the reflective exporter (scripts/export-gst-cartridge.st)
// defers to: it emits `contentHash: "pending"`, and this stamps the real,
// deterministic SHA-256 over the fact tables (server/src/kernel/cartridgeHash.ts).
// The hash excludes the header, so it is idempotent — re-running on an already
// stamped file is a no-op. Run after (re)generating a cartridge, then commit.
//
// Only the `contentHash` header field is rewritten; the rest of the file stays
// byte-identical (the exporter already emits canonical, sorted-key JSON), so the
// committed diff is a single value.

import fs from 'node:fs';
import path from 'node:path';
import { cartridgeContentHash } from '../server/src/kernel/cartridgeHash';
import type { DialectCartridge } from '../server/src/types/knowledge-base';

const DEFAULT_CARTRIDGE = path.join('server', 'data', 'cartridges', 'gst-3.2.5-cartridge.json');
const target = process.argv[2] ?? path.join(process.cwd(), DEFAULT_CARTRIDGE);

if (!fs.existsSync(target)) {
  console.error(`stamp-cartridge: cartridge not found: ${target}`);
  process.exit(1);
}

const raw = fs.readFileSync(target, 'utf8');
const cartridge = JSON.parse(raw) as DialectCartridge;
const hash = cartridgeContentHash(cartridge);

// The header is canonical (sorted keys), so `"contentHash":"…"` is unique to it.
const stamped = raw.replace(/"contentHash":"[^"]*"/, `"contentHash":"${hash}"`);
if (stamped === raw && cartridge.header.contentHash !== hash) {
  console.error('stamp-cartridge: could not locate the contentHash field to stamp.');
  process.exit(1);
}

fs.writeFileSync(target, stamped, 'utf8');
console.log(
  `stamp-cartridge: ${path.relative(process.cwd(), target)} → ${hash}` +
    (cartridge.header.contentHash === hash ? ' (unchanged)' : ''),
);
