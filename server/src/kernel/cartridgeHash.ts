// Cartridge content hashing (US-430, slice D / AC5; EPIC-005, ADR-0003).
//
// Deterministic SHA-256 over a cartridge's FACT TABLES (`classes` + optional
// `crossReference`) — never the header. Excluding the header makes the hash
// independent of the one volatile field (`builtAt`) and idempotent under
// re-stamping, so it is a stable cache key for the in-memory index (the loader
// memoizes by `header.contentHash`) and the deterministic-output guard's anchor.
//
// Pure: `node:crypto` only. The hash is canonical — object keys are sorted
// recursively, so it is invariant to the serializer's key order.

import { createHash } from 'node:crypto';
import type { DialectCartridge, JsonValue } from '../types/knowledge-base';

/** Canonical JSON: objects emit keys in sorted order; arrays keep order. */
function canonicalize(value: JsonValue): string {
  if (value === null || typeof value !== 'object') {
    return JSON.stringify(value);
  }
  if (Array.isArray(value)) {
    return `[${value.map(canonicalize).join(',')}]`;
  }
  const keys = Object.keys(value).sort();
  return `{${keys.map((k) => `${JSON.stringify(k)}:${canonicalize(value[k] as JsonValue)}`).join(',')}}`;
}

/** `sha256-<hex>` over the canonicalized fact tables (header excluded). */
export function cartridgeContentHash(cartridge: DialectCartridge): string {
  const tables = {
    classes: cartridge.classes,
    crossReference: cartridge.crossReference ?? null,
  } as unknown as JsonValue;
  return `sha256-${createHash('sha256').update(canonicalize(tables)).digest('hex')}`;
}
