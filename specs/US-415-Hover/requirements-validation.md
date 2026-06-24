# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins  
**Type**: Requirements Quality Gate  
**Story**: US-415 — Hover

---

## Section 1: Constitution Gates (Mandatory)
- [X] **Native Look & Feel**: Uses the standard `textDocument/hover` LSP request and VS Code's
  built-in hover UI; Markdown `MarkupContent`. No custom widgets.
- [X] **Zero Config**: No new settings. Hover is always on; reuses the resolved kernel tier
  (`auto|bundled|off`) and workspace index already configured.
- [X] **Protocol First**: Pure LSP — advertise `hoverProvider`, answer `connection.onHover`.
- [X] **Robustness**: Provider returns `null` (never throws); superclass-chain walk uses a
  visited set + depth cap; comment extraction is best-effort.
- [X] **Dialect Agnostic**: Reads the neutral kernel model / DialectCartridge; provenance, not
  dialect, gates prose. Other dialects' cartridges plug in unchanged.

## Section 2: Specification Completeness
- [X] Goals and Non-Goals explicitly listed? (§2/§3)
- [X] User stories in standard format? (§4)
- [X] Acceptance scenarios defined? (§4 AC1–AC5; resolved Clarify gate §4a)
- [X] Edge cases identified? (§7 — no comment, no install, malformed chain)
- [X] Dependencies listed? (US-411 front end; US-412 workspace index; US-413/US-430 cartridge)

## Section 3: Technical Design
- [X] API/Command contracts defined? (`hoverAt(...) → Hover|null`; `onHover` wiring; kernel
  query facade additions)
- [X] Data structures defined? (reuses `LiteralNode`, `SymbolNode`, `IndexEntry` (+`superclass`),
  `DialectCartridge.documentation`)
- [X] Error handling strategy defined? (never throws; null on miss)
- [X] Testing strategy (Unit vs Integration) defined? (§6)

## Section 3.5: Acceptance Harness (TDD e2e plan)
AC routing:

| AC | Surface | Layer |
|----|---------|-------|
| AC1 selector signature + implementors (+ comment by provenance) | user-observable hover | **e2e** + unit |
| AC2 class superclass chain + comment | user-observable hover | **e2e** + unit |
| AC3 variable kind + declaration site | user-observable hover | **e2e** + unit |
| AC4 numeric literal radix/scaled-decimal value | user-observable hover | **e2e** + unit |
| AC5 Markdown with code fences | rendered hover shape | **e2e** + unit |
| Bundled cartridge stays facts-only | data invariant | **unit** (`kernelIndex` test) |
| Installed adapter may carry prose | data invariant | **unit** (indexer test) |
| `hoverProvider` advertised + Markdown over the wire | LSP protocol shape | **handshake** |
| Golden hover outputs | output eval | **`evals/datasets/hover/`** |

- [X] Each AC routed to a verification layer.
- [X] User-observable ACs pinned by acceptance tests written **before** implementation (RED),
  in `client/test-e2e/US-415.acceptance.test.js`, then driven green.
- [X] Story HAS a user-observable surface → e2e stub kept.

## Section 4: Validation Result
- [X] **PASS** — Ready for implementation.
