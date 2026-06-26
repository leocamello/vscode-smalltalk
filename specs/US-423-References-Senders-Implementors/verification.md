# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding  
**Type**: Implementation Verification  
**US**: US-423 — References + Senders/Implementors (Two-Tier Engine) · **Updated**: 2026-06-26 · **Shipped**: v0.9.0

---

## Section 1: Acceptance Criteria
- [X] All ACs in `tasks.md` are checked (Slices A–D: T010–T013, T020–T022, T030–T032, T040–T041; T900 done; T901 in progress; T902 on push).
- [X] Each AC has a passing test:
  - **AC1** (references = de-duped workspace ∪ cartridge union; `Workspace ≻ Cartridge` dev-box
    precedence) — `resolve.test.ts` (union, dev-box dedup for implementors + senders); `test:server`
    `references` returns ≥3 over the fixture; e2e `AC1`; eval `references` union cases.
  - **AC2** (branded commands → tree with union/uncertainty **header disclaimer** + per-row
    **provenance**) — `crossReference.test.ts` (disclaimer + provenance + hint badges); e2e `AC2`
    asserts the command's structured result; eval asserts every result carries the disclaimer.
  - **AC3** (plural go-to-definition, never collapsed) — `test:server` definition is plural over two
    implementors; e2e `AC3`; `LocationLink[]` gated on client `linkSupport`, else `Location[]`.
  - **AC4** (call hierarchy reuses the engine: incoming = senders, outgoing = sends) —
    `callHierarchy.test.ts` (resolve method/send/class-side, prepare+data, incoming/outgoing grouping);
    `test:server` prepare→incoming/outgoing; e2e `AC4`.
  - **AC5** (O(1) probes + O(k) merge, no parse/I-O at query time) — structural: `workspaceXref`
    O(1) `sendersOf`; cartridge O(1) `crossReference` tier; `resolve` is pure array merge. Pinned by
    `workspaceXref.test.ts` (incremental patch) + `resolve.test.ts`.
  - **AC6** (`receiverHint` ranks likely above possible, **never filters**) — `resolve.test.ts`
    (null-hint long-tail kept; self/known-class float up); eval `AC6` case; QA Part B3.
  - **AC7** (works with no `gst`) — every unit/eval runs over the **bundled** cartridge with
    common-location probing disabled; `resolve.test.ts` no-cartridge case; QA Part E1.

## Section 2: Code Quality
- [X] `npm run lint` passes (eslint clean, client + server).
- [X] `npm test` passes — `test:parser` (incl. `workspaceXref` 8, `resolve` 7, `callHierarchy` 8,
  `crossReference` 5), `test:server` (references + plural definition + call-hierarchy), `test:e2e`
  (27 passing), full `npm run eval` (references 5/5 + all prior datasets).
- [X] No `any` types — engine + providers fully typed; the client reads `item.data` via a typed
  `CallItemData`; the cross-reference wire shape is an explicit interface on both ends.
- [X] JSDoc on the public API (`WorkspaceXref`, `resolve*`, `buildCrossReference`, the call-hierarchy
  builders, `registerCrossReferences`, the cartridge content provider).

## Section 3: Constitutional Compliance
- [X] **Native**: standard `textDocument/references`, `callHierarchy`, plural `definition`
  (`LocationLink[]`) drive VS Code's built-in Peek/Call Hierarchy/Go-to-Definition; a TreeView in a
  contributed panel + the palette/context menus host the branded commands; a `TextDocumentContentProvider`
  serves cartridge rows.
- [X] **Zero Config**: no setting required; the cartridge tier resolves via the US-430 Console and
  degrades to `[]` when absent; works with no `gst`.
- [X] **Robustness**: never throws — providers return empty on a missing doc / no selector; the query
  path does no parsing or I-O so it can't fail on malformed input; cartridge under-count is framed in the
  header, not hidden.
- [X] **TDD**: the e2e Acceptance Harness + unit tests were written RED before each slice, then GREEN.

## Section 4: Manual Verification
- [X] Manual-QA workspace prepared — `manual-qa-workspace/` (`Theatre.st` + matrix Parts A–E). The
  fixture is **pre-verified offline** to parse with **0 diagnostics** and the engine's outputs were
  checked to match every expected value in the matrix (Implementors/Senders of `#greet`/`#do:`/`#new`,
  the `(top level)` sender label, the class-side `Stage class` implementor ranked first, and the
  receiver-hint badges).
- [X] **Hands-on Extension Host pass + PO sign-off — DONE (2026-06-26).** The §A–E click-through was run
  in the Extension Development Host. It caught **five** real issues the automated layers missed, all fixed
  before release: (1) provenance vocabulary leaked the internal `cartridge:…` term — unified with the
  status-bar identity; (2) the cartridge virtual-doc copy mislabeled the installed tier — made
  tier-accurate; (3) the installed tier had no senders — gained its own `crossReference` tier (parity with
  bundled); (4) installed rows opened a stub instead of the real source — now carry a `SourceLocation` and
  open the actual `.st`; (5) a kernel file opened to *view* was indexed as `workspace` and shadowed the
  cartridge — open docs are now indexed only when under a workspace folder.
- [X] No errors in the Developer Tools console during the pass.

## Section 5: Sign-Off
- [X] **Ready for Merge — PO-accepted 2026-06-26**; merged via PR #97 (closes #66), CI green on
  Linux/macOS/Windows + e2e, and **shipped in v0.9.0** (published to the Marketplace).
