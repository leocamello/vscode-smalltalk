# Specification: References + Senders/Implementors (Two-Tier Engine)

**ID**: US-423
**Feature**: `textDocument/references`, "Senders of"/"Implementors of" commands, and call hierarchy over a two-tier (workspace + cartridge) cross-reference engine
**Status**: Draft
**Owner**: Leonardo Nascimento
**Created**: 2026-06-22
**Architecture**: [ADR-0002](../../docs/decisions/0002-kernel-symbol-sourcing.md), [ADR-0003](../../docs/decisions/0003-cartridge-resolution.md)

## 1. Overview
The flagship navigation feature and the most visible proof of the offline Knowledge Graph: bring the
System Browser's **Senders of / Implementors of** muscle memory to file-based GNU Smalltalk, **offline**,
across the user's workspace *and* the bundled kernel cartridge. Because Smalltalk is dynamically typed,
results are presented honestly as a **lexical union** ("everything that responds to / sends `#sel`"),
never as false single-target resolution. Call hierarchy is folded in (it is the same index).

## 2. Goals
- `textDocument/references` returning the de-duplicated **union** of workspace + cartridge send-sites
  (senders) / definitions (implementors), with `Workspace ≻ Cartridge` precedence.
- Commands **`Smalltalk: Senders of…`** / **`Smalltalk: Implementors of…`** returning a structured tree
  with a header node stating the union/uncertainty contract and per-row **provenance**.
- `callHierarchy/incomingCalls|outgoingCalls` reusing the same cross-reference index.
- A **workspace cross-reference index** (new) kept fresh incrementally on `didChange`.
- Sub-10 ms typical queries (O(1) lookups + O(k) merge; no parsing/I-O at query time).
- Works with **no `gst`**.

## 3. Non-Goals
- **No type inference / receiver resolution** — we never claim *which* implementor a send dispatches to;
  we rank by a cheap `receiverHint` but always return the full union.
- **No selector rename** (that is US-426, scope-only) and no refactoring.
- **No live image queries** — purely static over the workspace + cartridge.
- No System Browser tree view (that is US-801; this story provides the cross-reference data it will reuse).

## 4. User Stories & Acceptance Criteria
**US-423**: As a Smalltalk developer, I want "Senders of" and "Implementors of" across my workspace and
the bundled kernel, instantly and offline, so that I get the System Browser's cross-reference muscle
memory without a running image — and I am told honestly that results are a union.

- **AC1**: `references` returns the de-duplicated union of workspace + cartridge send-sites/definitions,
  `Workspace ≻ Cartridge` precedence (handles the **dev-box overlap** where the kernel source is also open
  in the workspace).
- **AC2**: `Senders of…` / `Implementors of…` commands return a tree with a **header node** stating the
  union/uncertainty contract and **per-row provenance** (`workspace` / `cartridge:<dialect>@<version>`).
- **AC3**: Go-to-definition on a message send returns **plural** `LocationLink[]`; never collapses to one.
- **AC4**: `callHierarchy/incoming|outgoingCalls` reuse the same cross-reference index (incoming =
  senders, outgoing = sends within a method).
- **AC5**: Query path does **no** parsing/I-O — O(1) lookups + O(k) merge; target < 10 ms typical.
- **AC6**: `receiverHint` ranks *likely* responders above *possible* ones; the long tail is never filtered.
- **AC7**: Works with no `gst`.

## 5. Technical Design

### 5.1 The two data tiers (asymmetric by design)
- **Workspace index (live):** mutates on every keystroke; kept fresh incrementally — on `didChange`,
  reparse the one doc and patch its slice of the maps. **New work:** today `workspaceIndex.ts` holds
  symbols/definitions only; add `selector → SendSite[]` (senders) and `selector → ImplementorRef[]`
  (implementors) built by walking message-send and method nodes in the US-411 AST.
- **Cartridge (frozen):** loaded once (US-430 `cartridgeLoader`); the `crossReference` tier already ships
  precomputed `implementors` and `senders`. Sliced reads memoized by `contentHash`.

### 5.2 Query pipeline ("Senders of `#do:`")
1. **Resolve token** under the cursor → normalize selector (`do:`; keywords `[do:]`, arity 1). Classify
   intent: senders vs implementors (references = the union).
2. **Workspace probe — O(1):** `workspaceXref.senders.get("do:")`.
3. **Cartridge probe — O(1):** `cartridge.crossReference?.senders["do:"]` (absent tier ⇒ `[]`, graceful).
4. **Normalize** both `SendSite[]` → a common `ResolvedRef { uri, range, provenance, receiverHint,
   classId, side }`. Cartridge sites synthesize a read-only URI (`smalltalk-cartridge:/<dialect>/<version>/…`)
   + single-line range; workspace sites read ranges from the current AST.
5. **De-duplicate** — key `(canonicalUri, line, selector)`. The real collision is the **dev-box case**
   (kernel source also open in the workspace, per the `dev-box-has-gst-installed` memory): when a workspace
   path equals a cartridge source path, **drop the frozen entry, keep the live one**. Precedence
   `Workspace ≻ Cartridge`.
6. **Sort** — stable, multi-key: provenance (workspace first), `receiverHint` confidence
   (`self`/`super`/known-class above `unknown`), class name, side, line. `k` is small ⇒ sub-ms.
7. **Package** — `references` → flat `Location[]`. The branded commands → a tree with a header node
   carrying the honest-union disclaimer + per-row provenance/`receiverHint` badge.

### 5.3 Dynamic-dispatch UX posture
We cannot prove the receiver's class, so we never pretend to: return the **lexical union, labelled as a
union**; rank (don't filter) by `receiverHint`; never collapse go-to-definition to one target; tag every
row with provenance. Hiding a real responder is the cardinal sin.

### 5.4 Capabilities & wiring (`server/src/server.ts`)
- Advertise `referencesProvider` and `callHierarchyProvider`; register the two commands
  (`smalltalk.sendersOf`, `smalltalk.implementorsOf`).
- New providers: `server/src/providers/references.ts`, `callHierarchy.ts`; the shared engine in
  `server/src/xref/` (workspace xref index + the merge/reconcile in 5.2).

### 5.5 Dependencies
- **Hard dep on US-430** (`cartridgeLoader`, the `crossReference` views). US-412 (`workspaceIndex`,
  `parseCache`, dynamic config) + US-411 (AST) for the workspace xref. Call hierarchy reuses 5.1/5.2.

### 5.6 Testing
- **Unit (`test:parser`)**: workspace xref build/incremental-patch; merge (dedup/precedence/dev-box
  overlap); union packaging; `receiverHint` ranking; cartridge slice reads.
- **Server (`test:server`)**: real-server `references` + `callHierarchy` over a fixture; capability ads.
- **E2E (`test:e2e`)**: peek-references + the command tree on a fixture workspace.
- **Eval**: `evals/datasets/references/` (senders/implementors of known kernel + workspace selectors).

## 6. Manual Verification
Extension Development Host: Senders/Implementors of a kernel selector (e.g. `do:`) shows workspace + kernel
hits with provenance + the union header; dev-box overlap de-dups correctly; go-to-def on a send shows
multiple targets; call hierarchy expands incoming/outgoing; all with no `gst`. Matrix in `verification.md`.

## 7. Risks & Limitations
- **Cartridge senders under-count** (US-430 §5: special-selector bytecodes missing from the literal
  scan) — senders of `+`, `at:put:`, `do:` (when inlined) may be incomplete from the cartridge tier.
  Honest framing in the header node; workspace tier is complete; bytecode-disassembly refinement later.
- **Graded a Transient Lead** (copyable; a static shadow of the image's whole-world query) — accept; the
  moat is the offline Knowledge Graph beneath it, not this provider alone.
- **Workspace xref memory/perf** on large workspaces — honor the Constitution budgets (US-901); build
  lazily + cap; incremental patch avoids full rescans.
- **Synthetic cartridge URIs** must resolve to a read-only virtual document for peek/jump — use a
  `TextDocumentContentProvider` on the client (`smalltalk-cartridge:` scheme).
