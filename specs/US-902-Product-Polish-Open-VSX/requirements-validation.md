# Requirements Validation Checklist — US-902

**Purpose**: Validate spec quality BEFORE implementation begins
**Type**: Requirements Quality Gate
**Story**: US-902 — Product Polish & Open VSX (1.0.0)

---

## Section 1: Constitution Gates (Mandatory)
- [x] **Native Look & Feel**: Uses standard VS Code surfaces only — `capabilities.*` manifest keys,
  `workspace.isTrusted` / `workbench.action.manageTrust`, the standard format gestures, and the LSP
  `CancellationToken`. No bespoke UI.
- [x] **Zero Config**: Graduating `format.enable` *removes* a config step (formatting now just works);
  the settings reorder makes the one setting that matters (GST path) the first one seen. No new required
  config. `OVSX_PAT` is CI infra, not user config.
- [x] **Protocol First**: Cancellation is plumbed through the LSP request tokens; workspace-trust /
  virtual-workspace are declared through the standard extension manifest capabilities.
- [x] **Robustness**: A cancelled request returns the empty-result shape; virtual-workspace / untrusted
  paths degrade best-effort; the front end never throws (Principle V preserved).
- [x] **Dialect Agnostic**: No dialect-specific logic added; all changes are packaging/manifest/docs and
  generic provider plumbing.
- [x] **No Telemetry (VII)**: `ovsx`/Open VSX is a publish-time CLI, not shipped code — zero runtime
  network surface added.

## Section 2: Specification Completeness
- [x] Goals and Non-Goals explicitly listed (§2/§3), including the two owner-only handoffs (record GIFs,
  provision `OVSX_PAT` + tag).
- [x] User story in standard role/capability/benefit format (§4).
- [x] Acceptance criteria AC1–AC9 defined and testable.
- [x] Edge cases identified: stale `format.enable` value ignored (AC2); missing `OVSX_PAT` no-ops (AC7);
  virtual workspace has no filesystem (AC5); already-cancelled token (AC6).
- [x] Dependencies listed: builds on the complete v0.13.0 engine; owner provisions `OVSX_PAT` + records media.

## Section 3: Technical Design
- [x] API/contract changes defined: manifest `capabilities.*`, removed `preview` + `format.enable`,
  `order` on settings, `deploy:ovsx` script + CI step, `runCurrentFile` trust gate, token threading (§5).
- [x] Data structures: `FormatSettings` loses `enable`; no other shape change.
- [x] Error handling: trust gate = warning + Manage-Trust action; cancellation = empty-result shape;
  missing secret = skipped step.
- [x] Testing strategy defined per AC in §3.5 below.

## Section 3.5: Acceptance Harness (TDD e2e plan) — **AC routing**
Route each AC to the layer that can actually observe it. Not every AC is e2e — several are manifest or
CI facts best pinned by a unit/static assertion.

| AC | Surface | Layer | Where |
|----|---------|-------|-------|
| **AC1** preview removed | manifest fact | **unit (manifest)** | `server/test` or a `scripts/` manifest assertion — `package.json` has no `preview` key |
| **AC2** formatting always-on | user-observable | **e2e** (red→green) | `client/test-e2e/US-902.acceptance.test.js` — Format Document edits a doc with **no `format.enable` set**; + unit: `FormatSettings` has no `enable` |
| **AC3** GST path first | manifest fact | **unit (manifest)** | manifest assertion — `gnuSmalltalkPath.order` is the minimum across `smalltalk.*` properties |
| **AC4** untrusted: static works / Run refused | mixed | **e2e** + **unit (manifest)** | e2e asserts the trust gate short-circuits Run when untrusted (drive `smalltalk.runCurrentFile`, assert no terminal spawned / warning path); manifest assertion for `untrustedWorkspaces.supported` + `restrictedConfigurations` |
| **AC5** virtual-workspace declared | manifest fact | **unit (manifest)** | manifest assertion — `virtualWorkspaces.supported === "limited"` + description present |
| **AC6** cancellation honoured | internal contract | **unit (server)** | `server/test` — call the workspace-spanning providers with a pre-cancelled token; assert prompt empty return + no full scan (spy on file reads) |
| **AC7** Open VSX in CI | CI/config fact | **static** | workflow assertion / review — `main.yml` has the `ovsx publish` step gated on release + secret; `deploy:ovsx` script + `ovsx` dep present |
| **AC8** polish docs | repo artifacts | **static** | presence checks — README demo section, `docs/media-shotlist.md`, `.github/ISSUE_TEMPLATE/*`, labels manifest |
| **AC9** docs synced + 1.0.0 | repo artifacts | **static + review** | doc-rot sweep in `verification.md`; `package.json` version === `1.0.0` |

- [x] Each AC is **routed** (table above). User-observable → e2e; manifest/CI/data invariants → unit or
  static assertion; no AC is left unpinned.
- [x] The user-observable ACs (**AC2 formatting-on**, **AC4 Run-refused-when-untrusted**) will be pinned
  by acceptance tests **written before implementation** that fail for the right reason (red), then driven
  green in `client/test-e2e/US-902.acceptance.test.js`.
- [x] The e2e stub is **kept** (AC2/AC4 are user-observable). Manifest/CI/doc ACs are recorded here as
  unit/static routes rather than forced into e2e.

**Note on AC4 e2e feasibility:** the Electron test host runs *trusted* by default, and toggling workspace
trust mid-session isn't reliably scriptable. So the e2e test asserts the **trusted** happy path is intact
and the unit layer covers the **untrusted refusal** by calling the guard directly / asserting the manifest
`restrictedConfigurations`. This is documented so the routing isn't mistaken for a gap.

## Section 4: Validation Result
- [x] **PASS** — Ready for implementation. ACs are testable and routed; scope is bounded to
  manifest/packaging/docs + two small code changes (trust gate, token threading); the two owner handoffs
  (record media, provision `OVSX_PAT` + tag) are called out as non-code gates.
