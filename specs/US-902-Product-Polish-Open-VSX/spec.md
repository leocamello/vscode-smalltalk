# Specification: Product Polish & Open VSX (1.0.0)

**ID**: US-902
**Feature**: Product Polish & Open VSX
**Status**: Draft
**Owner**: Leonardo Nascimento
**Created**: 2026-07-06
**Issue**: (to file) | **Milestone**: 1.0.0 | **Epic**: EPIC-004 (cross-cutting)

## 1. Overview
The offline language-intelligence surface is complete and hardened through v0.13.0 (US-901). **1.0 is
not a feature release** — it is the graduation pass that makes the extension read as a *finished,
trustworthy product*: remove the `preview` flag, graduate formatting out of its opt-in `format.enable`
gate, declare how the extension behaves in **untrusted** and **virtual** workspaces, plumb LSP
**cancellation tokens** through the workspace-spanning providers, publish to **Open VSX** alongside the
VS Code Marketplace, and polish the docs (README demo section + shot-list, issue templates, labels).
The settings are reordered so the **GST executable path is the first setting a user sees**. Reaching
1.0 = *offline parity with image-based extensions for everything that doesn't need a runtime, at zero
setup.* This story ships **no new provider**; it is manifest, packaging, docs, and good-citizen polish
over the existing engine.

## 2. Goals
- **Graduate the product surface:** remove `"preview": true`; remove the `smalltalk.format.enable`
  gate so conservative, idempotent formatting is **always available** (still whitespace-only, still
  only runs on the editor's own format gestures / `formatOnSave`).
- **Settings that read top-down:** reorder `contributes.configuration` so **`smalltalk.gnuSmalltalkPath`
  renders first** in the Settings UI (VS Code sorts alphabetically unless `order` is set), followed by
  the completion/kernel settings, then trace, then formatting.
- **Workspace-trust honesty:** declare `capabilities.untrustedWorkspaces` and
  `capabilities.virtualWorkspaces`, and make the runtime behaviour match the declaration (static LSP
  intelligence works everywhere; **code execution — Run Current File — requires a trusted workspace**).
- **Good-citizen cancellation:** the workspace-spanning server requests honour the LSP
  `CancellationToken` and bail promptly instead of always completing a full scan.
- **Reach beyond the Marketplace:** publish the same VSIX to **Open VSX** from CI on release.
- **Docs as a product:** a README demo section (captioned media placeholders) + a precise **GIF
  shot-list** for the owner to record; `.github/ISSUE_TEMPLATE/` (bug + feature) + a labels manifest
  (`area:parser`, `area:lsp`, `good-first-issue`); CONTRIBUTING linked from the README.
- **Doc-rot sweep inline with the release:** `docs/product/*`, ROADMAP, README, CLAUDE, CHANGELOG all
  synced to shipped reality; version bumped to **1.0.0**.

## 3. Non-Goals
- Any new provider or language feature (EPIC-005 consumers are complete for 1.0; Tonel wedge US-424 is
  post-1.0).
- Any runtime/`gst` intelligence — compile/semantic diagnostics stay deferred to EPIC-007.
- **Recording the demo GIFs/screenshots** — the environment cannot screen-capture. This story delivers
  the README structure, captions, and a shot-list; the owner records and drops the assets, which land
  before the `v1.0.0` tag (see §6).
- **Provisioning the `OVSX_PAT` secret** and **cutting the `v1.0.0` release** — owner-only, irreversible
  actions. This story wires CI and drafts the release notes; the owner provisions the secret and tags.
- A hard CI perf gate, worker offload, telemetry (Principle VII stands — zero network I/O).

## 4. User Stories & Acceptance Criteria
**US-902**: As a **user**, I want a **polished 1.0 with discoverable docs, honest workspace-trust
behaviour, and availability beyond the VS Code Marketplace**, so that **the extension reads as a
finished, trustworthy product.**

- **AC1 — Preview flag removed:** `"preview": true` is gone from `package.json`; `npm run package`
  produces a non-preview VSIX and CI stays green.
- **AC2 — Formatting graduated:** the `smalltalk.format.enable` setting is removed; document / range /
  on-type formatting is **always available** (the server no longer pulls or checks `enable`). It stays
  whitespace-only + idempotent and only runs when the user invokes a format gesture or has
  `formatOnSave`. A workspace that still carries a stale `smalltalk.format.enable` value is unaffected
  (unknown setting, ignored — no error). The other `format.*` knobs remain.
- **AC3 — GST path is the first setting:** in the Settings UI the **`smalltalk.gnuSmalltalkPath`**
  property sorts **before** all other `smalltalk.*` settings (achieved with `order` on the
  configuration properties, since VS Code otherwise sorts alphabetically and `completion.*` would lead).
  The documented order is: GST path → kernel library → kernel path → trace → formatting knobs.
- **AC4 — Untrusted-workspace declaration + enforcement:** the manifest declares
  `capabilities.untrustedWorkspaces` with `supported: "limited"` and
  `restrictedConfigurations: ["smalltalk.gnuSmalltalkPath"]`. All **static LSP intelligence works in a
  Restricted-Mode (untrusted) workspace**. **Run Current File refuses in an untrusted workspace** with a
  clear message offering to manage workspace trust — it never spawns `gst` from an untrusted folder or
  honours a workspace-scoped `gnuSmalltalkPath`.
- **AC5 — Virtual-workspace declaration:** the manifest declares `capabilities.virtualWorkspaces` with
  `supported: "limited"` and a description; in a virtual (no-filesystem) workspace, in-memory LSP
  features degrade cleanly and on-disk-only work (workspace indexing, kernel discovery, Run Current
  File) fails silently/best-effort — never throws.
- **AC6 — Cancellation plumbing:** the workspace-spanning server requests — **workspace symbols,
  references, call hierarchy, and rename** — receive the LSP `CancellationToken` and honour it: a
  request whose token is already cancelled returns promptly (empty / no edits) without completing the
  full multi-file scan, and the front end still never throws.
- **AC7 — Open VSX publishing:** CI publishes the packaged VSIX to **Open VSX** via `ovsx publish`
  (gated on a `release` event + the `OVSX_PAT` secret, mirroring the `vsce`/`MARKETPLACE` step), and
  the `ovsx` tooling + `deploy:ovsx` script are wired. The step no-ops safely if the secret is absent.
- **AC8 — Product-polish docs:** the README has a **demo section** with captioned media placeholders
  for highlighting / outline / completion / Run / diagnostics and links CONTRIBUTING; a committed
  **shot-list** (`docs/media-shotlist.md`) specifies each capture precisely; `.github/ISSUE_TEMPLATE/`
  provides a bug-report and feature-request form; a labels manifest declares `area:parser`, `area:lsp`,
  and `good-first-issue`.
- **AC9 — Docs synced + version bumped:** `docs/product/{high-level-plan,epics,user-stories}.md`,
  `docs/ROADMAP.md`, `README.md`, `CLAUDE.md`, and `CHANGELOG.md` reflect the shipped 1.0 reality (US-902
  Done, 1.0 milestone reached); `package.json` version is **1.0.0** with a `1.0.0` CHANGELOG entry.

## 5. Technical Design

### 5.1 Manifest (`package.json`)
- Delete the top-level `"preview": true`.
- Delete the `smalltalk.format.enable` configuration property.
- Add `order` integers to every `contributes.configuration.properties` entry so the render order is:
  `gnuSmalltalkPath` (1) → `completion.kernelLibrary` (2) → `completion.kernelPath` (3) →
  `trace.server` (4) → `format.indentSize` (5) → `format.cascades` (6) → `format.keywordWrap` (7) →
  `format.blockStyle` (8).
- Add top-level `capabilities`:
  ```jsonc
  "capabilities": {
    "untrustedWorkspaces": {
      "supported": "limited",
      "description": "Language intelligence works in Restricted Mode. Running a file with GNU Smalltalk (gst) executes code and is disabled until you trust the workspace.",
      "restrictedConfigurations": ["smalltalk.gnuSmalltalkPath"]
    },
    "virtualWorkspaces": {
      "supported": "limited",
      "description": "Editing intelligence works on open files. Workspace-wide indexing, kernel discovery, and Run Current File need a local filesystem and are unavailable in virtual workspaces."
    }
  }
  ```
- Add `ovsx` to `devDependencies` and a `"deploy:ovsx": "ovsx publish --no-dependencies"` script.

### 5.2 Formatting always-on (`server/`)
- `providers/formatting.ts`: drop `enable` from `FormatSettings` + `DEFAULT_FORMAT_SETTINGS`; remove the
  three `if (!settings.enable) return []` guards (the format gestures are the gate now).
- `server.ts`: `getFormatSettings()` no longer reads `enable`; the doc-comment at the capability
  registration drops the "off by default behind `smalltalk.format.enable`" note. Capabilities are
  unchanged (formatting was always *registered*; only the per-request no-op is removed).

### 5.3 Workspace trust (`client/`)
- `client/src/commands/runCurrentFile.ts`: early-return with
  `window.showWarningMessage(…, 'Manage Workspace Trust')` when `!workspace.isTrusted`, before saving or
  spawning `gst`; the button runs `workbench.action.manageTrust`. This backstops the manifest
  `restrictedConfigurations` (which already prevents a workspace-scoped `gnuSmalltalkPath` from applying
  in Restricted Mode).
- No change needed for static LSP — the language client already runs in Restricted Mode; the server
  performs no code execution.

### 5.4 Cancellation plumbing (`server/`)
- `vscode-languageserver` request handlers already receive `(params, token)`. Thread `token` into the
  workspace-spanning providers: `onWorkspaceSymbol`, `onReferences`, `onPrepareCallHierarchy` /
  call-hierarchy incoming-outgoing, and `onRenameRequest`. Check `token.isCancellationRequested`:
  - before the multi-file scan (early bail → empty / `null`), and
  - inside the per-file loop in `allWorkspaceFiles` / `ivarCandidateFiles` and the reference/symbol
    walks (cooperative bail every file).
- Keep it honest to the measured latency (index ≈0.2 s, completion p95 ≈5 ms): cancellation is
  correctness/good-citizenship, not a perf fix. A cancelled request returns the same *shape* as an
  empty result — the front end never throws (Principle V).

### 5.5 CI / Open VSX (`.github/workflows/main.yml`)
- After the Marketplace publish step, add a **Publish to Open VSX** step: same `if` guard
  (`success() && github.event_name == 'release' && matrix.os == 'ubuntu-latest'`), runs
  `npm run deploy:ovsx` with `OVSX_PAT` in the env. Guard against a missing secret so a release without
  the PAT provisioned doesn't fail the job (skip when the secret is empty).

### 5.6 Docs & polish
- `README.md`: add a **Demo** section near the top with five captioned image placeholders
  (`media/demo-*.gif`) and prose; ensure CONTRIBUTING + docs are linked. Update the config table for the
  new order and the removed `format.enable`. Note Open VSX availability + Restricted-Mode behaviour.
- `docs/media-shotlist.md` (new): one entry per capture — file name, the exact sample `.st`, the
  keystrokes/commands, crop/size guidance, and target length — so the owner can record deterministically.
- `.github/ISSUE_TEMPLATE/bug_report.md` + `feature_request.md` (+ `config.yml`); a labels manifest
  (`.github/labels.yml` or documented in CONTRIBUTING) declaring `area:parser`, `area:lsp`,
  `good-first-issue`.
- Doc-rot sweep across the status docs (AC9) as the final release step.

## 6. Risks & Limitations
- **Demo assets are a handoff:** the shot-list + README land in this story; the actual GIFs are recorded
  by the owner and must be dropped into `media/` **before** the `v1.0.0` tag, or the README shows broken
  images. The verification gate lists this as an owner action.
- **`OVSX_PAT` not yet provisioned:** the CI step is inert until the owner creates the secret. The step
  is written to no-op safely when the secret is empty so an early release doesn't red-fail CI.
- **Restricted Mode UX:** gating Run Current File on trust is a (small) behaviour change; mitigated by a
  clear, actionable message (Manage Trust) and the fact that all read-only intelligence is unaffected.
- **`order` and settings sorting:** VS Code honours `order` within a single configuration object; verified
  by a manifest unit assertion (AC3 routes to a unit check, not e2e — the Settings UI order isn't
  scriptable from the extension host).
- **Removing `format.enable` is one-way** for opted-out users: anyone who had formatting off now gets it
  on their next explicit format gesture. Accepted — it has been opt-in for ≥3 releases (0.10→0.13) and it
  is whitespace-only + idempotent; documented in the CHANGELOG.
