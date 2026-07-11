# Implementation Plan: Product Polish & Open VSX (1.0.0)

**ID**: US-902 | **Date**: 2026-07-06 | **Spec**: ./spec.md | **Branch**: `feature/US-902-product-polish-open-vsx`

## Summary
1.0 is a graduation/polish pass, not a feature release. The bulk is **manifest + packaging + docs**
(remove `preview`, remove `format.enable`, reorder settings, declare `capabilities.*`, wire Open VSX)
plus **two small code changes** (gate Run Current File on workspace trust; thread the LSP
`CancellationToken` through the workspace-spanning providers). No new provider, no `gst` runtime.

## Approach
Reuse existing patterns everywhere:
- **Manifest** (`package.json`): drop `preview`; drop the `smalltalk.format.enable` property; add `order`
  to each config property (GST path = 1); add top-level `capabilities.untrustedWorkspaces` +
  `capabilities.virtualWorkspaces`; add `ovsx` dev-dep + `deploy:ovsx` script.
- **Formatting** (`server/src/providers/formatting.ts`, `server.ts`): remove `enable` from
  `FormatSettings` / `DEFAULT_FORMAT_SETTINGS` and the three `if (!settings.enable) return []` guards;
  `getFormatSettings()` stops reading `enable`. Capabilities unchanged.
- **Trust gate** (`client/src/commands/runCurrentFile.ts`): early `!workspace.isTrusted` guard →
  `showWarningMessage(…, 'Manage Workspace Trust')` → `workbench.action.manageTrust`; mirror the existing
  "gst not found" message style.
- **Cancellation** (`server/src/server.ts`): the request handlers already receive `(params, token)`.
  Add `token.isCancellationRequested` early-bails to `onWorkspaceSymbol`, `onReferences`, the call-
  hierarchy handlers, and `onRenameRequest`; thread an optional token into `allWorkspaceFiles` /
  `ivarCandidateFiles` and the workspace-symbol / reference walks so a per-file loop bails cooperatively.
  Keep the empty-result shape (never throw).
- **CI** (`.github/workflows/main.yml`): add a Publish-to-Open-VSX step after the Marketplace step, same
  `if` guard, `OVSX_PAT` env, `npm run deploy:ovsx`, skip-if-secret-empty.
- **Docs**: README demo section + captioned `media/demo-*.gif` placeholders + config-table update;
  `docs/media-shotlist.md`; `.github/ISSUE_TEMPLATE/{bug_report,feature_request}.md` + `config.yml`;
  labels manifest; then the full doc-rot sweep (AC9) at release.

## Steps
1. **Acceptance Harness (RED first):** add AC2 (formatting-on) + AC4-happy-path (Run works when trusted)
   e2e tests to `client/test-e2e/US-902.acceptance.test.js`; add a manifest unit assertion
   (`server/test/manifest.test.ts` or extend an existing one) covering AC1/AC3/AC4/AC5; add a cancellation
   unit test (AC6). Prove red.
2. **Manifest:** remove `preview`; remove `format.enable`; add `order`; add `capabilities.*`; add `ovsx`
   dep + `deploy:ovsx` script.
3. **Formatting always-on:** strip `enable` from server.
4. **Trust gate:** guard `runCurrentFile`.
5. **Cancellation:** thread tokens through the four workspace-spanning providers.
6. **CI:** Open VSX publish step.
7. **Docs/polish:** README demo + shot-list + issue templates + labels.
8. **Green all layers** (`check-types`, `lint`, `test:parser`, `test:server`, `test:client`, `eval`,
   `test:e2e`, `package`), then the **doc-rot sweep** + version bump to 1.0.0.

## Dependencies & Risks
- **Owner handoffs (non-code):** record the 5 demo GIFs from the shot-list into `media/`; provision the
  `OVSX_PAT` secret; create the `v1.0.0` GitHub Release (I draft notes + prompt). Called out in
  `verification.md`.
- **AC4 e2e limitation:** the test host is trusted by default and trust isn't reliably toggled mid-run;
  the untrusted *refusal* is covered by the manifest assertion + a direct guard unit check, the trusted
  *happy path* by e2e (documented in `requirements-validation.md` §3.5).
- **`format.enable` removal is one-way** for opted-out users — accepted (opt-in ≥3 releases,
  whitespace-only + idempotent), noted in the CHANGELOG.

## Verification
- **Acceptance harness (TDD e2e):** AC2 + AC4-trusted pinned red→green in
  `client/test-e2e/US-902.acceptance.test.js`; AC1/AC3/AC4-untrusted/AC5 → manifest unit assertions;
  AC6 → server cancellation unit test; AC7/AC8/AC9 → static/CI/doc-review. Routing in
  `requirements-validation.md` §3.5.
- **All layers green** on the dev box + CI (Linux/macOS/Windows + e2e).
- **Manual QA** in `specs/US-902-*/manual-qa-workspace/` + the matrix in `verification.md` (settings
  order in the real Settings UI, Restricted-Mode behaviour, formatting-on, VSIX smoke).
