# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins
**Type**: Requirements Quality Gate
**Story**: US-413 — Completion + GNU Smalltalk kernel index
**Architecture**: [ADR-0002](../../docs/decisions/0002-kernel-symbol-sourcing.md)

---

## Section 1: Constitution Gates (Mandatory)
- [x] **Native Look & Feel**: Uses the standard VS Code completion widget via LSP
  `textDocument/completion`, snippet insertion (`InsertTextFormat.Snippet`), and a standard status-bar
  item. No custom UI.
- [x] **Zero Config**: Default `kernelLibrary = auto` gives useful completion out of the box — installed
  kernel when present, bundled reference otherwise — with no settings required; discovery is automatic.
- [x] **Protocol First**: A pure LSP completion provider over the bundled server; client unchanged except
  the status-bar surface.
- [x] **Robustness**: Built on the never-throws front end; indexer skips unreadable/parse-failing files;
  providers return empty results, never errors; the bundle is an always-safe fallback.
- [x] **Dialect Agnostic**: The kernel index model is dialect-neutral and fed by per-dialect **source
  adapters** (GST file adapter now; image-based reflective-export adapters later) — ADR-0002,
  Constitution Principle IV.

## Section 2: Specification Completeness
- [x] Goals and Non-Goals explicitly listed (§2/§3; hover/diagnostics/formatting, comment prose, image
  dialects, `smalltalk.dialect`, type inference excluded).
- [x] User stories in standard format (§4).
- [x] Acceptance scenarios (Given/When/Then) defined (§4), one per AC incl. determinism, installed vs
  bundled sourcing, `off`, and snippet insertion.
- [x] Edge cases identified: corpus absent in CI, regeneration drift, no `gst`, install discovery across
  OSes, malformed file, selector over-offering, licensing (no prose), clean-install VSIX.
- [x] Dependencies listed: US-411 (`parse`, `buildSymbolTable`) + US-412 (`WorkspaceIndex`,
  `parseCache`) — merged; `vscode-languageserver` types; the bundled GST kernel corpus (build-time only).

## Section 3: Technical Design
- [x] API/Command contracts defined: `textDocument/completion` + advertised `completionProvider`;
  `gen:kernel-index` build script; `kernelLibrary`/`kernelPath` settings (§5).
- [x] Data structures defined: the neutral `KernelIndexData`/`KernelClass`/`KernelSelector` model, the
  committed `kernel-index.json` shape, provenance enum, the directory indexer contract (§5).
- [x] Error handling strategy defined: never-throws indexer/providers; empty/partial results; always-safe
  bundle fallback; discovery failures degrade gracefully.
- [x] Testing strategy defined: unit (snapshot/invariant **without** corpus + licensing no-prose gate +
  drift guard when corpus present + completion-context + discovery) + server (handshake completion) +
  e2e (fixture workspace) + a **manual verification matrix** (§6 / `verification.md`) gating 0.5.0.

## Section 4: Validation Result
- [x] PASS - Ready for implementation

**Emphasis**: two hard, story-specific gates beyond green CI — (1) the **licensing** constraint is
encoded as a test (the index carries *facts only*, no comment prose), and (2) per PO direction the §6
**manual verification matrix** (real-corpus workspace, selector/class/variable completion, keyword
snippet, installed vs bundled sourcing + provenance honesty, no-`gst`, clean-install VSIX) is a release
gate recorded in `verification.md`.
