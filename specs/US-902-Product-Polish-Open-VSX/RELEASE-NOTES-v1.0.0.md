# v1.0.0 — The Complete Offline GNU Smalltalk IDE

*Draft release notes for the `v1.0.0` GitHub Release (US-902). Owner creates the release; CI publishes to
the Marketplace + Open VSX. Provision `OVSX_PAT` and drop the demo GIFs into `media/` before tagging.*

---

**1.0 is here.** `vscode-smalltalk` reaches **offline parity with image-based Smalltalk extensions for
everything that doesn't need a runtime — at zero setup.** Syntax & semantic highlighting, outline &
navigation, completion, diagnostics, hover, references / senders / implementors, call hierarchy, signature
help, formatting, and scope-aware rename (variables **and** classes) all work with **no `gst` installed and
no telemetry** — your code never leaves your machine.

This release is a **product-polish milestone**, not a new-feature drop — it graduates the extension out of
preview and makes it read as a finished, trustworthy 1.0.

### Highlights
- **Out of preview** — the "Preview" badge is gone.
- **Formatting just works** — the `smalltalk.format.enable` switch has been removed; conservative,
  idempotent, whitespace-only formatting is now always available on **Format Document / Selection** and
  `editor.formatOnSave`. (It never changes your code — only whitespace.)
- **Now on [Open VSX](https://open-vsx.org/)** — installable in VSCodium, Gitpod, Eclipse Theia, and other
  editors, alongside the VS Code Marketplace.
- **Workspace Trust done right** — all language intelligence works in **Restricted Mode**; **Run Current
  File** executes code with `gst`, so it's disabled until you trust the workspace (one click to manage
  trust), and a workspace-scoped `smalltalk.gnuSmalltalkPath` is ignored while untrusted.
- **Snappier under load** — the workspace-spanning requests (workspace symbols, references, call hierarchy,
  rename) honour cancellation, so a superseded request stops instead of finishing a full scan.
- **Friendlier settings** — the **GNU Smalltalk path** is now the first setting you see.
- **Better docs** — a README demo section, issue templates, and contributor labels.

### Notes for existing users
- If you had `smalltalk.format.enable` set, it's now a harmless no-op (the setting was removed); formatting
  runs on your next explicit format gesture. It has been opt-in since 0.10 and only rewrites whitespace.
- Nothing else changes in how your existing features behave.

**Full changelog:** see `CHANGELOG.md` (`[1.0.0]`).

---

### Maintainer checklist (before creating this release)
1. Record the five demo GIFs per `docs/media-shotlist.md` → commit `media/demo-*.gif` (else the README shows
   broken images).
2. Provision the **`OVSX_PAT`** repository secret (an Open VSX access token), analogous to `MARKETPLACE`.
3. Confirm the **`MARKETPLACE`** PAT hasn't expired (Azure DevOps PATs expire ~yearly).
4. Run the manual-QA matrix in `specs/US-902-Product-Polish-Open-VSX/manual-qa-workspace/README.md`.
5. Create the `v1.0.0` GitHub Release → CI packages and publishes to the Marketplace **and** Open VSX.
