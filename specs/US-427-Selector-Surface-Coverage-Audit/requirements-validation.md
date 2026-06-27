# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins
**Type**: Requirements Quality Gate
**Story**: US-427 — Selector-surface coverage audit · **Result**: **PASS** (2026-06-27)

---

## Section 1: Constitution Gates (Mandatory)
- [x] **Native Look & Feel**: Yes — VS Code's standard `contributes.snippets` declarative surface +
  a Markdown ADR. No custom UI.
- [x] **Zero Config**: Yes — snippets are always-on, no setting; works with no `gst`.
- [x] **Protocol First**: N/A for snippets (declarative contribution); the LSP selector surfaces
  (completion/signature help) are documented, not changed.
- [x] **Robustness**: The guard test fails loudly on a bad prefix/selector; the runtime surfaces are
  untouched so the front end's "never throw" posture is preserved.
- [x] **Dialect Agnostic**: The cross-check binds to the *active* cartridge mechanism (GST #01
  today); the division-of-labour contract is dialect-neutral. Snippets are scoped to the GST grammar.

## Section 2: Specification Completeness
- [x] Goals and Non-Goals explicitly listed (§2/§3) — including the explicit "do not fix the
  double-popup / context switch" non-goals.
- [x] User story in standard format (§4).
- [x] Acceptance scenarios defined — AC1–AC4 with concrete artifacts (inventory table, ADR-0004,
  14-snippet table, guard).
- [x] Edge cases identified (§6: prefix collisions, cartridge name drift, non-selector keys).
- [x] Dependencies listed — Cartridge #01 (`server/data/cartridges/gst-3.2.5-cartridge.json`),
  `docs/research/gst-syntax/` idiom corpus.

## Section 3: Technical Design
- [x] Contracts defined — ADR-0004 (division of labour); snippet schema = existing
  `prefix`/`body`/`description`/`scope` shape.
- [x] Data structures defined — §5.3 snippet table (selector → prefix → body); §5.4 guard inputs.
- [x] Error handling strategy — guard exits non-zero on collision / unknown selector / snapshot drift.
- [x] Testing strategy — Node guard (`src/test/snippets-verification.js`) wired into `npm run eval`;
  no new LSP/e2e tests (no runtime surface changes).

## Section 3.5: Acceptance Harness (TDD e2e plan)
- [x] **AC routing:**
  - **AC1 (inventory)** → documentation (spec §5.1); no executable test (a written artifact).
  - **AC2 (division-of-labour ADR)** → documentation (`docs/decisions/0004-*.md`); reviewed artifact.
  - **AC3 (snippets + guard)** → **unit guard** `src/test/snippets-verification.js` (unique prefixes +
    cartridge cross-check + prefix snapshot), run by `npm run eval`. This is the RED→GREEN harness:
    the guard is written first and fails (missing snapshot / pre-add prefix set), then driven green by
    adding the snippets and the snapshot.
  - **AC4 (no behaviour change)** → existing `test:parser` + `test:server` + `npm run eval` stay green
    (regression); the completion/signature-help providers and their evals are untouched.
- [x] **No user-observable LSP surface** changes — the only *runtime* surface is the declarative
  snippet contribution (VS Code renders it; not reachable from the Electron e2e harness without UI
  automation). Therefore the scaffolded `client/test-e2e/US-427.acceptance.test.js` stub is
  **removed**; the guard + the manual-QA tab-trigger matrix (verification.md) are the coverage. This
  is the §3.5 "no user-observable surface" path, recorded explicitly.

## Section 4: Validation Result
- [x] **PASS — Ready for implementation.**
