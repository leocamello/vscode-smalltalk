# Requirements Validation Checklist

**Purpose**: Validate spec quality BEFORE implementation begins  
**Type**: Requirements Quality Gate  

---

## Section 1: Constitution Gates (Mandatory)
- [ ] **Native Look & Feel**: Uses standard VS Code APIs?
- [ ] **Zero Config**: Defaults provided? Auto-detection planned?
- [ ] **Protocol First**: LSP/DAP used where applicable?
- [ ] **Robustness**: Error handling defined?
- [ ] **Dialect Agnostic**: Architecture supports future dialects?

## Section 2: Specification Completeness
- [ ] Goals and Non-Goals explicitly listed?
- [ ] User stories in standard format?
- [ ] Acceptance scenarios (Given/When/Then) defined?
- [ ] Edge cases identified?
- [ ] Dependencies listed?

## Section 3: Technical Design
- [ ] API/Command contracts defined?
- [ ] Data structures defined?
- [ ] Error handling strategy defined?
- [ ] Testing strategy (Unit vs Integration) defined?

## Section 3.5: Acceptance Harness (TDD e2e plan)
- [ ] Each AC is **routed** to a verification layer: user-observable → e2e (`client/test-e2e/`);
  internal contract / data invariant → unit or `evals/` golden dataset; LSP protocol shape → handshake.
- [ ] User-observable ACs will be pinned by acceptance tests **written before implementation** that fail
  for the right reason (red), then driven green (`client/test-e2e/{{US_ID}}.acceptance.test.js`).
- [ ] If the story has **no** user-observable surface, that is stated here and the scaffolded e2e stub
  is removed.

## Section 4: Validation Result
- [ ] PASS - Ready for implementation
