# Implementation Verification Checklist

**Purpose**: Verify implementation correctness AFTER coding  
**Type**: Implementation Verification  

---

## Section 1: Acceptance Criteria
- [ ] All ACs in `tasks.md` are checked?
- [ ] Each AC has a passing test?
- [ ] Each user-observable AC's acceptance test was **red before** implementation (TDD e2e)?

## Section 2: Code Quality
- [ ] `npm run lint` passes?
- [ ] `npm test` passes?
- [ ] No `any` types in TypeScript (unless justified)?
- [ ] JSDoc provided for public APIs?

## Section 3: Constitutional Compliance
- [ ] **Native**: Follows VS Code guidelines?
- [ ] **Zero Config**: Works without manual setup?
- [ ] **Robustness**: Handles errors gracefully?
- [ ] **TDD**: Tests were written/exist?

## Section 4: Manual Verification
- [ ] Feature works in Extension Host?
- [ ] No errors in Developer Tools console?

## Section 5: Sign-Off
- [ ] Ready for Merge?
