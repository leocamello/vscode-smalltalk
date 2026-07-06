// Workspace-trust gate for Run Current File (US-902, AC4).
//
// Running a file spawns `gst` on the workspace's on-disk content, and the gst
// path can come from a workspace-scoped `smalltalk.gnuSmalltalkPath` — that is
// code execution. In an untrusted (Restricted Mode) workspace it must be refused.
// The manifest also lists `smalltalk.gnuSmalltalkPath` in
// `capabilities.untrustedWorkspaces.restrictedConfigurations`, so a malicious
// workspace value can't apply; this runtime guard is the belt to that suspenders
// and gives the user an actionable message. The *decision* is a pure function so
// it's unit-testable without the `vscode` module (client/test/trustGate.test.ts).

export const RESTRICTED_RUN_MESSAGE =
  'Running a Smalltalk file executes code with GNU Smalltalk (gst) and is disabled in Restricted Mode. Trust this workspace to run files.';

export const MANAGE_TRUST_ACTION = 'Manage Workspace Trust';

/** True iff Run Current File must be refused because the workspace is untrusted. */
export function runBlockedByTrust(isTrusted: boolean): boolean {
  return !isTrusted;
}
