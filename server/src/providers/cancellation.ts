// Cancellation-token plumbing (US-902, AC6).
//
// The workspace-spanning requests (workspace symbols, references, call
// hierarchy, rename) can read many files from disk. When the editor supersedes
// a request it cancels the token; honouring it means a stale request stops
// instead of finishing a full multi-file scan. Latency is already tiny
// (index ≈0.2 s, completion p95 ≈5 ms — US-901), so this is good-citizen
// correctness, not a perf fix: a cancelled request just returns the empty-result
// shape (the front end never throws — Principle V).
//
// `CancellationLike` is the structural subset of the LSP `CancellationToken` we
// use, so this module stays dependency-free and unit-testable.

export interface CancellationLike {
  readonly isCancellationRequested: boolean;
}

/** True iff a token is present and already cancelled. Undefined ⇒ not cancelled. */
export function isCancelled(token?: CancellationLike): boolean {
  return token?.isCancellationRequested === true;
}

export interface FileText {
  readonly uri: string;
  readonly text: string;
}

/**
 * Read a list of files cooperatively, bailing as soon as the token is cancelled.
 * `read` returns the file text, or `undefined` for an unreadable file (skipped,
 * best-effort — never throws). The loop checks cancellation *before* each read so
 * an already-cancelled token does zero work.
 */
export function readFilesCancellable(
  uris: readonly string[],
  read: (uri: string) => string | undefined,
  token?: CancellationLike,
): FileText[] {
  const out: FileText[] = [];
  for (const uri of uris) {
    if (isCancelled(token)) break;
    const text = read(uri);
    if (text !== undefined) out.push({ uri, text });
  }
  return out;
}
