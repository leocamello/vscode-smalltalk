// Signature-help provider (US-425 / AC1, AC2).
//
// Keyword-message signature help: at a cursor inside a keyword send
// (`dict at: key put: ▮`), surface the matching keyword selectors from the
// workspace ∪ active-kernel cartridge index and highlight the active parameter —
// the keyword part whose argument the cursor is currently filling.
//
// The cursor is analysed from the TOKEN STREAM (robust at an incomplete cursor,
// like completion.ts): scan backward tracking bracket depth, skipping nested
// groups, and collect the `Keyword` tokens of the CURRENT message at depth 0
// until a boundary (an unmatched opener, `.`/`^`/`!`, a cascade `;`, an assignment
// `:=`, or `|`). Keyword messages are greedy, so every same-level keyword part is
// one selector typed so far. A typed keyword PREFIX matches every selector that
// starts with it (honest union, the US-423 posture); binary/unary/head cursors
// and an empty/non-matching set return null. Pure — vscode-languageserver-types
// only; never throws.

import {
  type ParameterInformation,
  type SignatureHelp,
  type SignatureInformation,
} from 'vscode-languageserver-types';
import { Provenance } from '../kernel/model';
import { TokenKind, type Token } from '../parser/token';

/** A keyword selector offered to signature help, with where it came from. */
export interface SignatureCandidate {
  readonly selector: string;
  /** Keyword parts: `at:put:` → ["at:","put:"]; unary/binary → [selector]. */
  readonly keywords: readonly string[];
  readonly provenance: Provenance;
}

/** A `SignatureInformation` carrying the selector it was built from (for tests +
 *  client correlation). Assignable to the LSP type, so it flows through unchanged. */
export interface SmalltalkSignature extends SignatureInformation {
  readonly selector: string;
}

export interface SmalltalkSignatureHelp extends SignatureHelp {
  readonly signatures: SmalltalkSignature[];
}

function provenanceRank(p: Provenance): number {
  switch (p) {
    case Provenance.Workspace:
      return 1;
    case Provenance.InstalledKernel:
      return 2;
    case Provenance.BundledKernel:
      return 3;
    default:
      return 4;
  }
}

function provenanceLabel(p: Provenance): string {
  switch (p) {
    case Provenance.Workspace:
      return 'workspace';
    case Provenance.InstalledKernel:
      return 'kernel (installed)';
    case Provenance.BundledKernel:
      return 'kernel (reference)';
    default:
      return '';
  }
}

/** Token kinds (at depth 0) that bound the start of the current keyword message. */
const BOUNDARY = new Set<TokenKind>([
  TokenKind.Period,
  TokenKind.Caret,
  TokenKind.Bang,
  TokenKind.Semicolon, // cascade — the next message is separate
  TokenKind.Assign,
  TokenKind.Pipe,
]);

const OPENERS = new Set<TokenKind>([TokenKind.LParen, TokenKind.LBracket, TokenKind.LBrace]);
const CLOSERS = new Set<TokenKind>([TokenKind.RParen, TokenKind.RBracket, TokenKind.RBrace]);

/** The keyword parts of the message enclosing `offset`, typed so far, in source
 *  order — or `[]` when the cursor is not inside a keyword send. */
function typedKeywordsAt(tokens: readonly Token[], offset: number): string[] {
  // Tokens fully to the left of the cursor (a keyword `at:` is "typed" once its
  // colon is in; the comment/EOF trivia is skipped).
  const pre = tokens.filter((t) => t.kind !== TokenKind.Comment && t.kind !== TokenKind.EOF && t.end <= offset);
  const keywords: string[] = [];
  let depth = 0;
  for (let i = pre.length - 1; i >= 0; i--) {
    const t = pre[i] as Token;
    if (CLOSERS.has(t.kind)) {
      depth++;
      continue;
    }
    if (OPENERS.has(t.kind)) {
      if (depth === 0) {
        break; // unmatched opener → start of the enclosing expression
      }
      depth--;
      continue;
    }
    if (depth > 0) {
      continue; // inside a nested argument group — ignore
    }
    if (t.kind === TokenKind.Keyword) {
      keywords.unshift(t.text);
    } else if (BOUNDARY.has(t.kind)) {
      break;
    }
  }
  return keywords;
}

/** Render a selector's keyword parts as a label + one parameter region per part. */
function buildSignature(c: SignatureCandidate): SmalltalkSignature {
  let label = '';
  const parameters: ParameterInformation[] = [];
  c.keywords.forEach((kw, i) => {
    if (i > 0) {
      label += ' ';
    }
    const start = label.length;
    label += kw;
    parameters.push({ label: [start, label.length] });
  });
  return {
    selector: c.selector,
    label,
    parameters,
    documentation: provenanceLabel(c.provenance),
  };
}

/** Signature help at byte `offset`. `signatures` is the merged workspace +
 *  active-kernel keyword-selector set (each provenance-tagged). */
export function signatureHelpAt(
  offset: number,
  _text: string,
  tokens: readonly Token[],
  signatures: readonly SignatureCandidate[],
): SmalltalkSignatureHelp | null {
  const typed = typedKeywordsAt(tokens, offset);
  if (typed.length === 0) {
    return null; // not a keyword send (unary/binary/head cursor)
  }

  // Dedup by selector, keeping the highest-confidence provenance.
  const best = new Map<string, SignatureCandidate>();
  for (const c of signatures) {
    const current = best.get(c.selector);
    if (current === undefined || provenanceRank(c.provenance) < provenanceRank(current.provenance)) {
      best.set(c.selector, c);
    }
  }

  // A candidate matches iff its keywords start with the typed prefix.
  const matches = [...best.values()].filter(
    (c) => c.keywords.length >= typed.length && typed.every((kw, i) => c.keywords[i] === kw),
  );
  if (matches.length === 0) {
    return null;
  }

  matches.sort(
    (a, b) =>
      provenanceRank(a.provenance) - provenanceRank(b.provenance) ||
      a.keywords.length - b.keywords.length ||
      a.selector.localeCompare(b.selector),
  );

  const built = matches.map(buildSignature);
  // Prefer the fully-typed selector (keyword count === typed length) as active.
  const exact = matches.findIndex((c) => c.keywords.length === typed.length);
  return {
    signatures: built,
    activeSignature: exact >= 0 ? exact : 0,
    activeParameter: typed.length - 1,
  };
}
