// Whitespace-only token-stream formatter (US-416, ADR-0005).
//
// The formatter NEVER re-synthesizes source from the AST. It walks the US-411
// significant-token stream in order, emits each token's exact text verbatim, and
// computes only the whitespace *between* consecutive tokens (indentation by
// bracket depth, a (prevKind,nextKind) spacing table, blank-line collapse,
// cascade-segment alignment, and long-keyword-message wrapping). Because token
// texts are copied and never reordered, the significant-token stream is invariant
// by construction (AC3), and because every whitespace decision is derived from a
// normal form, formatting is idempotent (AC3). A file with ANY parse diagnostic
// is returned unchanged — never reformat around an error (AC5).
//
// Pure: no `vscode` import, runs under tsx.

import { tokenize } from '../parser/lexer';
import { parse } from '../parser/parser';
import { TokenKind, type Token } from '../parser/token';
import {
  NodeKind,
  type BlockNode,
  type CascadeNode,
  type DefinitionNode,
  type MessageNode,
  type MethodDefinitionNode,
  type ProgramNode,
} from '../parser/ast';
import { visit } from '../parser/walk';

export interface FormatOptions {
  /** Spaces per indent level (when insertSpaces). */
  indentSize: number;
  /** Indent with spaces (true) or a tab per level (false). */
  insertSpaces: boolean;
  /** `align`: break each cascade segment onto its own indented line. `preserve`: keep author line breaks. */
  cascades: 'align' | 'preserve';
  /** Wrap a keyword message one-keyword-per-line when its single-line column exceeds this. 0 disables. */
  keywordWrap: number;
  /** `expand`: break block/method/class bodies onto their own indented lines, one statement per line.
   *  `preserve`: keep the author's block layout (only normalize whitespace within it). */
  blockStyle: 'preserve' | 'expand';
}

export const DEFAULT_FORMAT_OPTIONS: FormatOptions = {
  indentSize: 4,
  insertSpaces: true,
  cascades: 'align',
  keywordWrap: 100,
  blockStyle: 'preserve',
};

const OPENERS = new Set<TokenKind>([
  TokenKind.LParen,
  TokenKind.LBracket,
  TokenKind.LBrace,
  TokenKind.HashParen,
  TokenKind.HashBracket,
  TokenKind.HashBrace,
]);
const CLOSERS = new Set<TokenKind>([TokenKind.RParen, TokenKind.RBracket, TokenKind.RBrace]);

const isOpener = (k: TokenKind): boolean => OPENERS.has(k);
const isCloser = (k: TokenKind): boolean => CLOSERS.has(k);

/** Whether a single space belongs between two same-line tokens (tight brackets, GST kernel style). */
function needsSpace(prev: Token, cur: Token): boolean {
  // Comments float with a space, but hug an adjacent bracket (tight blocks).
  if (prev.kind === TokenKind.Comment) {
    return !isCloser(cur.kind);
  }
  if (cur.kind === TokenKind.Comment) {
    return !isOpener(prev.kind);
  }
  // No padding just inside brackets: `[^x]`, `(a + b)`, `#(1 2 3)`, `{1. 2}`.
  if (isOpener(prev.kind)) return false;
  if (isCloser(cur.kind)) return false;
  // `.` and `;` hug the token they follow.
  if (cur.kind === TokenKind.Period || cur.kind === TokenKind.Semicolon) return false;
  // A namespace scope (`A.B`, `A::B`) MUST stay tight — a space would re-lex `.`
  // as a statement `Period`, changing the token stream (AC3).
  if (prev.kind === TokenKind.Scope || cur.kind === TokenKind.Scope) return false;
  // These bind to what follows: `^value`, `:param`, `#sym`.
  if (prev.kind === TokenKind.Caret) return false;
  if (prev.kind === TokenKind.Colon) return false;
  if (prev.kind === TokenKind.Hash) return false;
  // Everything else (sends, binaries, keywords, assignment, pipes, separators) gets one space.
  return true;
}

const countNewlines = (s: string): number => {
  let n = 0;
  for (let i = 0; i < s.length; i++) {
    if (s.charCodeAt(i) === 10) n += 1;
  }
  return n;
};

/** A forced line break before a token: extra indent levels, and whether to keep a blank line the author left. */
interface BreakInfo {
  extra: number;
  keepBlank: boolean;
}

/** Build the forced break-before map (token start offset -> break info) from the AST. */
function collectForcedBreaks(
  ast: ReturnType<typeof parse>['ast'],
  toks: Token[],
  lineDepth: number[],
  indexByStart: Map<number, number>,
  opts: FormatOptions,
): Map<number, BreakInfo> {
  const breaks = new Map<number, BreakInfo>();
  const mark = (off: number, extra: number, keepBlank: boolean): void => {
    breaks.set(off, { extra, keepBlank });
  };

  /** Inline-rendered width of the tokens spanning [startOff, endOff). */
  const inlineWidth = (startOff: number, endOff: number): number => {
    let width = 0;
    let prev: Token | null = null;
    for (const t of toks) {
      if (t.start < startOff) continue;
      if (t.start >= endOff) break;
      if (prev) width += needsSpace(prev, t) ? 1 : 0;
      width += t.text.length;
      prev = t;
    }
    return width;
  };

  const startIndex = (off: number): number => {
    const exact = indexByStart.get(off);
    if (exact !== undefined) return exact;
    // First token at or after `off`.
    let lo = 0;
    let hi = toks.length;
    while (lo < hi) {
      const mid = (lo + hi) >> 1;
      if (toks[mid]!.start < off) lo = mid + 1;
      else hi = mid;
    }
    return lo;
  };

  const markCascade = (node: CascadeNode): void => {
    let local = 0;
    let firstMarked = false;
    for (let i = startIndex(node.start); i < toks.length && toks[i]!.start < node.end; i++) {
      const t = toks[i]!;
      if (isCloser(t.kind)) local = Math.max(0, local - 1);
      if (local === 0) {
        if (!firstMarked && t.start >= node.receiver.end && t.kind !== TokenKind.Period) {
          mark(t.start, 1, false);
          firstMarked = true;
        }
        if (t.kind === TokenKind.Semicolon) {
          const next = toks[i + 1];
          if (next && next.start < node.end) mark(next.start, 1, false);
        }
      }
      if (isOpener(t.kind)) local += 1;
    }
  };

  const markKeywordWrap = (node: MessageNode): void => {
    const idx = startIndex(node.start);
    const column = lineDepth[idx]! * opts.indentSize + inlineWidth(node.start, node.end);
    if (column <= opts.keywordWrap) return;
    let local = 0;
    for (let i = idx; i < toks.length && toks[i]!.start < node.end; i++) {
      const t = toks[i]!;
      if (isCloser(t.kind)) local = Math.max(0, local - 1);
      if (local === 0 && t.kind === TokenKind.Keyword) mark(t.start, 1, false);
      if (isOpener(t.kind)) local += 1;
    }
  };

  // --- blockStyle: expand — break body items onto their own lines, one per line. ---

  /** Offset of the node's closing `]` (the body bracket), if present. */
  const closingBracketOffset = (end: number): number | undefined => {
    const last = toks[startIndex(end) - 1];
    return last && last.kind === TokenKind.RBracket ? last.start : undefined;
  };

  /** Offset of the `|` opening a temporaries declaration, just before its first name. */
  const tempsBarOffset = (rangeStart: number, firstTempStart: number): number | undefined => {
    let best: number | undefined;
    for (const t of toks) {
      if (t.start >= firstTempStart) break;
      if (t.start >= rangeStart && t.kind === TokenKind.Pipe) best = t.start;
    }
    return best;
  };

  /** Break before each item offset (first item keepBlank=false to hug the `[`, rest preserve blanks). */
  const markItems = (offsets: number[], closeEnd: number): void => {
    const sorted = [...offsets].sort((a, b) => a - b);
    sorted.forEach((off, i) => mark(off, 0, i > 0));
    const close = closingBracketOffset(closeEnd);
    if (close !== undefined && sorted.length > 0) mark(close, 0, false);
  };

  const markMethodBody = (node: MethodDefinitionNode): void => {
    const items: number[] = node.pragmas.map((p) => p.start);
    const firstTemp = node.temporaries[0];
    if (firstTemp) {
      const bar = tempsBarOffset(node.start, firstTemp.start);
      if (bar !== undefined) items.push(bar);
    }
    for (const s of node.statements) items.push(s.start);
    markItems(items, node.end);
  };

  const markClassBody = (node: DefinitionNode): void => {
    markItems(
      node.body.map((item) => item.start),
      node.end,
    );
  };

  const markBlockBody = (node: BlockNode): void => {
    if (node.statements.length < 2) return; // single-statement blocks stay inline
    const items: number[] = [];
    const firstTemp = node.temporaries[0];
    if (firstTemp) {
      const bar = tempsBarOffset(node.start, firstTemp.start);
      if (bar !== undefined) items.push(bar);
    }
    for (const s of node.statements) items.push(s.start);
    markItems(items, node.end);
  };

  const markProgram = (node: ProgramNode): void => {
    // Top-level: one statement per line (no enclosing brackets; the first keeps its position).
    node.statements.forEach((s, i) => {
      if (i > 0) mark(s.start, 0, true);
    });
  };

  visit(ast, (node) => {
    if (node.kind === NodeKind.Cascade && opts.cascades === 'align') {
      markCascade(node);
    } else if (node.kind === NodeKind.Message && node.messageType === 'keyword' && opts.keywordWrap > 0) {
      markKeywordWrap(node);
    }
    if (opts.blockStyle === 'expand') {
      if (node.kind === NodeKind.Program) markProgram(node);
      else if (node.kind === NodeKind.MethodDefinition) markMethodBody(node);
      else if (node.kind === NodeKind.Definition) markClassBody(node);
      else if (node.kind === NodeKind.Block) markBlockBody(node);
    }
  });

  return breaks;
}

/** Format Smalltalk source: normalize whitespace only, preserving every token and comment. */
export function formatSource(source: string, options?: Partial<FormatOptions>): string {
  const opts = { ...DEFAULT_FORMAT_OPTIONS, ...(options ?? {}) };
  if (source.length === 0) return '';

  const { ast, diagnostics } = parse(source);
  // Non-destructive on error: any parse problem -> leave the file untouched (AC5).
  if (diagnostics.length > 0) return source;

  const toks = tokenize(source).tokens.filter((t) => t.kind !== TokenKind.EOF);
  if (toks.length === 0) return source; // whitespace-only: no change.

  const eol = source.includes('\r\n') ? '\r\n' : '\n';
  const unit = opts.insertSpaces ? ' '.repeat(Math.max(1, opts.indentSize)) : '\t';
  const indentOf = (d: number): string => unit.repeat(Math.max(0, d));

  // Indent depth a token would sit at if it begins a line (closer dedents first).
  const lineDepth = new Array<number>(toks.length);
  {
    let d = 0;
    for (let i = 0; i < toks.length; i++) {
      if (isCloser(toks[i]!.kind)) d = Math.max(0, d - 1);
      lineDepth[i] = d;
      if (isOpener(toks[i]!.kind)) d += 1;
    }
  }

  const indexByStart = new Map<number, number>();
  toks.forEach((t, i) => indexByStart.set(t.start, i));

  const forced = collectForcedBreaks(ast, toks, lineDepth, indexByStart, opts);

  let out = toks[0]!.text;
  let depth = isOpener(toks[0]!.kind) ? 1 : 0;
  for (let i = 1; i < toks.length; i++) {
    const cur = toks[i]!;
    const prev = toks[i - 1]!;
    if (isCloser(cur.kind)) depth = Math.max(0, depth - 1);

    const info = forced.get(cur.start);
    if (info !== undefined) {
      const blank = info.keepBlank && countNewlines(source.slice(prev.end, cur.start)) >= 2;
      out += (blank ? eol + eol : eol) + indentOf(depth + info.extra);
    } else {
      const nl = countNewlines(source.slice(prev.end, cur.start));
      if (nl >= 2) {
        out += eol + eol + indentOf(depth);
      } else if (nl === 1) {
        out += eol + indentOf(depth);
      } else {
        out += needsSpace(prev, cur) ? ' ' : '';
      }
    }
    out += cur.text;
    if (isOpener(cur.kind)) depth += 1;
  }

  // Preserve the author's trailing-newline choice (exactly one when present).
  if (/\n[ \t]*$/.test(source)) out += eol;
  return out;
}

/** Indent depth (levels) the first token on `line` sits at — used by on-type formatting. */
export function lineIndentDepth(source: string, line: number): number {
  const toks = tokenize(source).tokens.filter((t) => t.kind !== TokenKind.EOF);
  let d = 0;
  for (const t of toks) {
    const isFirstOnLine = t.startPos.line === line;
    if (isCloser(t.kind)) {
      if (isFirstOnLine) return Math.max(0, d - 1);
      d = Math.max(0, d - 1);
    }
    if (isFirstOnLine && !isCloser(t.kind)) return d;
    if (t.startPos.line > line) return d;
    if (isOpener(t.kind)) d += 1;
  }
  return d;
}
