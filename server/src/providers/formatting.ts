// Formatting providers (US-416): document / range / on-type. Thin LSP adapters
// over the pure whitespace-only formatter core (server/src/format/formatter.ts,
// ADR-0005). Each returns TextEdit[]; all are gated on `smalltalk.format.enable`
// (off by default, AC4) — disabled yields no edits. Pure: text in, edits out.

import { Position, Range, TextEdit, type FormattingOptions } from 'vscode-languageserver-types';
import { formatSource, lineIndentDepth, type FormatOptions } from '../format/formatter';

/** Resolved `smalltalk.format.*` settings (see package.json contributes). */
export interface FormatSettings {
  enable: boolean;
  indentSize: number;
  cascades: 'align' | 'preserve';
  keywordWrap: number;
  blockStyle: 'preserve' | 'expand';
}

export const DEFAULT_FORMAT_SETTINGS: FormatSettings = {
  enable: false,
  indentSize: 4,
  cascades: 'align',
  keywordWrap: 100,
  blockStyle: 'preserve',
};

/** On-type trigger characters we act on (must mirror the advertised capability). */
const ON_TYPE_TRIGGERS = new Set([']', '\n', ';']);

/** Merge the editor's FormattingOptions (tabs/spaces) with the smalltalk knobs. */
function toFormatOptions(lsp: FormattingOptions, s: FormatSettings): FormatOptions {
  return {
    indentSize: s.indentSize,
    insertSpaces: lsp.insertSpaces,
    cascades: s.cascades,
    keywordWrap: s.keywordWrap,
    blockStyle: s.blockStyle,
  };
}

const eolOf = (text: string): string => (text.includes('\r\n') ? '\r\n' : '\n');

const indentUnit = (lsp: FormattingOptions, s: FormatSettings): string =>
  lsp.insertSpaces ? ' '.repeat(Math.max(1, s.indentSize)) : '\t';

/** End position of the whole document, EOL-agnostic. */
function endPosition(text: string): Position {
  let line = 0;
  let lastBreak = 0;
  for (let i = 0; i < text.length; i++) {
    if (text.charCodeAt(i) === 10) {
      line += 1;
      lastBreak = i + 1;
    }
  }
  return Position.create(line, text.length - lastBreak);
}

/** Whole-document formatting: one replace edit, or none if already normalized / disabled. */
export function formatDocument(text: string, lsp: FormattingOptions, settings: FormatSettings): TextEdit[] {
  if (!settings.enable) return [];
  const formatted = formatSource(text, toFormatOptions(lsp, settings));
  if (formatted === text) return [];
  return [TextEdit.replace(Range.create(Position.create(0, 0), endPosition(text)), formatted)];
}

/** Range formatting: format the selected whole-line span standalone; edits stay within it. */
export function formatRange(
  text: string,
  range: Range,
  lsp: FormattingOptions,
  settings: FormatSettings,
): TextEdit[] {
  if (!settings.enable) return [];
  const eol = eolOf(text);
  const lines = text.split(eol);
  const startLine = Math.max(0, range.start.line);
  // A selection ending at column 0 of a later line does not include that line.
  let endLine = range.end.line;
  if (range.end.character === 0 && endLine > startLine) endLine -= 1;
  endLine = Math.min(endLine, lines.length - 1);
  if (endLine < startLine) return [];

  const span = lines.slice(startLine, endLine + 1).join(eol);
  const formatted = formatSource(span, toFormatOptions(lsp, settings));
  if (formatted === span) return [];
  return [
    TextEdit.replace(
      Range.create(Position.create(startLine, 0), Position.create(endLine, (lines[endLine] ?? '').length)),
      formatted,
    ),
  ];
}

/** On-type formatting: re-indent the current line to its block depth (dedent on `]`, indent after `[`). */
export function formatOnType(
  text: string,
  position: Position,
  ch: string,
  lsp: FormattingOptions,
  settings: FormatSettings,
): TextEdit[] {
  if (!settings.enable || !ON_TYPE_TRIGGERS.has(ch)) return [];
  const eol = eolOf(text);
  const lines = text.split(eol);
  if (position.line >= lines.length) return [];

  const lineText = lines[position.line] ?? '';
  const leading = /^[\t ]*/.exec(lineText)?.[0] ?? '';
  const want = indentUnit(lsp, settings).repeat(lineIndentDepth(text, position.line));
  if (leading === want) return [];
  return [TextEdit.replace(Range.create(Position.create(position.line, 0), Position.create(position.line, leading.length)), want)];
}
