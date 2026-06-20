// Error-tolerant lexer for GNU Smalltalk (US-411, slice 1).
//
// `tokenize()` turns a source string into a token stream plus a diagnostics
// list. It is pure (no `vscode`/`gst`), and it NEVER throws: malformed input
// (stray bytes, unterminated strings/comments) yields recovery tokens and
// diagnostics instead of an exception. Every scan branch advances the cursor by
// at least one character, so the scanner cannot loop or hang on any input.
//
// Token boundaries follow the inventory in
// docs/research/gst-syntax/01-gst-lexer-token-analysis.md. A few intentional
// decisions are documented in specs/US-411-*/plan.md (notably: a leading `-` is
// a binary selector, not part of a number; `|` is always its own token; `#(`,
// `#[`, `#{` are single combined tokens).

import {
  DiagnosticSeverity,
  TokenKind,
  type LexDiagnostic,
  type Token,
} from './token';

export interface LexResult {
  readonly tokens: Token[];
  readonly diagnostics: LexDiagnostic[];
}

/** Binary-selector characters, per `BIN_OP_CHAR` in lex.c — note `|` is excluded
 *  (a lone `|` is its own {@link TokenKind.Pipe} token; the parser decides its role). */
const BIN_OP_CHARS = '%&*+,-/<=>?@\\~';

export function tokenize(source: string): LexResult {
  return new Lexer(source).run();
}

class Lexer {
  private pos = 0;
  private line = 0;
  private character = 0;
  private readonly tokens: Token[] = [];
  private readonly diagnostics: LexDiagnostic[] = [];

  constructor(private readonly source: string) {}

  run(): LexResult {
    // Shebang: only when the file begins with `#!`. The whole first line is one token.
    if (this.source.startsWith('#!')) {
      while (this.peek() !== '' && this.peek() !== '\n' && this.peek() !== '\r') {
        this.advance();
      }
      this.push(TokenKind.Shebang, 0, 0, 0);
    }

    while (this.pos < this.source.length) {
      while (this.isWhitespace(this.peek())) {
        this.advance();
      }
      if (this.pos >= this.source.length) {
        break;
      }

      const sOff = this.pos;
      const sLine = this.line;
      const sChar = this.character;
      const c = this.peek();

      if (c === '"') {
        this.scanComment(sOff, sLine, sChar);
      } else if (c === "'") {
        this.scanString(sOff, sLine, sChar);
      } else if (c === '$') {
        this.scanChar(sOff, sLine, sChar);
      } else if (this.isDigit(c)) {
        this.scanNumber(sOff, sLine, sChar);
      } else if (this.isIdentStart(c)) {
        this.scanIdentifier(sOff, sLine, sChar);
      } else if (c === '#') {
        this.scanHash(sOff, sLine, sChar);
      } else if (c === ':') {
        this.scanColon(sOff, sLine, sChar);
      } else if (c === '_') {
        this.advance();
        this.push(TokenKind.Assign, sOff, sLine, sChar);
      } else if (this.isBinOpChar(c)) {
        this.scanBinaryOp(sOff, sLine, sChar);
      } else {
        this.scanSingleChar(sOff, sLine, sChar, c);
      }
    }

    this.push(TokenKind.EOF, this.pos, this.line, this.character);
    return { tokens: this.tokens, diagnostics: this.diagnostics };
  }

  // --- Scanners --------------------------------------------------------------

  private scanComment(sOff: number, sLine: number, sChar: number): void {
    this.advance(); // opening "
    for (;;) {
      const c = this.peek();
      if (c === '') {
        this.diag('Unterminated comment', sOff, sLine, sChar);
        break;
      }
      if (c === '"') {
        if (this.peek(1) === '"') {
          this.advance();
          this.advance();
          continue; // "" is an escaped quote inside a comment
        }
        this.advance(); // closing "
        break;
      }
      this.advance();
    }
    this.push(TokenKind.Comment, sOff, sLine, sChar);
  }

  private scanString(sOff: number, sLine: number, sChar: number): void {
    this.advance(); // opening '
    for (;;) {
      const c = this.peek();
      if (c === '') {
        this.diag('Unterminated string literal', sOff, sLine, sChar);
        break;
      }
      if (c === "'") {
        if (this.peek(1) === "'") {
          this.advance();
          this.advance();
          continue; // '' is an escaped quote
        }
        this.advance(); // closing '
        break;
      }
      this.advance();
    }
    this.push(TokenKind.String, sOff, sLine, sChar);
  }

  private scanChar(sOff: number, sLine: number, sChar: number): void {
    this.advance(); // '$'
    const c = this.peek();
    if (c === '') {
      this.diag('Unterminated character literal', sOff, sLine, sChar);
      this.push(TokenKind.Error, sOff, sLine, sChar);
      return;
    }
    if (c === '<' && this.isDigit(this.peek(1))) {
      // Code-point form: $<65>, $<16r42>.
      this.advance(); // '<'
      while (this.peek() !== '>' && this.peek() !== '' && this.peek() !== '\n') {
        this.advance();
      }
      if (this.peek() === '>') {
        this.advance();
      } else {
        this.diag('Unterminated character code literal', sOff, sLine, sChar);
      }
    } else {
      this.advance(); // the single character (incl. $, space, brackets, '<')
    }
    this.push(TokenKind.Char, sOff, sLine, sChar);
  }

  private scanNumber(sOff: number, sLine: number, sChar: number): void {
    let kind = TokenKind.Integer;
    this.readDigits(10); // leading decimal run (also the radix specifier)
    let radix = 10;

    // Radix prefix: <digits> 'r' <radix-digit>, e.g. 16rFF, 2r1011, 36rSMALLTALK.
    if (this.peek() === 'r' || this.peek() === 'R') {
      const parsed = parseInt(this.source.slice(sOff, this.pos), 10);
      const base = Number.isFinite(parsed) && parsed >= 2 && parsed <= 36 ? parsed : 10;
      if (this.isRadixDigit(this.peek(1), base)) {
        radix = base;
        this.advance(); // 'r'
        this.readDigits(radix);
      }
    }

    // Fractional part: only when a radix-digit follows the '.', so `1.` stays an
    // integer followed by a Period.
    if (this.peek() === '.' && this.isRadixDigit(this.peek(1), radix)) {
      kind = TokenKind.Float;
      this.advance(); // '.'
      this.readDigits(radix);
    }

    // Exponent / float suffix: e | d | q, when not a valid digit of the radix.
    // A bare marker (`1.0e`, `123e`) is a valid type suffix with no exponent.
    const expCh = this.peek();
    if (
      (expCh === 'e' || expCh === 'E' || expCh === 'd' || expCh === 'D' || expCh === 'q' || expCh === 'Q') &&
      !this.isRadixDigit(expCh, radix)
    ) {
      kind = TokenKind.Float;
      this.advance(); // marker
      if ((this.peek() === '+' || this.peek() === '-') && this.isDigit(this.peek(1))) {
        this.advance(); // exponent sign
        this.readDigits(10);
      } else if (this.isDigit(this.peek())) {
        this.readDigits(10);
      }
    }

    // Scaled decimal: 's' [scale], when 's' is not a valid digit of the radix.
    if ((this.peek() === 's' || this.peek() === 'S') && !this.isRadixDigit(this.peek(), radix)) {
      kind = TokenKind.ScaledDecimal;
      this.advance(); // 's'
      if (this.isDigit(this.peek())) {
        this.readDigits(10);
      }
    }

    this.push(kind, sOff, sLine, sChar);
  }

  private scanIdentifier(sOff: number, sLine: number, sChar: number): void {
    while (this.isIdentChar(this.peek())) {
      this.advance();
    }
    // Keyword: a trailing colon that is not part of `:=` (assignment) or `::` (scope).
    if (this.peek() === ':' && this.peek(1) !== '=' && this.peek(1) !== ':') {
      this.advance(); // ':'
      this.push(TokenKind.Keyword, sOff, sLine, sChar);
    } else {
      this.push(TokenKind.Identifier, sOff, sLine, sChar);
    }
  }

  private scanHash(sOff: number, sLine: number, sChar: number): void {
    this.advance(); // '#'
    const c = this.peek();
    if (c === '(') {
      this.advance();
      this.push(TokenKind.HashParen, sOff, sLine, sChar);
    } else if (c === '[') {
      this.advance();
      this.push(TokenKind.HashBracket, sOff, sLine, sChar);
    } else if (c === '{') {
      this.advance();
      this.push(TokenKind.HashBrace, sOff, sLine, sChar);
    } else if (c === "'") {
      this.scanQuotedSymbol(sOff, sLine, sChar);
    } else if (this.isIdentChar(c)) {
      // Identifier/keyword symbol: #foo, #at:put:, #123 (digit-leading symbols are
      // unusual but accepted, per fixture 02).
      while (this.isIdentChar(this.peek())) {
        this.advance();
      }
      while (this.peek() === ':') {
        this.advance();
        while (this.isIdentChar(this.peek())) {
          this.advance();
        }
      }
      this.push(TokenKind.Symbol, sOff, sLine, sChar);
    } else if (this.isSymbolBinChar(c)) {
      while (this.isSymbolBinChar(this.peek())) {
        this.advance();
      }
      this.push(TokenKind.Symbol, sOff, sLine, sChar);
    } else {
      this.push(TokenKind.Hash, sOff, sLine, sChar);
    }
  }

  private scanQuotedSymbol(sOff: number, sLine: number, sChar: number): void {
    this.advance(); // opening '
    for (;;) {
      const c = this.peek();
      if (c === '') {
        this.diag('Unterminated symbol literal', sOff, sLine, sChar);
        break;
      }
      if (c === "'") {
        if (this.peek(1) === "'") {
          this.advance();
          this.advance();
          continue;
        }
        this.advance();
        break;
      }
      this.advance();
    }
    this.push(TokenKind.Symbol, sOff, sLine, sChar);
  }

  private scanColon(sOff: number, sLine: number, sChar: number): void {
    if (this.peek(1) === '=') {
      this.advance();
      this.advance();
      this.push(TokenKind.Assign, sOff, sLine, sChar);
    } else if (this.peek(1) === ':') {
      this.advance();
      this.advance();
      this.push(TokenKind.Scope, sOff, sLine, sChar);
    } else {
      this.advance();
      this.push(TokenKind.Colon, sOff, sLine, sChar);
    }
  }

  private scanBinaryOp(sOff: number, sLine: number, sChar: number): void {
    while (this.isBinOpChar(this.peek())) {
      this.advance();
    }
    this.push(TokenKind.BinarySelector, sOff, sLine, sChar);
  }

  private scanSingleChar(sOff: number, sLine: number, sChar: number, c: string): void {
    this.advance();
    switch (c) {
      case '(':
        this.push(TokenKind.LParen, sOff, sLine, sChar);
        break;
      case ')':
        this.push(TokenKind.RParen, sOff, sLine, sChar);
        break;
      case '[':
        this.push(TokenKind.LBracket, sOff, sLine, sChar);
        break;
      case ']':
        this.push(TokenKind.RBracket, sOff, sLine, sChar);
        break;
      case '{':
        this.push(TokenKind.LBrace, sOff, sLine, sChar);
        break;
      case '}':
        this.push(TokenKind.RBrace, sOff, sLine, sChar);
        break;
      case '.':
        this.push(TokenKind.Period, sOff, sLine, sChar);
        break;
      case '^':
        this.push(TokenKind.Caret, sOff, sLine, sChar);
        break;
      case ';':
        this.push(TokenKind.Semicolon, sOff, sLine, sChar);
        break;
      case '|':
        this.push(TokenKind.Pipe, sOff, sLine, sChar);
        break;
      case '!':
        this.push(TokenKind.Bang, sOff, sLine, sChar);
        break;
      default:
        this.diag(`Unexpected character ${JSON.stringify(c)}`, sOff, sLine, sChar);
        this.push(TokenKind.Error, sOff, sLine, sChar);
    }
  }

  // --- Digit helpers ---------------------------------------------------------

  private readDigits(radix: number): void {
    for (;;) {
      const c = this.peek();
      if (this.isRadixDigit(c, radix)) {
        this.advance();
        continue;
      }
      // An underscore separator is consumed only between digits, so a trailing
      // `_` falls through to the assignment token.
      if (c === '_' && this.isRadixDigit(this.peek(1), radix)) {
        this.advance();
        continue;
      }
      break;
    }
  }

  private isRadixDigit(c: string, radix: number): boolean {
    if (c.length !== 1) {
      return false;
    }
    const code = c.charCodeAt(0);
    let value: number;
    if (code >= 48 && code <= 57) {
      value = code - 48; // 0-9
    } else if (code >= 97 && code <= 122) {
      value = code - 97 + 10; // a-z
    } else if (code >= 65 && code <= 90) {
      value = code - 65 + 10; // A-Z
    } else {
      return false;
    }
    return value < radix;
  }

  // --- Character classes -----------------------------------------------------

  private isWhitespace(c: string): boolean {
    return c === ' ' || c === '\t' || c === '\n' || c === '\r' || c === '\f' || c === '\v';
  }

  private isDigit(c: string): boolean {
    return c >= '0' && c <= '9';
  }

  private isIdentStart(c: string): boolean {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
  }

  private isIdentChar(c: string): boolean {
    return this.isIdentStart(c) || this.isDigit(c) || c === '_';
  }

  private isBinOpChar(c: string): boolean {
    return c.length === 1 && BIN_OP_CHARS.includes(c);
  }

  private isSymbolBinChar(c: string): boolean {
    return this.isBinOpChar(c) || c === '|';
  }

  // --- Cursor ----------------------------------------------------------------

  private peek(offset = 0): string {
    return this.source[this.pos + offset] ?? '';
  }

  private advance(): string {
    const ch = this.source[this.pos] ?? '';
    this.pos++;
    if (ch === '\n') {
      this.line++;
      this.character = 0;
    } else if (ch === '\r' && this.source[this.pos] !== '\n') {
      this.line++;
      this.character = 0;
    } else {
      this.character++;
    }
    return ch;
  }

  private push(kind: TokenKind, sOff: number, sLine: number, sChar: number): void {
    this.tokens.push({
      kind,
      text: this.source.slice(sOff, this.pos),
      start: sOff,
      end: this.pos,
      startPos: { line: sLine, character: sChar },
      endPos: { line: this.line, character: this.character },
    });
  }

  private diag(message: string, sOff: number, sLine: number, sChar: number): void {
    this.diagnostics.push({
      message,
      severity: DiagnosticSeverity.Error,
      start: sOff,
      end: this.pos,
      startPos: { line: sLine, character: sChar },
      endPos: { line: this.line, character: this.character },
    });
  }
}
