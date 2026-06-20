// Recursive-descent parser for GNU Smalltalk expressions (US-411, slice 2).
//
// `parse()` turns source into an AST (with positions) plus a diagnostics list.
// Like the lexer it NEVER throws: a malformed statement records a diagnostic,
// yields an `Error` node, and the statement loop synchronizes on `.`/`]`/`!`/EOF.
// Every loop makes progress (parsePrimary always consumes >= 1 token, even on
// error), so no input can hang the parser.
//
// Precedence climb (tightest first): primary -> unary -> binary -> keyword ->
// cascade -> assignment. See specs/US-411-*/plan.md ("Slice 2") for the design
// decisions, notably the block-header `looksLikeTemporaries` lookahead.

import { tokenize } from './lexer';
import {
  DiagnosticSeverity,
  TokenKind,
  type LexDiagnostic,
  type Token,
} from './token';
import {
  NodeKind,
  type AssignmentNode,
  type BlockNode,
  type CascadeNode,
  type CascadeReceiverNode,
  type LiteralKind,
  type MessageNode,
  type MessageType,
  type NameRef,
  type Node,
  type ProgramNode,
  type VariableNode,
} from './ast';

const NUMBER_KINDS = new Set<TokenKind>([
  TokenKind.Integer,
  TokenKind.Float,
  TokenKind.ScaledDecimal,
]);

export interface ParseResult {
  readonly ast: ProgramNode;
  readonly diagnostics: LexDiagnostic[];
}

const LITERAL_KIND: Partial<Record<TokenKind, LiteralKind>> = {
  [TokenKind.Integer]: 'integer',
  [TokenKind.Float]: 'float',
  [TokenKind.ScaledDecimal]: 'scaledDecimal',
  [TokenKind.String]: 'string',
  [TokenKind.Symbol]: 'symbol',
  [TokenKind.Char]: 'character',
};

export function parse(source: string): ParseResult {
  const { tokens, diagnostics } = tokenize(source);
  // Comments are trivia; positions still come from the real (non-comment) tokens.
  const stream = tokens.filter((t) => t.kind !== TokenKind.Comment);
  return new Parser(stream, diagnostics).parseProgram();
}

class Parser {
  private index = 0;
  private prev: Token;

  constructor(
    private readonly tokens: Token[],
    private readonly diagnostics: LexDiagnostic[],
  ) {
    this.prev = tokens[0] as Token; // stream always has at least EOF
  }

  parseProgram(): ParseResult {
    const startTok = this.current();
    const { temporaries, statements } = this.parseSequenceBody(TokenKind.EOF);
    const ast: ProgramNode = {
      kind: NodeKind.Program,
      temporaries,
      statements,
      ...this.span(startTok),
    };
    return { ast, diagnostics: this.diagnostics };
  }

  // --- Statement sequences ---------------------------------------------------

  /** `[ '|' temps '|' ] statement ( '.' statement )*` up to `stop`. */
  private parseSequenceBody(stop: TokenKind): { temporaries: NameRef[]; statements: Node[] } {
    const temporaries = this.parseTemporaries();
    const statements: Node[] = [];
    while (!this.atEnd() && !this.at(stop)) {
      // Tolerate stray separators: empty statements and chunk bangs.
      if (this.at(TokenKind.Period) || this.at(TokenKind.Bang)) {
        this.advance();
        continue;
      }
      // REPL-style sources redeclare `| temps |` between statements; merge them.
      if (this.looksLikeTemporaries()) {
        temporaries.push(...this.parseTemporaries());
        continue;
      }
      const stmt = this.parseStatement();
      statements.push(stmt);
      if (this.at(TokenKind.Period)) {
        this.advance();
      } else if (!this.atEnd() && !this.at(stop) && !this.at(TokenKind.Bang)) {
        // A statement should be followed by `.`, the stop token, or EOF.
        if (stmt.kind !== NodeKind.Error) {
          this.diag('Expected "." after statement', this.current());
        }
        this.synchronize(stop);
      }
    }
    return { temporaries, statements };
  }

  private parseTemporaries(): NameRef[] {
    if (!this.looksLikeTemporaries()) {
      return [];
    }
    this.advance(); // opening |
    const temps: NameRef[] = [];
    while (this.at(TokenKind.Identifier)) {
      temps.push(this.nameRef(this.current()));
      this.advance();
    }
    if (this.at(TokenKind.Pipe)) {
      this.advance(); // closing |
    }
    return temps;
  }

  /** True when the current `|` opens a `| id* |` temporaries section (closing `|` confirmed). */
  private looksLikeTemporaries(): boolean {
    if (!this.at(TokenKind.Pipe)) {
      return false;
    }
    let k = 1;
    while (this.peek(k).kind === TokenKind.Identifier) {
      k++;
    }
    return this.peek(k).kind === TokenKind.Pipe;
  }

  private synchronize(stop: TokenKind): void {
    while (
      !this.atEnd() &&
      !this.at(stop) &&
      !this.at(TokenKind.Period) &&
      !this.at(TokenKind.Bang) &&
      !this.at(TokenKind.RBracket)
    ) {
      this.advance();
    }
  }

  private parseStatement(): Node {
    if (this.at(TokenKind.Caret)) {
      const startTok = this.current();
      this.advance(); // ^
      const value = this.parseExpression();
      return { kind: NodeKind.Return, value, ...this.span(startTok) };
    }
    return this.parseExpression();
  }

  // --- Expressions (precedence climb) ----------------------------------------

  private parseExpression(): Node {
    // Assignment: `identifier ':=' expression` (right-associative; also legacy `_`).
    if (this.at(TokenKind.Identifier) && this.peek(1).kind === TokenKind.Assign) {
      const idTok = this.current();
      this.advance(); // identifier
      const target: VariableNode = { kind: NodeKind.Variable, name: idTok.text, ...this.range(idTok) };
      this.advance(); // := or _
      const value = this.parseExpression();
      const node: AssignmentNode = { kind: NodeKind.Assignment, target, value, ...this.span(idTok) };
      return node;
    }
    return this.parseCascade();
  }

  private parseCascade(): Node {
    const startTok = this.current();
    const expr = this.parseKeywordMessage();
    if (!this.at(TokenKind.Semicolon) || expr.kind !== NodeKind.Message) {
      return expr;
    }
    // The cascade receiver is the receiver of the first message's outermost send.
    const receiver = expr.receiver;
    const marker = this.cascadeReceiver(receiver);
    // Segment 0 is the first message re-rooted at the marker; the rest are fresh chains.
    const messages: Node[] = [{ ...expr, receiver: marker }];
    while (this.at(TokenKind.Semicolon)) {
      this.advance(); // ;
      messages.push(this.parseCascadeSegment(this.cascadeReceiver(receiver)));
    }
    const node: CascadeNode = { kind: NodeKind.Cascade, receiver, messages, ...this.span(startTok) };
    return node;
  }

  /** One `;` segment: a full unary > binary > keyword message chain on the implicit receiver. */
  private parseCascadeSegment(receiver: Node): Node {
    let node = receiver;
    while (this.at(TokenKind.Identifier)) {
      const selector = this.current().text;
      this.advance();
      node = this.message(node, selector, 'unary', []);
    }
    while (this.at(TokenKind.BinarySelector) || this.at(TokenKind.Pipe)) {
      const selector = this.current().text;
      this.advance();
      node = this.message(node, selector, 'binary', [this.parseUnaryMessage()]);
    }
    if (this.at(TokenKind.Keyword)) {
      let selector = '';
      const args: Node[] = [];
      while (this.at(TokenKind.Keyword)) {
        selector += this.current().text;
        this.advance();
        args.push(this.parseBinaryMessage());
      }
      node = this.message(node, selector, 'keyword', args);
    }
    if (node === receiver) {
      this.diag('Expected a message selector after ";"', this.current());
    }
    return node;
  }

  private cascadeReceiver(of: Node): CascadeReceiverNode {
    return {
      kind: NodeKind.CascadeReceiver,
      start: of.end,
      end: of.end,
      startPos: of.endPos,
      endPos: of.endPos,
    };
  }

  private parseKeywordMessage(): Node {
    let receiver = this.parseBinaryMessage();
    if (this.at(TokenKind.Keyword)) {
      let selector = '';
      const args: Node[] = [];
      while (this.at(TokenKind.Keyword)) {
        selector += this.current().text;
        this.advance();
        args.push(this.parseBinaryMessage());
      }
      receiver = this.message(receiver, selector, 'keyword', args);
    }
    return receiver;
  }

  private parseBinaryMessage(): Node {
    let receiver = this.parseUnaryMessage();
    // A lone `|` acts as the bitOr binary selector in expression position.
    while (this.at(TokenKind.BinarySelector) || this.at(TokenKind.Pipe)) {
      const selector = this.current().text;
      this.advance();
      const arg = this.parseUnaryMessage();
      receiver = this.message(receiver, selector, 'binary', [arg]);
    }
    return receiver;
  }

  private parseUnaryMessage(): Node {
    let receiver = this.parsePrimary();
    while (this.at(TokenKind.Identifier)) {
      const selector = this.current().text;
      this.advance();
      receiver = this.message(receiver, selector, 'unary', []);
    }
    return receiver;
  }

  // --- Primaries -------------------------------------------------------------

  private parsePrimary(): Node {
    const t = this.current();
    // Negative numeric literal: the lexer emits `-` as a binary selector, so in primary
    // position `-` immediately before a number folds into a signed literal.
    if (t.kind === TokenKind.BinarySelector && t.text === '-' && NUMBER_KINDS.has(this.peek(1).kind)) {
      const numTok = this.peek(1);
      this.advance(); // -
      this.advance(); // number
      const literalKind = LITERAL_KIND[numTok.kind] as LiteralKind;
      return {
        kind: NodeKind.Literal,
        literalKind,
        value: `-${numTok.text}`,
        start: t.start,
        end: numTok.end,
        startPos: t.startPos,
        endPos: numTok.endPos,
      };
    }
    const literalKind = LITERAL_KIND[t.kind];
    if (literalKind) {
      this.advance();
      return { kind: NodeKind.Literal, literalKind, value: t.text, ...this.range(t) };
    }
    switch (t.kind) {
      case TokenKind.Identifier:
        this.advance();
        return { kind: NodeKind.Variable, name: t.text, ...this.range(t) };
      case TokenKind.LParen:
        return this.parseParenthesized();
      case TokenKind.HashParen:
        return this.parseCollection(NodeKind.LiteralArray, TokenKind.RParen);
      case TokenKind.HashBracket:
        return this.parseCollection(NodeKind.ByteArray, TokenKind.RBracket);
      case TokenKind.LBrace:
        return this.parseDynamicArray();
      case TokenKind.LBracket:
        return this.parseBlock();
      default:
        this.advance(); // consume the offending token to guarantee progress
        this.diag(`Unexpected ${t.kind} "${t.text}"`, t);
        return { kind: NodeKind.Error, message: `Unexpected ${t.kind}`, ...this.range(t) };
    }
  }

  private parseParenthesized(): Node {
    this.advance(); // (
    const inner = this.parseExpression();
    if (this.at(TokenKind.RParen)) {
      this.advance();
    } else {
      this.diag('Expected ")"', this.current());
    }
    return inner;
  }

  /** `#( … )` literal array or `#[ … ]` byte array — elements are literal tokens. */
  private parseCollection(kind: NodeKind.LiteralArray | NodeKind.ByteArray, close: TokenKind): Node {
    const startTok = this.current();
    this.advance(); // #( or #[
    const elements: Node[] = [];
    while (!this.atEnd() && !this.at(close)) {
      elements.push(this.parseLiteralElement());
    }
    if (this.at(close)) {
      this.advance();
    } else {
      this.diag(`Expected "${close === TokenKind.RParen ? ')' : ']'}"`, this.current());
    }
    return kind === NodeKind.LiteralArray
      ? { kind: NodeKind.LiteralArray, elements, ...this.span(startTok) }
      : { kind: NodeKind.ByteArray, elements, ...this.span(startTok) };
  }

  /** An element inside a literal `#( … )` / `#[ … ]`: a literal, a bareword (treated as a
   *  symbol), an operator (symbol), or a nested literal collection. Always consumes >= 1 token. */
  private parseLiteralElement(): Node {
    const t = this.current();
    const literalKind = LITERAL_KIND[t.kind];
    if (literalKind) {
      this.advance();
      return { kind: NodeKind.Literal, literalKind, value: t.text, ...this.range(t) };
    }
    switch (t.kind) {
      case TokenKind.Identifier:
      case TokenKind.Keyword:
      case TokenKind.BinarySelector:
      case TokenKind.Pipe:
        // Inside a literal array these are symbol elements.
        this.advance();
        return { kind: NodeKind.Literal, literalKind: 'symbol', value: t.text, ...this.range(t) };
      case TokenKind.HashParen:
      case TokenKind.LParen:
        return this.parseCollection(NodeKind.LiteralArray, TokenKind.RParen);
      case TokenKind.HashBracket:
        return this.parseCollection(NodeKind.ByteArray, TokenKind.RBracket);
      default:
        this.advance();
        this.diag(`Unexpected ${t.kind} in literal array`, t);
        return { kind: NodeKind.Error, message: `Unexpected ${t.kind}`, ...this.range(t) };
    }
  }

  /** `{ expr. expr. … }` — elements are full expressions. */
  private parseDynamicArray(): Node {
    const startTok = this.current();
    this.advance(); // {
    const elements: Node[] = [];
    while (!this.atEnd() && !this.at(TokenKind.RBrace)) {
      if (this.at(TokenKind.Period)) {
        this.advance();
        continue;
      }
      elements.push(this.parseExpression());
      if (this.at(TokenKind.Period)) {
        this.advance();
      } else if (!this.at(TokenKind.RBrace) && !this.atEnd()) {
        this.diag('Expected "." or "}" in dynamic array', this.current());
        this.synchronize(TokenKind.RBrace);
      }
    }
    if (this.at(TokenKind.RBrace)) {
      this.advance();
    } else {
      this.diag('Expected "}"', this.current());
    }
    return { kind: NodeKind.DynamicArray, elements, ...this.span(startTok) };
  }

  /** `[ (':' id)* '|'? '| temps |'? statements ]`. */
  private parseBlock(): Node {
    const startTok = this.current();
    this.advance(); // [
    const parameters: NameRef[] = [];
    while (this.at(TokenKind.Colon) && this.peek(1).kind === TokenKind.Identifier) {
      this.advance(); // :
      parameters.push(this.nameRef(this.current()));
      this.advance(); // identifier
    }
    if (parameters.length > 0 && this.at(TokenKind.Pipe)) {
      this.advance(); // argument terminator |
      // A doubled `||` (args/body separator with no temporaries) leaves a stray pipe.
      if (this.at(TokenKind.Pipe) && !this.looksLikeTemporaries()) {
        this.advance();
      }
    }
    const { temporaries, statements } = this.parseSequenceBody(TokenKind.RBracket);
    if (this.at(TokenKind.RBracket)) {
      this.advance();
    } else {
      this.diag('Expected "]" to close block', this.current());
    }
    const node: BlockNode = {
      kind: NodeKind.Block,
      parameters,
      temporaries,
      statements,
      ...this.span(startTok),
    };
    return node;
  }

  // --- Node + token helpers --------------------------------------------------

  private message(receiver: Node, selector: string, messageType: MessageType, args: Node[]): MessageNode {
    return {
      kind: NodeKind.Message,
      receiver,
      selector,
      messageType,
      arguments: args,
      start: receiver.start,
      end: this.prev.end,
      startPos: receiver.startPos,
      endPos: this.prev.endPos,
    };
  }

  private nameRef(tok: Token): NameRef {
    return { name: tok.text, ...this.range(tok) };
  }

  private range(tok: Token): { start: number; end: number; startPos: Token['startPos']; endPos: Token['endPos'] } {
    return { start: tok.start, end: tok.end, startPos: tok.startPos, endPos: tok.endPos };
  }

  private span(startTok: Token): { start: number; end: number; startPos: Token['startPos']; endPos: Token['endPos'] } {
    return {
      start: startTok.start,
      end: this.prev.end,
      startPos: startTok.startPos,
      endPos: this.prev.endPos,
    };
  }

  private current(): Token {
    return this.tokens[this.index] as Token;
  }

  private peek(offset: number): Token {
    const i = Math.min(this.index + offset, this.tokens.length - 1);
    return this.tokens[i] as Token;
  }

  private at(kind: TokenKind): boolean {
    return this.current().kind === kind;
  }

  private atEnd(): boolean {
    return this.current().kind === TokenKind.EOF;
  }

  private advance(): Token {
    const t = this.current();
    if (this.index < this.tokens.length - 1) {
      this.index++;
    }
    this.prev = t;
    return t;
  }

  private diag(message: string, tok: Token): void {
    this.diagnostics.push({
      message,
      severity: DiagnosticSeverity.Error,
      start: tok.start,
      end: tok.end,
      startPos: tok.startPos,
      endPos: tok.endPos,
    });
  }
}
