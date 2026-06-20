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
  type DefinitionKind,
  type LiteralKind,
  type MessageNode,
  type MessageType,
  type MethodDefinitionNode,
  type NameRef,
  type Node,
  type PragmaNode,
  type ProgramNode,
  type VariableNode,
} from './ast';

const NUMBER_KINDS = new Set<TokenKind>([
  TokenKind.Integer,
  TokenKind.Float,
  TokenKind.ScaledDecimal,
]);

/** GST definitions are not `.`-separated from what follows them. */
function isDefinition(node: Node): boolean {
  return node.kind === NodeKind.Definition || node.kind === NodeKind.MethodDefinition;
}

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
      } else if (!this.atEnd() && !this.at(stop) && !this.at(TokenKind.Bang) && !isDefinition(stmt)) {
        // A plain statement should be followed by `.`, the stop token, or EOF.
        // GST definitions (`… [ … ]`) are not `.`-separated from what follows.
        if (stmt.kind !== NodeKind.Error) {
          this.diag('Expected "." after statement', this.current());
        }
        this.synchronize(stop);
      }
      // GST chunk container: `Class methodsFor: '…'! method ! … ! !` (top level only).
      if (stop === TokenKind.EOF && this.isMethodsForHeader(stmt) && this.at(TokenKind.Bang)) {
        statements[statements.length - 1] = this.parseMethodsForSection(stmt);
      }
    }
    return { temporaries, statements };
  }

  // --- GST chunk container (AC3) ---------------------------------------------

  private isMethodsForHeader(stmt: Node): boolean {
    return (
      stmt.kind === NodeKind.Message &&
      stmt.messageType === 'keyword' &&
      stmt.selector.includes('methodsFor:')
    );
  }

  private parseMethodsForSection(header: Node): Node {
    let target: Node = header;
    let classSide = false;
    if (header.kind === NodeKind.Message) {
      const recv = header.receiver;
      if (recv.kind === NodeKind.Message && recv.messageType === 'unary' && recv.selector === 'class') {
        classSide = true;
        target = recv.receiver;
      } else {
        target = recv;
      }
    }
    this.advance(); // '!' after the header
    const body: Node[] = [];
    while (!this.atEnd()) {
      if (this.at(TokenKind.Bang)) {
        this.advance(); // chunk separator / empty terminating chunk
        continue;
      }
      // The section ends at the first chunk that reads as a doit, not a method pattern.
      if (!this.chunkLooksLikeMethod()) {
        break;
      }
      body.push(this.parseChunkMethod(target, classSide));
    }
    return {
      kind: NodeKind.Definition,
      definitionKind: 'methodsFor',
      definer: header,
      body,
      start: header.start,
      end: this.prev.end,
      startPos: header.startPos,
      endPos: this.prev.endPos,
    };
  }

  /** A chunk is a method when it starts with a method pattern, not a `receiver keyword:` doit.
   *  (Chunk method bodies have no brackets, so this can't use the `[` lookahead.) */
  private chunkLooksLikeMethod(): boolean {
    if (this.at(TokenKind.Keyword)) {
      return true;
    }
    if (this.at(TokenKind.BinarySelector) && this.peek(1).kind === TokenKind.Identifier) {
      return true;
    }
    return this.at(TokenKind.Identifier) && this.peek(1).kind !== TokenKind.Keyword;
  }

  private parseChunkMethod(target: Node, classSide: boolean): Node {
    const startTok = this.current();
    const pattern = this.parseMethodPattern();
    const { temporaries, statements } = this.parseSequenceBody(TokenKind.Bang);
    const node: MethodDefinitionNode = {
      kind: NodeKind.MethodDefinition,
      target,
      classSide,
      selector: pattern.selector,
      messageType: pattern.messageType,
      parameters: pattern.parameters,
      pragmas: [],
      temporaries,
      statements,
      ...this.span(startTok),
    };
    return node;
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
    // GST brace container: `Class [class] >> pattern [ … ]` method definition.
    if (this.looksLikeMethodDef()) {
      return this.parseMethodDefinition();
    }
    if (this.at(TokenKind.Caret)) {
      const startTok = this.current();
      this.advance(); // ^
      const value = this.parseExpression();
      return { kind: NodeKind.Return, value, ...this.span(startTok) };
    }
    const expr = this.parseExpression();
    // GST brace container: `<expr> [ … ]` scoped definition (subclass / namespace / extend / class scope).
    if (this.at(TokenKind.LBracket)) {
      return this.parseScopedDefinition(expr);
    }
    return expr;
  }

  // --- GST brace container (AC3) ---------------------------------------------

  /** Lookahead for `Identifier [class] >> pattern [` — a scoped method definition.
   *  Requires the trailing `[`, so `self >> method methodCategory: …` (a `>>` expression)
   *  is not misread as a definition. */
  private looksLikeMethodDef(): boolean {
    if (!this.at(TokenKind.Identifier)) {
      return false;
    }
    let k = 1;
    if (this.peek(k).kind === TokenKind.Identifier && this.peek(k).text === 'class') {
      k++;
    }
    if (!(this.peek(k).kind === TokenKind.BinarySelector && this.peek(k).text === '>>')) {
      return false;
    }
    return this.methodPatternEndsWithBracket(k + 1);
  }

  /** True if a method pattern starting at lookahead offset `k` is immediately followed by `[`. */
  private methodPatternEndsWithBracket(k: number): boolean {
    const t = this.peek(k);
    if (t.kind === TokenKind.Keyword) {
      while (this.peek(k).kind === TokenKind.Keyword) {
        k++;
        if (this.peek(k).kind !== TokenKind.Identifier) {
          return false;
        }
        k++;
      }
      return this.peek(k).kind === TokenKind.LBracket;
    }
    if (t.kind === TokenKind.BinarySelector || t.kind === TokenKind.Pipe) {
      if (this.peek(k + 1).kind !== TokenKind.Identifier) {
        return false;
      }
      return this.peek(k + 2).kind === TokenKind.LBracket;
    }
    if (t.kind === TokenKind.Identifier) {
      return this.peek(k + 1).kind === TokenKind.LBracket;
    }
    return false;
  }

  private parseMethodDefinition(): Node {
    const startTok = this.current();
    const target: VariableNode = { kind: NodeKind.Variable, name: startTok.text, ...this.range(startTok) };
    this.advance(); // class name
    let classSide = false;
    if (this.at(TokenKind.Identifier) && this.current().text === 'class') {
      classSide = true;
      this.advance();
    }
    this.advance(); // '>>' (guaranteed by looksLikeMethodDef)
    const pattern = this.parseMethodPattern();
    const { pragmas, temporaries, statements } = this.parseMethodBody();
    const node: MethodDefinitionNode = {
      kind: NodeKind.MethodDefinition,
      target,
      classSide,
      selector: pattern.selector,
      messageType: pattern.messageType,
      parameters: pattern.parameters,
      pragmas,
      temporaries,
      statements,
      ...this.span(startTok),
    };
    return node;
  }

  /** Lookahead for a short-form method `pattern [ … ]` (no `Class >>`), valid inside a body. */
  private looksLikeShortMethodDef(): boolean {
    return this.methodPatternEndsWithBracket(0);
  }

  private parseShortMethodDefinition(): Node {
    const startTok = this.current();
    const pattern = this.parseMethodPattern();
    const { pragmas, temporaries, statements } = this.parseMethodBody();
    const node: MethodDefinitionNode = {
      kind: NodeKind.MethodDefinition,
      classSide: false,
      selector: pattern.selector,
      messageType: pattern.messageType,
      parameters: pattern.parameters,
      pragmas,
      temporaries,
      statements,
      ...this.span(startTok),
    };
    return node;
  }

  /** A method pattern: unary `sel`, binary `+ arg`, or keyword `k1: a1 k2: a2 …`. */
  private parseMethodPattern(): { selector: string; messageType: MessageType; parameters: NameRef[] } {
    const parameters: NameRef[] = [];
    if (this.at(TokenKind.Keyword)) {
      let selector = '';
      while (this.at(TokenKind.Keyword)) {
        selector += this.current().text;
        this.advance();
        if (this.at(TokenKind.Identifier)) {
          parameters.push(this.nameRef(this.current()));
          this.advance();
        } else {
          this.diag('Expected a parameter name in keyword method pattern', this.current());
        }
      }
      return { selector, messageType: 'keyword', parameters };
    }
    if (this.at(TokenKind.BinarySelector) || this.at(TokenKind.Pipe)) {
      const selector = this.current().text;
      this.advance();
      if (this.at(TokenKind.Identifier)) {
        parameters.push(this.nameRef(this.current()));
        this.advance();
      } else {
        this.diag('Expected a parameter name in binary method pattern', this.current());
      }
      return { selector, messageType: 'binary', parameters };
    }
    if (this.at(TokenKind.Identifier)) {
      const selector = this.current().text;
      this.advance();
      return { selector, messageType: 'unary', parameters };
    }
    this.diag('Expected a method selector pattern', this.current());
    return { selector: '', messageType: 'unary', parameters };
  }

  private parseMethodBody(): { pragmas: PragmaNode[]; temporaries: NameRef[]; statements: Node[] } {
    const pragmas: PragmaNode[] = [];
    const temporaries: NameRef[] = [];
    if (this.at(TokenKind.LBracket)) {
      this.advance();
    } else {
      this.diag('Expected "[" to open method body', this.current());
    }
    // A method header interleaves pragmas and a `| temps |` declaration in any order.
    while (this.atPragma() || this.looksLikeTemporaries()) {
      if (this.atPragma()) {
        pragmas.push(this.parsePragma());
      } else {
        temporaries.push(...this.parseTemporaries());
      }
    }
    const { temporaries: bodyTemps, statements } = this.parseSequenceBody(TokenKind.RBracket);
    temporaries.push(...bodyTemps);
    if (this.at(TokenKind.RBracket)) {
      this.advance();
    } else {
      this.diag('Expected "]" to close method body', this.current());
    }
    return { pragmas, temporaries, statements };
  }

  private parseScopedDefinition(definer: Node): Node {
    this.advance(); // '['
    const body = this.parseClassBody();
    if (this.at(TokenKind.RBracket)) {
      this.advance();
    } else {
      this.diag('Expected "]" to close definition', this.current());
    }
    const { definitionKind, name } = this.classifyDefiner(definer);
    return {
      kind: NodeKind.Definition,
      definitionKind,
      definer,
      ...(name !== undefined ? { name } : {}),
      body,
      start: definer.start,
      end: this.prev.end,
      startPos: definer.startPos,
      endPos: this.prev.endPos,
    };
  }

  /** Class-body items: instance-var decls, pragmas, method/nested definitions, statements. */
  private parseClassBody(): Node[] {
    const items: Node[] = [];
    while (!this.atEnd() && !this.at(TokenKind.RBracket)) {
      if (this.at(TokenKind.Period) || this.at(TokenKind.Bang)) {
        this.advance();
        continue;
      }
      if (this.looksLikeTemporaries()) {
        const startTok = this.current();
        const declared = this.parseTemporaries();
        items.push({ kind: NodeKind.InstanceVariables, names: declared, ...this.span(startTok) });
        continue;
      }
      if (this.atPragma()) {
        items.push(this.parsePragma());
        continue;
      }
      if (this.looksLikeMethodDef()) {
        items.push(this.parseMethodDefinition());
        continue;
      }
      // Short-form method definition inside a body: `pattern [ … ]` (no `Class >>`).
      if (this.looksLikeShortMethodDef()) {
        items.push(this.parseShortMethodDefinition());
        continue;
      }
      const expr = this.parseExpression();
      if (this.at(TokenKind.LBracket)) {
        items.push(this.parseScopedDefinition(expr));
      } else {
        items.push(expr);
        if (this.at(TokenKind.Period)) {
          this.advance(); // class-body items need no `.`, but tolerate it
        }
      }
    }
    return items;
  }

  private atPragma(): boolean {
    if (!(this.at(TokenKind.BinarySelector) && this.current().text === '<')) {
      return false;
    }
    // A bare `<` opens an attribute (`<primitive: …>`, `<category: …>`, `<gst.x: …>`,
    // `<reentrant>`), unless it is the binary method pattern `< arg [ … ]`.
    return !(this.peek(1).kind === TokenKind.Identifier && this.peek(2).kind === TokenKind.LBracket);
  }

  private parsePragma(): PragmaNode {
    const startTok = this.current();
    this.advance(); // '<'
    let selector = '';
    const args: Node[] = [];
    while (
      !this.atEnd() &&
      !this.at(TokenKind.RBracket) &&
      !(this.at(TokenKind.BinarySelector) && this.current().text === '>')
    ) {
      if (this.at(TokenKind.Keyword)) {
        selector += this.current().text;
        this.advance();
        args.push(this.parsePrimary()); // primary only, so the closing '>' is not consumed
      } else if (this.at(TokenKind.Identifier) && selector === '' && args.length === 0) {
        selector = this.current().text; // unary pragma name, e.g. <exceptionHandlerSearch>
        this.advance();
      } else {
        this.advance(); // tolerate namespace qualifiers / stray tokens
      }
    }
    if (this.at(TokenKind.BinarySelector) && this.current().text === '>') {
      this.advance();
    } else {
      this.diag('Expected ">" to close attribute', this.current());
    }
    return { kind: NodeKind.Pragma, selector, arguments: args, ...this.span(startTok) };
  }

  private classifyDefiner(definer: Node): { definitionKind: DefinitionKind; name?: string } {
    if (definer.kind === NodeKind.Message) {
      const name = this.symbolName(definer.arguments);
      if (definer.messageType === 'keyword') {
        if (definer.selector.includes('subclass:')) {
          return name !== undefined ? { definitionKind: 'subclass', name } : { definitionKind: 'subclass' };
        }
        if (definer.selector === 'current:') {
          return name !== undefined ? { definitionKind: 'namespace', name } : { definitionKind: 'namespace' };
        }
      } else if (definer.messageType === 'unary') {
        if (definer.selector === 'extend') {
          return { definitionKind: 'extend' };
        }
        if (definer.selector === 'class') {
          return { definitionKind: 'classScope' };
        }
      }
    }
    return { definitionKind: 'scoped' };
  }

  /** The class/namespace name argument — a symbol `#Foo` or a bare identifier `Foo`. */
  private symbolName(args: Node[]): string | undefined {
    for (const a of args) {
      if (a.kind === NodeKind.Literal && a.literalKind === 'symbol') {
        return a.value.replace(/^#/, '').replace(/^'(.*)'$/, '$1');
      }
      if (a.kind === NodeKind.Variable) {
        return a.name;
      }
    }
    return undefined;
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

  /** A zero-width implicit-receiver marker at the current position. */
  private implicitReceiver(): Node {
    const t = this.current();
    return {
      kind: NodeKind.ImplicitReceiver,
      start: t.start,
      end: t.start,
      startPos: t.startPos,
      endPos: t.startPos,
    };
  }

  private parseKeywordMessage(): Node {
    // A statement starting with a keyword has no receiver — a GST `definition: [ name: … ]`
    // block of messages to an implicit receiver. Use a zero-width marker rather than erroring.
    let receiver = this.at(TokenKind.Keyword) ? this.implicitReceiver() : this.parseBinaryMessage();
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
      case TokenKind.Identifier: {
        // Scoped name: `Namespace::Class` or `Namespace.Class` (a run of `<scope> Identifier`).
        this.advance();
        let name = t.text;
        while (this.at(TokenKind.Scope) && this.peek(1).kind === TokenKind.Identifier) {
          const separator = this.current().text; // '::' or '.'
          this.advance();
          name += `${separator}${this.current().text}`;
          this.advance(); // identifier
        }
        return {
          kind: NodeKind.Variable,
          name,
          start: t.start,
          end: this.prev.end,
          startPos: t.startPos,
          endPos: this.prev.endPos,
        };
      }
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
      case TokenKind.HashBrace:
        return this.parseBindingConstant();
      case TokenKind.Hash:
        if (this.peek(1).kind === TokenKind.HashParen) {
          return this.parseCompileTimeConstant();
        }
        this.advance();
        this.diag('Unexpected "#"', t);
        return { kind: NodeKind.Error, message: 'Unexpected "#"', ...this.range(t) };
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
      case TokenKind.HashBrace:
        return this.parseBindingConstant();
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
    const temporaries = this.parseTemporaries(); // `{ | t | … }` constructor locals
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
    return { kind: NodeKind.DynamicArray, temporaries, elements, ...this.span(startTok) };
  }

  /** `#{ Namespace::Class }` binding constant. */
  private parseBindingConstant(): Node {
    const startTok = this.current();
    this.advance(); // '#{'
    const parts: string[] = [];
    while (!this.atEnd() && !this.at(TokenKind.RBrace)) {
      const t = this.current();
      if (t.kind === TokenKind.Identifier || t.kind === TokenKind.Scope || t.kind === TokenKind.Period) {
        parts.push(t.text);
      }
      this.advance();
    }
    if (this.at(TokenKind.RBrace)) {
      this.advance();
    } else {
      this.diag('Expected "}" to close binding constant', this.current());
    }
    return { kind: NodeKind.BindingConstant, path: parts.join(''), ...this.span(startTok) };
  }

  /** `##( … )` compile-time constant — a temporaries + statement sequence. */
  private parseCompileTimeConstant(): Node {
    const startTok = this.current();
    this.advance(); // '#'
    this.advance(); // '#('
    const { temporaries, statements } = this.parseSequenceBody(TokenKind.RParen);
    if (this.at(TokenKind.RParen)) {
      this.advance();
    } else {
      this.diag('Expected ")" to close compile-time constant', this.current());
    }
    return { kind: NodeKind.CompileTimeConstant, temporaries, statements, ...this.span(startTok) };
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
