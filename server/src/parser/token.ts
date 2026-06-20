// Token model for the GNU Smalltalk lexer (US-411, slice 1).
//
// Pure data types only — no `vscode`/`vscode-languageserver` imports — so the
// lexer and its tests run in plain Node via `tsx`. The LSP-shaped `Position`
// and `LexDiagnostic` are mapped to real `vscode-languageserver` types at the
// provider boundary in later stories (US-412+).

/** The kinds a {@link Token} can take. String-valued for readable snapshots. */
export enum TokenKind {
  /** A general identifier: `foo`, `Object`, `with_underscore`. Pseudo-variables
   *  (`self`, `super`, `nil`, `true`, `false`, `thisContext`) are also `Identifier`;
   *  the parser gives them meaning. */
  Identifier = 'Identifier',
  /** An identifier immediately followed by a colon: `ifTrue:`, `at:`. */
  Keyword = 'Keyword',
  /** A binary selector: a run of `% & * + , - / < = > ? @ \ ~` (e.g. `+`, `<=`, `>>`, `->`). */
  BinarySelector = 'BinarySelector',
  /** Assignment: `:=` or the legacy `_`. */
  Assign = 'Assign',
  /** Namespace scope separator: `::`. */
  Scope = 'Scope',

  Integer = 'Integer',
  Float = 'Float',
  ScaledDecimal = 'ScaledDecimal',

  String = 'String',
  /** A symbol literal: `#foo`, `#at:put:`, `#'a b'`, `#+`. */
  Symbol = 'Symbol',
  /** A character literal: `$a`, `$$`, `$<65>`. */
  Char = 'Char',

  /** A bare `#` (e.g. before a binding `#{...}` is otherwise handled, or stray). */
  Hash = 'Hash',
  /** Literal-array start `#(`. */
  HashParen = 'HashParen',
  /** Byte-array start `#[`. */
  HashBracket = 'HashBracket',
  /** Binding / compile-time-constant start `#{`. */
  HashBrace = 'HashBrace',

  LParen = 'LParen',
  RParen = 'RParen',
  LBracket = 'LBracket',
  RBracket = 'RBracket',
  /** Array-constructor / binding `{`. */
  LBrace = 'LBrace',
  RBrace = 'RBrace',

  Period = 'Period',
  Caret = 'Caret',
  Semicolon = 'Semicolon',
  /** A lone `|` — temporaries delimiter / block-arg separator / binary selector (parser decides). */
  Pipe = 'Pipe',
  /** A lone `:` not forming `:=`, `::`, or a keyword. */
  Colon = 'Colon',
  /** Chunk / file-out separator `!`. */
  Bang = 'Bang',
  /** A `#!` shebang line, only at the very start of a file. */
  Shebang = 'Shebang',

  /** A `"..."` comment, kept as trivia for later hover support; the parser ignores it. */
  Comment = 'Comment',
  /** A recovery token for an unexpected character; always paired with a diagnostic. */
  Error = 'Error',
  /** End-of-input sentinel; always the last token. */
  EOF = 'EOF',
}

/** A 0-based, UTF-16 source position, matching the LSP `Position` shape. */
export interface Position {
  readonly line: number;
  readonly character: number;
}

/** A lexed token with byte offsets and LSP positions. */
export interface Token {
  readonly kind: TokenKind;
  /** The exact source text of the token. */
  readonly text: string;
  /** Start byte offset (inclusive). */
  readonly start: number;
  /** End byte offset (exclusive). */
  readonly end: number;
  readonly startPos: Position;
  readonly endPos: Position;
}

export enum DiagnosticSeverity {
  Error = 'Error',
  Warning = 'Warning',
}

/** A lexer diagnostic. LSP-free; mapped to a `vscode-languageserver` `Diagnostic` later. */
export interface LexDiagnostic {
  readonly message: string;
  readonly severity: DiagnosticSeverity;
  readonly start: number;
  readonly end: number;
  readonly startPos: Position;
  readonly endPos: Position;
}
