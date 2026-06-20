// AST for the GNU Smalltalk expression parser (US-411, slice 2).
//
// Pure data types — no `vscode` imports — so the parser and its tests run in
// plain Node via `tsx`. Every node carries byte offsets and LSP-shaped
// positions (reused from token.ts), so downstream LSP features (US-412+) get
// ranges directly.

import type { Position } from './token';

export enum NodeKind {
  /** Top-level: optional temporaries + a statement sequence. */
  Program = 'Program',
  /** `[ :params | | temps | statements ]`. */
  Block = 'Block',
  /** `^ expression`. */
  Return = 'Return',
  /** `target := value` (also legacy `target _ value`). */
  Assignment = 'Assignment',
  /** A unary, binary, or keyword message send. */
  Message = 'Message',
  /** `receiver msg ; msg ; …`. */
  Cascade = 'Cascade',
  /** A zero-width marker standing in for the cascade receiver inside each cascade segment. */
  CascadeReceiver = 'CascadeReceiver',
  /** A variable / identifier reference. */
  Variable = 'Variable',
  /** A scalar literal: integer, float, scaled decimal, string, symbol, character. */
  Literal = 'Literal',
  /** `#( … )`. */
  LiteralArray = 'LiteralArray',
  /** `#[ … ]`. */
  ByteArray = 'ByteArray',
  /** `{ expr. expr. … }`. */
  DynamicArray = 'DynamicArray',
  /** A recovery node standing in for an unparseable fragment; paired with a diagnostic. */
  Error = 'Error',
}

export type MessageType = 'unary' | 'binary' | 'keyword';

export type LiteralKind =
  | 'integer'
  | 'float'
  | 'scaledDecimal'
  | 'string'
  | 'symbol'
  | 'character';

/** Fields every node carries: kind + source range. */
export interface NodeBase {
  readonly kind: NodeKind;
  readonly start: number;
  readonly end: number;
  readonly startPos: Position;
  readonly endPos: Position;
}

/** A named declaration (a temporary or a block parameter) with its own range. */
export interface NameRef {
  readonly name: string;
  readonly start: number;
  readonly end: number;
  readonly startPos: Position;
  readonly endPos: Position;
}

export interface ProgramNode extends NodeBase {
  readonly kind: NodeKind.Program;
  readonly temporaries: NameRef[];
  readonly statements: Node[];
}

export interface BlockNode extends NodeBase {
  readonly kind: NodeKind.Block;
  readonly parameters: NameRef[];
  readonly temporaries: NameRef[];
  readonly statements: Node[];
}

export interface ReturnNode extends NodeBase {
  readonly kind: NodeKind.Return;
  readonly value: Node;
}

export interface AssignmentNode extends NodeBase {
  readonly kind: NodeKind.Assignment;
  readonly target: VariableNode;
  readonly value: Node;
}

export interface MessageNode extends NodeBase {
  readonly kind: NodeKind.Message;
  readonly receiver: Node;
  readonly selector: string;
  readonly messageType: MessageType;
  readonly arguments: Node[];
}

export interface CascadeNode extends NodeBase {
  readonly kind: NodeKind.Cascade;
  /** The shared receiver, parsed once; each segment is a chain rooted at a {@link CascadeReceiverNode}. */
  readonly receiver: Node;
  /** One message chain per `;` segment (incl. the first), each rooted at a `CascadeReceiver` marker. */
  readonly messages: Node[];
}

/** Zero-width placeholder for the cascade receiver inside a segment's message chain. */
export interface CascadeReceiverNode extends NodeBase {
  readonly kind: NodeKind.CascadeReceiver;
}

export interface VariableNode extends NodeBase {
  readonly kind: NodeKind.Variable;
  readonly name: string;
}

export interface LiteralNode extends NodeBase {
  readonly kind: NodeKind.Literal;
  readonly literalKind: LiteralKind;
  /** Raw source text of the literal (no value evaluation in the front end). */
  readonly value: string;
}

export interface LiteralArrayNode extends NodeBase {
  readonly kind: NodeKind.LiteralArray;
  readonly elements: Node[];
}

export interface ByteArrayNode extends NodeBase {
  readonly kind: NodeKind.ByteArray;
  readonly elements: Node[];
}

export interface DynamicArrayNode extends NodeBase {
  readonly kind: NodeKind.DynamicArray;
  readonly elements: Node[];
}

export interface ErrorNode extends NodeBase {
  readonly kind: NodeKind.Error;
  readonly message: string;
}

export type Node =
  | ProgramNode
  | BlockNode
  | ReturnNode
  | AssignmentNode
  | MessageNode
  | CascadeNode
  | CascadeReceiverNode
  | VariableNode
  | LiteralNode
  | LiteralArrayNode
  | ByteArrayNode
  | DynamicArrayNode
  | ErrorNode;
