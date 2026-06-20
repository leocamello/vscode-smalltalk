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
  /** A GST binding constant `#{Namespace::Class}`. */
  BindingConstant = 'BindingConstant',
  /** A GST compile-time constant `##( … )`. */
  CompileTimeConstant = 'CompileTimeConstant',
  /** A GST scoped definition: `<expr> [ … ]` (subclass / namespace / extend / class-side scope). */
  Definition = 'Definition',
  /** A GST scoped method: `Class [class] >> pattern [ … ]`. */
  MethodDefinition = 'MethodDefinition',
  /** A `<…>` attribute / pragma (e.g. `<primitive: 80>`, `<gst.methodCategory: '…'>`). */
  Pragma = 'Pragma',
  /** A `| a b |` instance-variable declaration inside a class body. */
  InstanceVariables = 'InstanceVariables',
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

/** How a definition reads: a `<expr> [ … ]` scoped form, or a `methodsFor:` chunk section. */
export type DefinitionKind = 'subclass' | 'namespace' | 'extend' | 'classScope' | 'scoped' | 'methodsFor';

export interface DefinitionNode extends NodeBase {
  readonly kind: NodeKind.Definition;
  readonly definitionKind: DefinitionKind;
  /** The expression preceding the `[` (e.g. `Object subclass: #Foo`). */
  readonly definer: Node;
  /** The class/namespace name, when derivable from the definer's symbol argument. */
  readonly name?: string;
  /** Body items: instance-var decls, pragmas, method/nested definitions, statements. */
  readonly body: Node[];
}

export interface MethodDefinitionNode extends NodeBase {
  readonly kind: NodeKind.MethodDefinition;
  /** The owning class reference (e.g. `Foo`); absent for the short form `sel [ … ]` inside a body. */
  readonly target?: Node;
  /** True for `Foo class >> …`. */
  readonly classSide: boolean;
  readonly selector: string;
  readonly messageType: MessageType;
  readonly parameters: NameRef[];
  readonly pragmas: PragmaNode[];
  readonly temporaries: NameRef[];
  readonly statements: Node[];
}

export interface PragmaNode extends NodeBase {
  readonly kind: NodeKind.Pragma;
  readonly selector: string;
  readonly arguments: Node[];
}

export interface InstanceVariablesNode extends NodeBase {
  readonly kind: NodeKind.InstanceVariables;
  readonly names: NameRef[];
}

export interface BindingConstantNode extends NodeBase {
  readonly kind: NodeKind.BindingConstant;
  /** The bound name, e.g. `Transcript` or `TestBindings::MyBoundClass`. */
  readonly path: string;
}

export interface CompileTimeConstantNode extends NodeBase {
  readonly kind: NodeKind.CompileTimeConstant;
  readonly temporaries: NameRef[];
  readonly statements: Node[];
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
  /** Constructor-local temporaries (`{ | t | … }`); usually empty. */
  readonly temporaries: NameRef[];
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
  | DefinitionNode
  | MethodDefinitionNode
  | PragmaNode
  | InstanceVariablesNode
  | BindingConstantNode
  | CompileTimeConstantNode
  | LiteralNode
  | LiteralArrayNode
  | ByteArrayNode
  | DynamicArrayNode
  | ErrorNode;
