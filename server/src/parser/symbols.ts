// Per-document symbol table for GNU Smalltalk (US-411, slice 4 / AC5).
//
// Walks the AST (slice 2/3) and records classes, methods (selector + arity +
// side), instance/class/temporary variables, and namespaces — as a
// DocumentSymbol-like tree with positions, ready for the LSP providers in
// US-412+. Pure: no `vscode` imports. Definitions of the same class (a brace
// body, `Class >> sel` methods, `extend`, and `methodsFor:` chunks) merge into
// one class symbol.

import {
  NodeKind,
  type DefinitionNode,
  type MethodDefinitionNode,
  type NameRef,
  type Node,
  type ProgramNode,
} from './ast';
import type { Position } from './token';

export enum SymbolKind {
  Namespace = 'namespace',
  Class = 'class',
  Method = 'method',
  InstanceVariable = 'instanceVariable',
  ClassVariable = 'classVariable',
  Temporary = 'temporary',
}

export interface SymbolRange {
  readonly start: number;
  readonly end: number;
  readonly startPos: Position;
  readonly endPos: Position;
}

export interface SymbolNode {
  readonly name: string;
  readonly kind: SymbolKind;
  /** Human-readable extra info (e.g. superclass, `class binary/1`). */
  readonly detail?: string;
  readonly classSide?: boolean;
  readonly selector?: string;
  readonly arity?: number;
  /** Full source range of the declaration. */
  readonly range: SymbolRange;
  /** Range of the name itself (for go-to-definition selection). */
  readonly selectionRange: SymbolRange;
  readonly children: SymbolNode[];
}

export function buildSymbolTable(program: ProgramNode): SymbolNode[] {
  return new SymbolBuilder().build(program);
}

function rangeOf(n: { start: number; end: number; startPos: Position; endPos: Position }): SymbolRange {
  return { start: n.start, end: n.end, startPos: n.startPos, endPos: n.endPos };
}

/** A class name from a `Foo` reference or a `Foo class` message; `undefined` otherwise. */
function classNameOf(node: Node): string | undefined {
  if (node.kind === NodeKind.Variable) {
    return node.name;
  }
  if (node.kind === NodeKind.Message && node.messageType === 'unary' && node.selector === 'class') {
    return classNameOf(node.receiver);
  }
  return undefined;
}

class SymbolBuilder {
  private readonly top: SymbolNode[] = [];
  private readonly classes = new Map<string, SymbolNode & { children: SymbolNode[] }>();

  build(program: ProgramNode): SymbolNode[] {
    for (const t of program.temporaries) {
      this.top.push(this.variable(t, SymbolKind.Temporary));
    }
    for (const stmt of program.statements) {
      this.visitTopLevel(stmt);
    }
    return this.top;
  }

  private visitTopLevel(node: Node): void {
    if (node.kind === NodeKind.MethodDefinition) {
      this.addMethod(node);
      return;
    }
    if (node.kind !== NodeKind.Definition) {
      return;
    }
    switch (node.definitionKind) {
      case 'subclass': {
        const cls = this.classFor(node.name ?? '<anonymous>', node, this.superclassName(node));
        this.collectClassBody(cls, node.body, false);
        break;
      }
      case 'namespace': {
        const ns = this.namespace(node);
        this.top.push(ns);
        break;
      }
      case 'extend': {
        const name = classNameOf((node.definer as { receiver?: Node }).receiver ?? node.definer);
        if (name) {
          this.collectClassBody(this.classFor(name, node), node.body, false);
        }
        break;
      }
      case 'classScope': {
        const name = classNameOf((node.definer as { receiver?: Node }).receiver ?? node.definer);
        if (name) {
          this.collectClassBody(this.classFor(name, node), node.body, true);
        }
        break;
      }
      case 'methodsFor': {
        for (const m of node.body) {
          if (m.kind === NodeKind.MethodDefinition) {
            this.addMethod(m);
          }
        }
        break;
      }
      default:
        break;
    }
  }

  private namespace(node: DefinitionNode): SymbolNode {
    const children: SymbolNode[] = [];
    for (const item of node.body) {
      if (item.kind === NodeKind.Definition && item.definitionKind === 'subclass') {
        const cls: SymbolNode & { children: SymbolNode[] } = {
          name: item.name ?? '<anonymous>',
          kind: SymbolKind.Class,
          detail: this.superclassName(item),
          range: rangeOf(item),
          selectionRange: rangeOf(item.definer),
          children: [],
        };
        this.collectClassBody(cls, item.body, false);
        children.push(cls);
      }
    }
    return {
      name: node.name ?? '<namespace>',
      kind: SymbolKind.Namespace,
      range: rangeOf(node),
      selectionRange: rangeOf(node.definer),
      children,
    };
  }

  /** Add the members of a class body to `cls`. `classSide` marks class-side scopes. */
  private collectClassBody(cls: SymbolNode & { children: SymbolNode[] }, items: Node[], classSide: boolean): void {
    for (const item of items) {
      switch (item.kind) {
        case NodeKind.InstanceVariables:
          for (const name of item.names) {
            cls.children.push(this.variable(name, classSide ? SymbolKind.ClassVariable : SymbolKind.InstanceVariable));
          }
          break;
        case NodeKind.MethodDefinition:
          // A method is class-side from its own `Class class >> …` form or its enclosing scope.
          cls.children.push(this.method(item, item.classSide || classSide));
          break;
        case NodeKind.Definition:
          if (item.definitionKind === 'classScope') {
            this.collectClassBody(cls, item.body, true);
          } else if (item.definitionKind === 'subclass') {
            const nested: SymbolNode & { children: SymbolNode[] } = {
              name: item.name ?? '<anonymous>',
              kind: SymbolKind.Class,
              detail: this.superclassName(item),
              range: rangeOf(item),
              selectionRange: rangeOf(item.definer),
              children: [],
            };
            this.collectClassBody(nested, item.body, false);
            cls.children.push(nested);
          }
          break;
        case NodeKind.Assignment:
          // `ClassVar := …` inside a class-side scope declares a class variable.
          if (classSide) {
            cls.children.push(this.variable(item.target, SymbolKind.ClassVariable));
          }
          break;
        default:
          break;
      }
    }
  }

  private addMethod(m: MethodDefinitionNode): void {
    const name = m.target ? classNameOf(m.target) : undefined;
    const cls = this.classFor(name ?? '<methods>', m);
    cls.children.push(this.method(m, m.classSide));
  }

  private method(m: MethodDefinitionNode, classSide: boolean): SymbolNode {
    const arity = m.parameters.length;
    return {
      name: m.selector,
      kind: SymbolKind.Method,
      detail: `${classSide ? 'class ' : ''}${m.messageType}/${arity}`,
      classSide,
      selector: m.selector,
      arity,
      range: rangeOf(m),
      selectionRange: rangeOf(m),
      children: [
        ...m.parameters.map((p) => this.variable(p, SymbolKind.Temporary)),
        ...m.temporaries.map((t) => this.variable(t, SymbolKind.Temporary)),
      ],
    };
  }

  private variable(ref: NameRef, kind: SymbolKind): SymbolNode {
    return { name: ref.name, kind, range: rangeOf(ref), selectionRange: rangeOf(ref), children: [] };
  }

  /** Get or create the merged class symbol for `name`. */
  private classFor(
    name: string,
    node: { start: number; end: number; startPos: Position; endPos: Position },
    detail?: string,
  ): SymbolNode & { children: SymbolNode[] } {
    const existing = this.classes.get(name);
    if (existing) {
      return existing;
    }
    const created: SymbolNode & { children: SymbolNode[] } = {
      name,
      kind: SymbolKind.Class,
      ...(detail !== undefined ? { detail } : {}),
      range: rangeOf(node),
      selectionRange: rangeOf(node),
      children: [],
    };
    this.classes.set(name, created);
    this.top.push(created);
    return created;
  }

  private superclassName(def: DefinitionNode): string | undefined {
    if (def.definer.kind === NodeKind.Message) {
      const recv = def.definer.receiver;
      if (recv.kind === NodeKind.Variable) {
        return recv.name;
      }
    }
    return undefined;
  }
}
