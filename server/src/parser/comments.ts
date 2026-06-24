// Comment extraction (US-415 slice B).
//
// Best-effort recovery of the *prose* the symbol table deliberately drops:
//   - a class comment from a `<comment: '…'>` pragma in the class body, and
//   - a method comment from the leading `"…"` comment token inside the method.
// Keyed by class name + side + selector so it can decorate a DialectCartridge
// (installed adapter) or a workspace index entry. Pure (front-end types only);
// never throws. Prose is licence-gated by the CALLER (provenance), not here.

import { NodeKind, type DefinitionNode, type Node, type ProgramNode } from './ast';
import { TokenKind, type Token } from './token';

export interface FileComments {
  /** Class comment for `name`, or undefined. */
  classComment(name: string): string | undefined;
  /** Method comment for `class`/`side`/`selector`, or undefined. */
  methodComment(className: string, classSide: boolean, selector: string): string | undefined;
}

/** A class name from a `Foo` reference or a `Foo class` message; else undefined. */
function classNameOf(node: Node): string | undefined {
  if (node.kind === NodeKind.Variable) {
    return node.name;
  }
  if (node.kind === NodeKind.Message && node.messageType === 'unary' && node.selector === 'class') {
    return classNameOf(node.receiver);
  }
  return undefined;
}

/** The class a definition targets (`Object subclass: Foo` → Foo; `Foo class […]` → Foo). */
function classNameForDefinition(node: DefinitionNode): string | undefined {
  if (node.name) {
    return node.name;
  }
  const def = node.definer;
  if (def.kind === NodeKind.Message) {
    const byReceiver = classNameOf(def.receiver);
    if (byReceiver) {
      return byReceiver;
    }
  }
  return classNameOf(def);
}

/** Strip a GST string literal `'…'` and unescape doubled quotes. */
function unquoteString(raw: string): string {
  const inner = raw.length >= 2 && raw.startsWith("'") && raw.endsWith("'") ? raw.slice(1, -1) : raw;
  return inner.replace(/''/g, "'").trim();
}

/** Strip a GST comment `"…"` and unescape doubled quotes. */
function unquoteComment(raw: string): string {
  const inner = raw.length >= 2 && raw.startsWith('"') && raw.endsWith('"') ? raw.slice(1, -1) : raw;
  return inner.replace(/""/g, '"').trim();
}

const methodKey = (className: string, classSide: boolean, selector: string): string =>
  `${className}\t${classSide ? 'c' : 'i'}\t${selector}`;

/** Extract class + method comments from a parsed file. */
export function extractComments(ast: ProgramNode, tokens: readonly Token[]): FileComments {
  const classComments = new Map<string, string>();
  const methodComments = new Map<string, string>();
  const commentTokens = tokens.filter((t) => t.kind === TokenKind.Comment);

  /** The first comment token inside `[start, end)` (the method's leading doc). */
  const leadingComment = (start: number, end: number): string | undefined => {
    const tok = commentTokens.find((t) => t.start >= start && t.start < end);
    return tok ? unquoteComment(tok.text) : undefined;
  };

  const walk = (node: Node, currentClass: string | undefined, currentSide: boolean): void => {
    if (node.kind === NodeKind.MethodDefinition) {
      const className = (node.target ? classNameOf(node.target) : undefined) ?? currentClass;
      if (className) {
        const side = node.classSide || currentSide;
        const text = leadingComment(node.start, node.end);
        if (text) {
          methodComments.set(methodKey(className, side, node.selector), text);
        }
      }
      return; // method bodies hold no nested classes
    }

    if (node.kind === NodeKind.Definition) {
      const classSide = node.definitionKind === 'classScope';
      const className =
        node.definitionKind === 'namespace' ? currentClass : classNameForDefinition(node) ?? currentClass;

      if (className && !classComments.has(className)) {
        for (const item of node.body) {
          if (item.kind === NodeKind.Pragma && item.selector === 'comment:') {
            const arg = item.arguments[0];
            if (arg && arg.kind === NodeKind.Literal && arg.literalKind === 'string') {
              classComments.set(className, unquoteString(arg.value));
              break;
            }
          }
        }
      }
      for (const item of node.body) {
        walk(item, className, classSide || currentSide);
      }
    }
    // Any other node is a statement that cannot enclose a class/method — stop here.
  };

  for (const stmt of ast.statements) {
    walk(stmt, undefined, false);
  }

  return {
    classComment: (name) => classComments.get(name),
    methodComment: (className, classSide, selector) => methodComments.get(methodKey(className, classSide, selector)),
  };
}
