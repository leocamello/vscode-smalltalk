// Workspace-wide instance-variable reference resolver (US-426 rename, AC3).
//
// Given a class + ivar name and a set of (uri, text), find every occurrence
// (declaration + scope-resolved references) across all files that define or
// `extend` the class — skipping any method where a local temp/arg shadows the
// name. Pure: parses each file; reuses the shadow-aware scope walk.

import { NodeKind, type DefinitionNode, type MethodDefinitionNode, type Node, type ProgramNode } from '../parser/ast';
import { parse } from '../parser/parser';
import { bindsName, variableOccurrences, type Ranged } from '../parser/scope';

export interface FileText {
  readonly uri: string;
  readonly text: string;
}

/** A class name from a `Foo` reference or a `Foo class` message; else undefined. */
export function classNameOf(node: Node): string | undefined {
  if (node.kind === NodeKind.Variable) {
    return node.name;
  }
  if (node.kind === NodeKind.Message && node.messageType === 'unary' && node.selector === 'class') {
    return classNameOf(node.receiver);
  }
  return undefined;
}

/** The class a definition targets (`Object subclass: Foo` → Foo; `Foo extend […]` → Foo). */
export function classNameForDefinition(node: DefinitionNode): string | undefined {
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

/** Declaration NameRefs of `ivarName` among a class body's instance-variable decls. */
function ivarDeclsIn(body: readonly Node[], ivarName: string): Ranged[] {
  const out: Ranged[] = [];
  for (const item of body) {
    if (item.kind === NodeKind.InstanceVariables) {
      for (const nm of item.names) {
        if (nm.name === ivarName) {
          out.push(nm);
        }
      }
    }
  }
  return out;
}

/** References to `ivarName` inside a method — unless the method shadows the name with a local. */
function methodRefs(method: MethodDefinitionNode, ivarName: string): Ranged[] {
  if (bindsName(method, ivarName)) {
    return []; // shadowed by a local temp/arg — a different variable.
  }
  return variableOccurrences(method, ivarName).map((o) => ({
    start: o.start,
    end: o.end,
    startPos: o.startPos,
    endPos: o.endPos,
  }));
}

/** All occurrence ranges (decls + refs) of `className`'s `ivarName` within one parsed file. */
export function ivarOccurrencesInAst(ast: ProgramNode, className: string, ivarName: string): Ranged[] {
  const ranges: Ranged[] = [];
  const walk = (node: Node, currentClass: string | undefined): void => {
    if (node.kind === NodeKind.Definition) {
      const cls = classNameForDefinition(node) ?? currentClass;
      if (cls === className) {
        ranges.push(...ivarDeclsIn(node.body, ivarName));
      }
      for (const item of node.body) {
        walk(item, cls);
      }
    } else if (node.kind === NodeKind.MethodDefinition) {
      const cls = (node.target ? classNameOf(node.target) : undefined) ?? currentClass;
      if (cls === className) {
        ranges.push(...methodRefs(node, ivarName));
      }
    }
  };
  for (const stmt of ast.statements) {
    walk(stmt, undefined);
  }
  return ranges;
}

/** Whether `name` is declared as an instance/class variable of `className` in this file. */
export function isDeclaredIvarInAst(ast: ProgramNode, className: string, name: string): boolean {
  let found = false;
  const walk = (node: Node, currentClass: string | undefined): void => {
    if (found || node.kind !== NodeKind.Definition) {
      return;
    }
    const cls = classNameForDefinition(node) ?? currentClass;
    if (cls === className && ivarDeclsIn(node.body, name).length > 0) {
      found = true;
      return;
    }
    for (const item of node.body) {
      walk(item, cls);
    }
  };
  for (const stmt of ast.statements) {
    walk(stmt, undefined);
  }
  return found;
}

/** Workspace-wide: occurrence ranges of `className`'s `ivarName`, keyed by file uri. */
export function ivarOccurrences(
  className: string,
  ivarName: string,
  files: ReadonlyArray<FileText>,
): Map<string, Ranged[]> {
  const result = new Map<string, Ranged[]>();
  for (const { uri, text } of files) {
    const ranges = ivarOccurrencesInAst(parse(text).ast, className, ivarName);
    if (ranges.length > 0) {
      result.set(uri, ranges);
    }
  }
  return result;
}

/** Whether `name` is a declared instance/class variable of `className` in any file. */
export function isDeclaredIvar(className: string, name: string, files: ReadonlyArray<FileText>): boolean {
  return files.some(({ text }) => isDeclaredIvarInAst(parse(text).ast, className, name));
}
