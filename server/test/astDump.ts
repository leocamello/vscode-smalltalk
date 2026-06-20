// Shared AST serializer for parser/container snapshot tests (US-411).
// Produces a compact, indented tree with `@start..end` ranges, plus a
// diagnostics section — deterministic and stable across runs.
import { parse } from '../src/parser/parser.ts';
import { NodeKind, type Node } from '../src/parser/ast.ts';

function names(list: ReadonlyArray<{ name: string }>): string {
  return list.map((n) => n.name).join(', ');
}

export function dump(node: Node, indent: string, out: string[]): void {
  const at = `@${node.start}..${node.end}`;
  const child = indent + '  ';
  switch (node.kind) {
    case NodeKind.Program:
      out.push(`${indent}Program temps=[${names(node.temporaries)}] ${at}`);
      node.statements.forEach((s) => dump(s, child, out));
      break;
    case NodeKind.Block:
      out.push(`${indent}Block params=[${names(node.parameters)}] temps=[${names(node.temporaries)}] ${at}`);
      node.statements.forEach((s) => dump(s, child, out));
      break;
    case NodeKind.Return:
      out.push(`${indent}Return ${at}`);
      dump(node.value, child, out);
      break;
    case NodeKind.Assignment:
      out.push(`${indent}Assignment target='${node.target.name}' ${at}`);
      dump(node.value, child, out);
      break;
    case NodeKind.Message:
      out.push(`${indent}Message ${node.messageType} '${node.selector}' ${at}`);
      dump(node.receiver, child, out);
      node.arguments.forEach((a) => dump(a, child, out));
      break;
    case NodeKind.Cascade:
      out.push(`${indent}Cascade ${at}`);
      dump(node.receiver, child, out);
      node.messages.forEach((m) => {
        out.push(`${child};`);
        dump(m, child + '  ', out);
      });
      break;
    case NodeKind.CascadeReceiver:
      out.push(`${indent}CascadeReceiver ${at}`);
      break;
    case NodeKind.ImplicitReceiver:
      out.push(`${indent}ImplicitReceiver ${at}`);
      break;
    case NodeKind.Definition:
      out.push(`${indent}Definition ${node.definitionKind}${node.name ? ` name='${node.name}'` : ''} ${at}`);
      dump(node.definer, child, out);
      node.body.forEach((b) => dump(b, child, out));
      break;
    case NodeKind.MethodDefinition:
      out.push(
        `${indent}MethodDefinition ${node.classSide ? 'class ' : ''}${node.messageType} '${node.selector}'` +
          ` params=[${names(node.parameters)}] temps=[${names(node.temporaries)}] ${at}`,
      );
      if (node.target) {
        dump(node.target, child, out);
      }
      node.pragmas.forEach((p) => dump(p, child, out));
      node.statements.forEach((s) => dump(s, child, out));
      break;
    case NodeKind.Pragma:
      out.push(`${indent}Pragma '${node.selector}' ${at}`);
      node.arguments.forEach((a) => dump(a, child, out));
      break;
    case NodeKind.InstanceVariables:
      out.push(`${indent}InstanceVariables [${names(node.names)}] ${at}`);
      break;
    case NodeKind.Variable:
      out.push(`${indent}Variable '${node.name}' ${at}`);
      break;
    case NodeKind.BindingConstant:
      out.push(`${indent}BindingConstant '${node.path}' ${at}`);
      break;
    case NodeKind.CompileTimeConstant:
      out.push(`${indent}CompileTimeConstant temps=[${names(node.temporaries)}] ${at}`);
      node.statements.forEach((s) => dump(s, child, out));
      break;
    case NodeKind.Literal:
      out.push(`${indent}Literal ${node.literalKind} ${JSON.stringify(node.value)} ${at}`);
      break;
    case NodeKind.DynamicArray:
      out.push(`${indent}DynamicArray${node.temporaries.length ? ` temps=[${names(node.temporaries)}]` : ''} ${at}`);
      node.elements.forEach((e) => dump(e, child, out));
      break;
    case NodeKind.LiteralArray:
    case NodeKind.ByteArray:
      out.push(`${indent}${node.kind} ${at}`);
      node.elements.forEach((e) => dump(e, child, out));
      break;
    case NodeKind.Error:
      out.push(`${indent}Error ${JSON.stringify(node.message)} ${at}`);
      break;
  }
}

/** Serialize a source string's AST + diagnostics to a stable snapshot string. */
export function serializeAst(src: string): string {
  const { ast, diagnostics } = parse(src);
  const out: string[] = [];
  dump(ast, '', out);
  const diag = diagnostics.map((d) => `${d.severity} ${JSON.stringify(d.message)} @${d.start}..${d.end}`);
  return [...out, '', '--- diagnostics ---', ...diag, ''].join('\n');
}
