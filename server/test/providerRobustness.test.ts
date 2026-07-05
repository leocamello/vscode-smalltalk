// Provider robustness matrix (US-901, AC4/AC6). tsx.
//
// The bug-bash guard: every pure provider entry point is driven over a matrix of
// adversarial documents × boundary offsets (0, 1, mid, end, PAST end, negative),
// asserting the "front end never throws" invariant (CLAUDE.md) holds end-to-end,
// not just in the parser. A throw here is a defect to fix (providers must return
// empty/null, never raise). Complements the parser fuzz in robustness.test.ts.
import assert from 'node:assert/strict';
import { parse } from '../src/parser/parser.ts';
import { tokenize } from '../src/parser/lexer.ts';
import { buildSymbolTable } from '../src/parser/symbols.ts';
import { completionsAt } from '../src/providers/completion.ts';
import { hoverAt } from '../src/providers/hover.ts';
import { signatureHelpAt } from '../src/providers/signatureHelp.ts';
import { semanticTokensFull, semanticTokensRange } from '../src/providers/semanticTokens.ts';
import { documentHighlightsAt } from '../src/providers/documentHighlight.ts';
import { toFoldingRanges } from '../src/providers/foldingRange.ts';
import { toDocumentSymbols } from '../src/providers/documentSymbol.ts';
import { prepareRenameAt, renameAt } from '../src/providers/rename.ts';
import { formatSource } from '../src/format/formatter.ts';
import { toDiagnostics } from '../src/providers/diagnostics.ts';
import { toCodeActions } from '../src/providers/codeAction.ts';

let passed = 0;
function test(name: string, fn: () => void): void {
  fn();
  passed += 1;
  console.log(`  ok - ${name}`);
}

const DOCS: Array<[string, string]> = [
  ['empty', ''],
  ['whitespace', '   \n\t  \n'],
  ['comment only', '"just a comment"'],
  ['unterminated comment', '"never closed'],
  ['unterminated string', "x := 'oops"],
  ['unbalanced brackets', 'Object subclass: Foo [ bar [ ^['],
  ['bare bang soup', '! ! !'],
  ['unicode', "Object subclass: Café [ naïve [ ^'héllo' ] ]"],
  ['normal class', 'Object subclass: Foo [ | x | bar: y [ ^x foo: y bar: 3 ] ]'],
  ['keyword cascade', 'x foo: 1; bar: 2; baz'],
  ['just an identifier', 'Foo'],
  ['dots', '.....'],
  ['deep nest (bounded)', '['.repeat(1000) + '1' + ']'.repeat(1000)],
  ['truncated mid-keyword', 'x at:'],
  ['crlf', 'a := 1\r\nb := 2\r\n'],
];

const hoverCtx = {
  isClass: () => false,
  superclassOf: () => undefined,
  implementorsOf: () => [] as never[],
  classComment: () => undefined,
};
const semCtx = { hasCartridge: false, classOrigin: () => undefined } as const;

test('every provider survives the adversarial doc × offset matrix (never throws)', () => {
  let checks = 0;
  for (const [name, text] of DOCS) {
    const { ast, diagnostics } = parse(text);
    const { tokens } = tokenize(text);
    const symbols = buildSymbolTable(ast);
    const len = text.length;
    const offsets = [0, 1, Math.floor(len / 2), Math.max(0, len - 1), len, len + 5, -1];
    const uri = 'file:///probe.st';

    const run = (label: string, fn: () => void): void => {
      checks += 1;
      try {
        fn();
      } catch (e) {
        assert.fail(`${label}: threw ${String(e).split('\n')[0]}`);
      }
    };

    run(`${name} :: semanticTokensFull`, () => semanticTokensFull(ast, symbols, tokens, semCtx));
    run(`${name} :: semanticTokensRange`, () =>
      semanticTokensRange(ast, symbols, tokens, semCtx, {
        start: { line: 0, character: 0 },
        end: { line: 5, character: 0 },
      }),
    );
    run(`${name} :: toFoldingRanges`, () => toFoldingRanges(ast, tokens));
    run(`${name} :: toDocumentSymbols`, () => toDocumentSymbols(symbols));
    run(`${name} :: formatSource`, () => formatSource(text));
    run(`${name} :: toDiagnostics`, () => toDiagnostics(diagnostics));
    run(`${name} :: toCodeActions`, () => toCodeActions(uri, toDiagnostics(diagnostics), text));

    for (const offset of offsets) {
      run(`${name} @${offset} :: completionsAt`, () => completionsAt(offset, text, tokens, ast, symbols, [], []));
      run(`${name} @${offset} :: hoverAt`, () => hoverAt(offset, text, tokens, ast, symbols, hoverCtx));
      run(`${name} @${offset} :: signatureHelpAt`, () => signatureHelpAt(offset, text, tokens, []));
      run(`${name} @${offset} :: documentHighlightsAt`, () => documentHighlightsAt(ast, tokens, offset));
      run(`${name} @${offset} :: prepareRenameAt`, () => prepareRenameAt(ast, tokens, symbols, offset));
      run(`${name} @${offset} :: renameAt`, () => renameAt(uri, offset, 'Renamed', [{ uri, text }]));
    }
  }
  assert.ok(checks > 700, `ran the full matrix (${checks} checks)`);
});

console.log(`providerRobustness.test: ${passed} passed.`);
