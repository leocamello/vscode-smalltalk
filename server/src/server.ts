import { fileURLToPath } from 'node:url';
import fs from 'node:fs';
import path from 'node:path';
import {
  createConnection,
  ProposedFeatures,
  DidChangeConfigurationNotification,
  ResponseError,
  ErrorCodes,
  TextDocuments,
  TextDocumentSyncKind,
  CodeActionKind,
  type CallHierarchyIncomingCall,
  type CallHierarchyItem,
  type CallHierarchyIncomingCallsParams,
  type CallHierarchyOutgoingCall,
  type CallHierarchyOutgoingCallsParams,
  type CallHierarchyPrepareParams,
  type CodeAction,
  type CodeActionParams,
  type CompletionItem,
  type CompletionParams,
  type DefinitionParams,
  type DocumentHighlight,
  type DocumentHighlightParams,
  type DocumentSymbol,
  type DocumentSymbolParams,
  type DocumentFormattingParams,
  type DocumentRangeFormattingParams,
  type DocumentOnTypeFormattingParams,
  type PrepareRenameParams,
  type RenameParams,
  type Range,
  type WorkspaceEdit,
  type TextEdit,
  type FoldingRange,
  type FoldingRangeParams,
  type Hover,
  type HoverParams,
  type InitializeParams,
  type InitializeResult,
  type Location,
  type LocationLink,
  type ReferenceParams,
  type SemanticTokens,
  type SemanticTokensParams,
  type SemanticTokensRangeParams,
  type SignatureHelp,
  type SignatureHelpParams,
  type WorkspaceSymbol,
  type WorkspaceSymbolParams,
} from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { parse } from './parser/parser';
import { getAst, getDiagnostics, getSymbols, getTokens, invalidate } from './documents/parseCache';
import {
  formatDocument,
  formatRange,
  formatOnType,
  DEFAULT_FORMAT_SETTINGS,
  type FormatSettings,
} from './providers/formatting';
import {
  prepareRenameAt,
  renameAt,
  enclosingClassNameAt,
  renameKindAt,
  withMultiFileConfirmation,
  type FileText,
} from './providers/rename';
import { buildClassWorldFromFiles, type ClassWorld } from './xref/classRefs';
import { toDiagnostics } from './providers/diagnostics';
import { toCodeActions } from './providers/codeAction';
import { toDocumentSymbols } from './providers/documentSymbol';
import { toFoldingRanges } from './providers/foldingRange';
import { documentHighlightsAt } from './providers/documentHighlight';
import {
  WorkspaceIndex,
  defaultExclude,
  excludeFromConfig,
  walkStFiles,
  type ExcludePredicate,
  type IndexEntry,
} from './providers/workspaceIndex';
import { toWorkspaceSymbols } from './providers/workspaceSymbol';
import { findDefinitions, resolveDefinitionQuery } from './providers/definition';
import { WorkspaceXref } from './xref/workspaceXref';
import { resolveImplementors, resolveSenders, type WorkspaceMethodRef, type XrefSources } from './xref/resolve';
import { buildCrossReference, type CrossReferenceResult, type XrefDirection } from './providers/crossReference';
import { definitionLinks, definitionLocations, referenceLocations } from './providers/references';
import {
  incomingCalls,
  outgoingCalls,
  prepareCallHierarchy,
  resolveCallTarget,
  type CallItemData,
} from './providers/callHierarchy';
import { completionsAt, type ClassCandidate, type SelectorCandidate } from './providers/completion';
import { signatureHelpAt, type SignatureCandidate } from './providers/signatureHelp';
import { hoverAt, type HoverContext, type HoverImplementor } from './providers/hover';
import {
  semanticTokensFull,
  semanticTokensRange,
  SEMANTIC_TOKEN_TYPES,
  SEMANTIC_TOKEN_MODIFIERS,
  type SemanticTokenContext,
} from './providers/semanticTokens';
import { KernelIndexService, type KernelLibrarySetting } from './kernel/kernelIndexService';
import { Provenance } from './kernel/model';
import { SymbolKind } from './parser/symbols';

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);
const index = new WorkspaceIndex();
// Live workspace cross-reference index (US-423): selector → send sites, kept in
// lockstep with `index` so "Senders of" / references query both tiers offline.
const workspaceXref = new WorkspaceXref();
const kernelService = new KernelIndexService();

/** Custom notification: the resolved kernel-completion source (for the status bar). */
const KERNEL_STATUS_NOTIFICATION = 'smalltalk/kernelStatus';

const INDEX_DEBOUNCE_MS = 250;
const indexTimers = new Map<string, ReturnType<typeof setTimeout>>();
// Live parser diagnostics (US-414, AC1): debounced on its own timer so squiggles
// stay responsive and don't flicker mid-token. The parse is already cached per
// (uri, version), so this is cheap; the debounce only smooths rapid typing.
const DIAGNOSTIC_DEBOUNCE_MS = 250;
const diagnosticTimers = new Map<string, ReturnType<typeof setTimeout>>();
let workspaceFolders: string[] = [];
// Whether the client accepts `LocationLink[]` from go-to-definition (US-423 AC3).
// Real VS Code does; a minimal client may not, so we fall back to `Location[]`.
let definitionLinkSupport = false;
// Whether the client supports annotated workspace edits (US-426): lets a multi-file
// rename ask for confirmation via the Refactor Preview instead of applying on Enter.
let changeAnnotationSupport = false;
// The exclude predicate in force; refreshed from `files.exclude` on init and on
// configuration change so the index honors it consistently.
let currentExclude: ExcludePredicate = defaultExclude;

connection.onInitialize((params: InitializeParams): InitializeResult => {
  // Language-intelligence providers are built on the US-411 front end and run
  // with no `gst`. US-412: document symbols (outline), workspace symbol search,
  // and go-to-definition.
  workspaceFolders = (params.workspaceFolders ?? [])
    .map((folder) => uriToPath(folder.uri))
    .filter((p): p is string => p !== undefined);
  definitionLinkSupport = params.capabilities.textDocument?.definition?.linkSupport === true;
  changeAnnotationSupport =
    params.capabilities.workspace?.workspaceEdit?.documentChanges === true &&
    params.capabilities.workspace?.workspaceEdit?.changeAnnotationSupport !== undefined;
  // Have a usable kernel immediately (refined from config in onInitialized).
  kernelService.configure({ kernelLibrary: 'auto' });
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      documentSymbolProvider: true,
      workspaceSymbolProvider: true,
      definitionProvider: true,
      // Cross-reference union: senders/implementors of a selector across the
      // workspace + the bundled cartridge, offline (US-423).
      referencesProvider: true,
      // Call hierarchy over the same engine: incoming = senders, outgoing =
      // sends within a method (US-423 AC4).
      callHierarchyProvider: true,
      foldingRangeProvider: true,
      documentHighlightProvider: true,
      // Hover over selectors/classes/variables/numeric literals (US-415).
      hoverProvider: true,
      // Cartridge-aware semantic highlighting (US-422): role-accurate token
      // classification, incl. the known-class vs unknown-global distinction.
      semanticTokensProvider: {
        legend: {
          tokenTypes: [...SEMANTIC_TOKEN_TYPES],
          tokenModifiers: [...SEMANTIC_TOKEN_MODIFIERS],
        },
        full: true,
        range: true,
      },
      // Trivial quick fixes (US-414 AC4): insert a missing closer (`]`/`)`/`}`/`>`)
      // or close an unterminated string.
      codeActionProvider: { codeActionKinds: [CodeActionKind.QuickFix] },
      // Space/`:` auto-trigger selector + keyword-part completion; identifier
      // typing triggers via the client's default quick-suggestions.
      completionProvider: { triggerCharacters: [' ', ':'] },
      // Keyword-message signature help (US-425): `:`/space trigger a new (or
      // retriggered) signature popup as keyword parts are typed.
      signatureHelpProvider: { triggerCharacters: [':', ' '], retriggerCharacters: [':'] },
      // Conservative, idempotent whitespace-only formatting (US-416), off by
      // default behind `smalltalk.format.enable`. Document + range + on-type;
      // on-type dedents on `]`, re-indents after a newline / cascade `;`.
      documentFormattingProvider: true,
      documentRangeFormattingProvider: true,
      documentOnTypeFormattingProvider: { firstTriggerCharacter: ']', moreTriggerCharacter: ['\n', ';'] },
      // Scope-aware rename of temporaries/arguments/instance variables (US-426);
      // prepareRename gates renameable positions and explains rejections.
      renameProvider: { prepareProvider: true },
    },
  };
});

connection.onInitialized(async () => {
  connection.console.log('Smalltalk language server initialized.');
  // Ask the client to push `workspace/didChangeConfiguration` so edits to
  // `files.exclude` re-trigger indexing (the client does not send these by default).
  try {
    await connection.client.register(DidChangeConfigurationNotification.type, undefined);
  } catch {
    // Client without dynamic registration — the initial index still applies files.exclude.
  }
  await configureKernel();
  await rebuildIndex();
});

// Re-scan when settings change so edits to `files.exclude` and the kernel settings take effect.
connection.onDidChangeConfiguration(() => {
  void configureKernel();
  void rebuildIndex();
});

connection.onDocumentSymbol((params: DocumentSymbolParams): DocumentSymbol[] => {
  const doc = documents.get(params.textDocument.uri);
  return doc ? toDocumentSymbols(getSymbols(doc)) : [];
});

/** Pull `smalltalk.format.*` per request (pull model — always fresh, no init-only staleness). */
async function getFormatSettings(): Promise<FormatSettings> {
  let cfg: { format?: Partial<FormatSettings> } | undefined;
  try {
    cfg = (await connection.workspace.getConfiguration('smalltalk')) as typeof cfg;
  } catch {
    cfg = undefined;
  }
  const f = cfg?.format ?? {};
  return {
    enable: f.enable ?? DEFAULT_FORMAT_SETTINGS.enable,
    indentSize: f.indentSize ?? DEFAULT_FORMAT_SETTINGS.indentSize,
    cascades: f.cascades ?? DEFAULT_FORMAT_SETTINGS.cascades,
    keywordWrap: f.keywordWrap ?? DEFAULT_FORMAT_SETTINGS.keywordWrap,
    blockStyle: f.blockStyle ?? DEFAULT_FORMAT_SETTINGS.blockStyle,
  };
}

connection.onDocumentFormatting(async (params: DocumentFormattingParams): Promise<TextEdit[]> => {
  const doc = documents.get(params.textDocument.uri);
  return doc ? formatDocument(doc.getText(), params.options, await getFormatSettings()) : [];
});

connection.onDocumentRangeFormatting(async (params: DocumentRangeFormattingParams): Promise<TextEdit[]> => {
  const doc = documents.get(params.textDocument.uri);
  return doc ? formatRange(doc.getText(), params.range, params.options, await getFormatSettings()) : [];
});

connection.onDocumentOnTypeFormatting(async (params: DocumentOnTypeFormattingParams): Promise<TextEdit[]> => {
  const doc = documents.get(params.textDocument.uri);
  return doc ? formatOnType(doc.getText(), params.position, params.ch, params.options, await getFormatSettings()) : [];
});

/** (uri, text) for an ivar rename: the active doc (dirty wins) + every file that
 *  defines/extends the enclosing class. Temp/arg renames only need the active doc. */
function ivarCandidateFiles(activeUri: string, activeText: string, offset: number): FileText[] {
  const files: FileText[] = [{ uri: activeUri, text: activeText }];
  const seen = new Set<string>([activeUri]);
  const className = enclosingClassNameAt(parse(activeText).ast, offset);
  if (!className) {
    return files; // local (temp/arg) or not renameable — the active document is enough.
  }
  const wanted = new Set<string>();
  for (const e of index.all()) {
    if (e.name === className || e.containerName === className) wanted.add(e.uri);
  }
  for (const doc of documents.all()) {
    if (!seen.has(doc.uri) && wanted.has(doc.uri)) {
      seen.add(doc.uri);
      files.push({ uri: doc.uri, text: doc.getText() });
    }
  }
  for (const uri of wanted) {
    if (seen.has(uri)) continue;
    const fsPath = uriToPath(uri);
    if (!fsPath) continue;
    try {
      seen.add(uri);
      files.push({ uri, text: fs.readFileSync(fsPath, 'utf8') });
    } catch {
      // unreadable file — skip it (best-effort, never throw)
    }
  }
  return files;
}

/** Every workspace file as (uri, text) for class rename — a class may be referenced
 *  anywhere, so the whole indexed workspace ∪ open docs is in scope (open docs win). */
function allWorkspaceFiles(activeUri: string, activeText: string): FileText[] {
  const byUri = new Map<string, string>([[activeUri, activeText]]);
  for (const doc of documents.all()) {
    if (!byUri.has(doc.uri)) byUri.set(doc.uri, doc.getText());
  }
  for (const uri of new Set(index.all().map((e) => e.uri))) {
    if (byUri.has(uri)) continue;
    const fsPath = uriToPath(uri);
    if (!fsPath) continue;
    try {
      byUri.set(uri, fs.readFileSync(fsPath, 'utf8'));
    } catch {
      // unreadable file — skip it (best-effort, never throw)
    }
  }
  return [...byUri].map(([uri, text]) => ({ uri, text }));
}

/** The class-resolution world: workspace classes from the index ∪ the given files'
 *  own definitions (so an unindexed/untitled active doc still resolves), the kernel
 *  boundary from the cartridge, and namespaces from the index containerName. */
function buildClassWorld(files: FileText[]): ClassWorld {
  const nsOf = new Map<string, string | undefined>();
  for (const e of index.all()) {
    if (e.kind === SymbolKind.Class && !nsOf.has(e.name)) nsOf.set(e.name, e.containerName);
  }
  const fromFiles = buildClassWorldFromFiles(files, () => false);
  return {
    isKnownWorkspaceClass: (name) => nsOf.has(name) || fromFiles.isKnownWorkspaceClass(name),
    isKernelClass: (name) => kernelService.hasClass(name),
    namespaceOf: (name) => (nsOf.has(name) ? nsOf.get(name) : fromFiles.namespaceOf(name)),
  };
}

/** Candidate (uri, text) files + the class world for a rename: a class rename scans
 *  the whole workspace; an ivar only the class's files; a local just the active doc. */
function renameContext(
  activeUri: string,
  activeText: string,
  ast: ReturnType<typeof getAst>,
  tokens: ReturnType<typeof getTokens>,
  offset: number,
): { files: FileText[]; world: ClassWorld } {
  const active: FileText[] = [{ uri: activeUri, text: activeText }];
  const world0 = buildClassWorld(active);
  if (renameKindAt(ast, tokens, offset, active, world0) === 'class') {
    const files = allWorkspaceFiles(activeUri, activeText);
    return { files, world: buildClassWorld(files) };
  }
  return { files: ivarCandidateFiles(activeUri, activeText, offset), world: world0 };
}

connection.onPrepareRename((params: PrepareRenameParams): Range | null => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) {
    return null;
  }
  const offset = doc.offsetAt(params.position);
  const { files, world } = renameContext(doc.uri, doc.getText(), getAst(doc), getTokens(doc), offset);
  const result = prepareRenameAt(getAst(doc), getTokens(doc), getSymbols(doc), offset, files, world);
  if ('reject' in result) {
    throw new ResponseError(ErrorCodes.InvalidRequest, result.reject);
  }
  return result.range;
});

connection.onRenameRequest((params: RenameParams): WorkspaceEdit => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) {
    return { changes: {} };
  }
  const offset = doc.offsetAt(params.position);
  const { files, world } = renameContext(doc.uri, doc.getText(), getAst(doc), getTokens(doc), offset);
  const result = renameAt(doc.uri, offset, params.newName, files, world);
  if ('reject' in result) {
    throw new ResponseError(ErrorCodes.InvalidRequest, result.reject);
  }
  // A rename spanning multiple files asks for confirmation via the Refactor Preview.
  return withMultiFileConfirmation(result, changeAnnotationSupport, (uri) => documents.get(uri)?.version ?? null);
});

connection.onWorkspaceSymbol((params: WorkspaceSymbolParams): WorkspaceSymbol[] =>
  toWorkspaceSymbols(index.query(params.query)),
);

connection.onDefinition((params: DefinitionParams): Location[] | LocationLink[] => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) {
    return [];
  }
  const query = resolveDefinitionQuery(doc.getText(), doc.offsetAt(params.position));
  if (!query) {
    return [];
  }
  // A message send resolves to the plural union of ALL implementors across the
  // workspace + cartridge — never collapsed to one target (US-423 AC3). A class
  // reference keeps the workspace symbol jump (US-412).
  if (query.target === 'selector') {
    const sources = buildXrefSources(query.name);
    return definitionLinkSupport
      ? definitionLinks(sources, query.name)
      : definitionLocations(sources, query.name);
  }
  return findDefinitions(index, query, doc.uri);
});

connection.onReferences((params: ReferenceParams): Location[] => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) {
    return [];
  }
  const query = resolveDefinitionQuery(doc.getText(), doc.offsetAt(params.position));
  if (!query || query.target !== 'selector') {
    return [];
  }
  return referenceLocations(buildXrefSources(query.name), query.name);
});

/** Assemble the two-tier facts for one selector: workspace implementors (from the
 *  symbol index), workspace senders (from the live xref), and the cartridge
 *  cross-reference half, plus the known-class predicate for hint ranking (AC6). */
function buildXrefSources(selector: string): XrefSources {
  const entries = index.all();
  const workspaceImplementors: WorkspaceMethodRef[] = entries
    .filter((e: IndexEntry) => e.kind === SymbolKind.Method && e.name === selector)
    .map((e) => ({
      uri: e.uri,
      side: e.classSide ? 'class' : 'instance',
      range: e.selectionRange,
      ...(e.containerName !== undefined ? { className: e.containerName } : {}),
    }));
  const workspaceClasses = new Set(
    entries.filter((e) => e.kind === SymbolKind.Class || e.kind === SymbolKind.Namespace).map((e) => e.name),
  );
  return {
    workspaceImplementors,
    workspaceSenders: workspaceXref.sendersOf(selector),
    cartridgeSenders: kernelService.crossReferenceSenders(selector),
    cartridgeImplementors: kernelService.crossReferenceImplementors(selector),
    // Label the cartridge tier with the SAME identity the status bar shows, so
    // the panel's per-row provenance and the status bar agree (terminology parity).
    ...(kernelService.cartridgeId
      ? { cartridge: { ...kernelService.cartridgeId, label: kernelService.identity.label } }
      : {}),
    isKnownClass: (name) => workspaceClasses.has(name) || kernelService.hasClass(name),
  };
}

/** Custom request backing the branded `Smalltalk: Senders of…` / `Implementors
 *  of…` commands (US-423 AC2). Resolves the selector under the cursor (method
 *  def or send), then returns the honest union + per-row provenance the client
 *  renders as a tree. Returns null when the cursor is not on a selector. */
const CROSS_REFERENCE_REQUEST = 'smalltalk/crossReference';
interface CrossReferenceParams {
  readonly textDocument: { readonly uri: string };
  readonly position: { readonly line: number; readonly character: number };
  readonly direction: XrefDirection;
}
connection.onRequest(CROSS_REFERENCE_REQUEST, (params: CrossReferenceParams): CrossReferenceResult | null => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) {
    return null;
  }
  const target = resolveCallTarget(getAst(doc), getTokens(doc), doc.offsetAt(params.position));
  if (!target) {
    return null;
  }
  const sources = buildXrefSources(target.selector);
  const refs =
    params.direction === 'senders'
      ? resolveSenders(sources, target.selector)
      : resolveImplementors(sources, target.selector);
  return buildCrossReference(params.direction, target.selector, refs);
});

connection.languages.callHierarchy.onPrepare((params: CallHierarchyPrepareParams): CallHierarchyItem[] => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) {
    return [];
  }
  const target = resolveCallTarget(getAst(doc), getTokens(doc), doc.offsetAt(params.position));
  return prepareCallHierarchy(target, doc.uri);
});

connection.languages.callHierarchy.onIncomingCalls(
  (params: CallHierarchyIncomingCallsParams): CallHierarchyIncomingCall[] => {
    const data = params.item.data as CallItemData | undefined;
    if (!data) {
      return [];
    }
    // Incoming = everyone who sends this selector (the lexical union, AC4).
    return incomingCalls(resolveSenders(buildXrefSources(data.selector), data.selector));
  },
);

connection.languages.callHierarchy.onOutgoingCalls(
  (params: CallHierarchyOutgoingCallsParams): CallHierarchyOutgoingCall[] => {
    const data = params.item.data as CallItemData | undefined;
    // Outgoing = the sends inside this method's body (workspace tier only).
    if (!data || data.side === undefined) {
      return [];
    }
    return outgoingCalls(workspaceXref.sendsFrom(data.uri, data.className, data.side, data.selector));
  },
);

connection.onFoldingRanges((params: FoldingRangeParams): FoldingRange[] => {
  const doc = documents.get(params.textDocument.uri);
  return doc ? toFoldingRanges(getAst(doc), getTokens(doc)) : [];
});

connection.onDocumentHighlight((params: DocumentHighlightParams): DocumentHighlight[] => {
  const doc = documents.get(params.textDocument.uri);
  return doc ? documentHighlightsAt(getAst(doc), getTokens(doc), doc.offsetAt(params.position)) : [];
});

connection.onCodeAction((params: CodeActionParams): CodeAction[] => {
  const doc = documents.get(params.textDocument.uri);
  return toCodeActions(params.textDocument.uri, params.context.diagnostics, doc?.getText() ?? '');
});

connection.onCompletion((params: CompletionParams): CompletionItem[] => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) {
    return [];
  }
  const entries = index.all();
  const selectors: SelectorCandidate[] = [
    ...entries
      .filter((e) => e.kind === SymbolKind.Method)
      .map((e) => ({ selector: e.name, provenance: Provenance.Workspace })),
    ...kernelService.selectors().map((s) => ({ selector: s.selector, provenance: s.provenance })),
  ];
  const classes: ClassCandidate[] = [
    ...entries
      .filter((e) => e.kind === SymbolKind.Class || e.kind === SymbolKind.Namespace)
      .map((e) => ({ name: e.name, provenance: Provenance.Workspace })),
    ...kernelService.classes().map((c) => ({ name: c.name, provenance: c.provenance })),
  ];
  return completionsAt(
    doc.offsetAt(params.position),
    doc.getText(),
    getTokens(doc),
    getAst(doc),
    getSymbols(doc),
    selectors,
    classes,
  );
});

connection.onSignatureHelp((params: SignatureHelpParams): SignatureHelp | null => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) {
    return null;
  }
  // Only keyword selectors carry a parameter sequence worth tracking; derive the
  // keyword parts (`at:put:` → ["at:","put:"]) from the selector text (US-425).
  const keywordsOf = (selector: string): string[] => selector.match(/[^:]+:/g) ?? [selector];
  const isKeyword = (selector: string): boolean => selector.includes(':');
  const signatures: SignatureCandidate[] = [
    ...index
      .all()
      .filter((e) => e.kind === SymbolKind.Method && isKeyword(e.name))
      .map((e) => ({ selector: e.name, keywords: keywordsOf(e.name), provenance: Provenance.Workspace })),
    ...kernelService
      .selectors()
      .filter((s) => isKeyword(s.selector))
      .map((s) => ({ selector: s.selector, keywords: keywordsOf(s.selector), provenance: s.provenance })),
  ];
  return signatureHelpAt(doc.offsetAt(params.position), doc.getText(), getTokens(doc), signatures);
});

connection.onHover((params: HoverParams): Hover | null => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) {
    return null;
  }
  const entries = index.all();
  const classNames = new Set(
    entries.filter((e) => e.kind === SymbolKind.Class || e.kind === SymbolKind.Namespace).map((e) => e.name),
  );
  // Workspace classes' immediate superclass + comment, for the chain walk and
  // class hover (prefer the user's own definition over the kernel when both know it).
  const workspaceSuper = new Map<string, string>();
  const workspaceClassComment = new Map<string, string>();
  for (const e of entries) {
    if (e.kind === SymbolKind.Class) {
      if (e.superclass !== undefined && !workspaceSuper.has(e.name)) {
        workspaceSuper.set(e.name, e.superclass);
      }
      if (e.comment !== undefined && !workspaceClassComment.has(e.name)) {
        workspaceClassComment.set(e.name, e.comment);
      }
    }
  }
  const ctx: HoverContext = {
    isClass: (name) => classNames.has(name) || kernelService.hasClass(name),
    superclassOf: (name) => workspaceSuper.get(name) ?? kernelService.superclassOf(name),
    classComment: (name) => workspaceClassComment.get(name) ?? kernelService.classComment(name),
    implementorsOf: (selector) => {
      // Workspace methods carry the user's own comments (no licence gate); kernel
      // implementors carry prose only when the active source allows it (§4a).
      const workspace: HoverImplementor[] = entries
        .filter((e) => e.kind === SymbolKind.Method && e.name === selector && e.containerName !== undefined)
        .map((e) => ({
          className: e.containerName as string,
          provenance: Provenance.Workspace,
          ...(e.comment ? { comment: e.comment } : {}),
        }));
      const kernel: HoverImplementor[] = kernelService.implementorsOf(selector).map((c) => ({
        className: c.name,
        provenance: c.provenance,
        ...(c.comment ? { comment: c.comment } : {}),
      }));
      return [...workspace, ...kernel];
    },
  };
  return hoverAt(doc.offsetAt(params.position), doc.getText(), getTokens(doc), getAst(doc), getSymbols(doc), ctx);
});

/** Build the semantic-token context (US-422): a class is `class` iff known in
 *  workspace ∪ the active cartridge, with the source recorded so a cartridge
 *  (kernel) class can carry the `defaultLibrary` modifier; otherwise a global. */
function semanticContext(): SemanticTokenContext {
  const workspaceClasses = new Set(
    index
      .all()
      .filter((e) => e.kind === SymbolKind.Class || e.kind === SymbolKind.Namespace)
      .map((e) => e.name),
  );
  return {
    hasCartridge: kernelService.isEnabled,
    classOrigin: (name) =>
      workspaceClasses.has(name) ? 'workspace' : kernelService.hasClass(name) ? 'cartridge' : undefined,
  };
}

connection.languages.semanticTokens.on((params: SemanticTokensParams): SemanticTokens => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) {
    return { data: [] };
  }
  return { data: semanticTokensFull(getAst(doc), getSymbols(doc), getTokens(doc), semanticContext()) };
});

connection.languages.semanticTokens.onRange((params: SemanticTokensRangeParams): SemanticTokens => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) {
    return { data: [] };
  }
  return { data: semanticTokensRange(getAst(doc), getSymbols(doc), getTokens(doc), semanticContext(), params.range) };
});

// Keep the workspace index fresh, debounced so rapid typing does not reparse on
// every keystroke. (The outline reads the live doc via the version-keyed cache,
// so it stays immediate.)
// `onDidChangeContent` fires on open and on every edit, so it covers both AC1
// triggers (publish on open + change).
documents.onDidChangeContent((e) => {
  scheduleIndex(e.document.uri, e.document.getText());
  scheduleDiagnostics(e.document.uri);
});

documents.onDidClose((e) => {
  invalidate(e.document.uri);
  const timer = indexTimers.get(e.document.uri);
  if (timer) {
    clearTimeout(timer);
    indexTimers.delete(e.document.uri);
  }
  // Stop any pending diagnostics publish and clear the squiggles for the closed doc.
  const diagTimer = diagnosticTimers.get(e.document.uri);
  if (diagTimer) {
    clearTimeout(diagTimer);
    diagnosticTimers.delete(e.document.uri);
  }
  connection.sendDiagnostics({ uri: e.document.uri, diagnostics: [] });
  // Revert the index to the on-disk version of the closed file (drop unsaved edits).
  const fsPath = uriToPath(e.document.uri);
  if (fsPath !== undefined && fs.existsSync(fsPath)) {
    try {
      indexDoc(e.document.uri, fs.readFileSync(fsPath, 'utf8'));
      return;
    } catch {
      // fall through to removal
    }
  }
  index.removeFile(e.document.uri);
  workspaceXref.removeFile(e.document.uri);
});

/** Re-pull `files.exclude`, re-scan the folders, then re-apply non-excluded open docs. */
async function rebuildIndex(): Promise<void> {
  try {
    const files = (await connection.workspace.getConfiguration('files')) as
      | { exclude?: Record<string, boolean> }
      | undefined;
    currentExclude = excludeFromConfig(files?.exclude);
  } catch {
    currentExclude = defaultExclude;
  }
  index.clear();
  workspaceXref.clear();
  // One disk walk feeds both the symbol index and the cross-reference index.
  for (const folder of workspaceFolders) {
    walkStFiles(folder, currentExclude, (uri, text) => {
      index.setFile(uri, text);
      workspaceXref.setFile(uri, text);
    });
  }
  for (const doc of documents.all()) {
    indexDoc(doc.uri, doc.getText());
  }
  connection.console.log(`Indexed ${index.size} Smalltalk file(s).`);
}

/** Pull `smalltalk.completion.*` and re-resolve the kernel tier. Never throws. */
async function configureKernel(): Promise<void> {
  let cfg:
    | { gnuSmalltalkPath?: string; completion?: { kernelLibrary?: KernelLibrarySetting; kernelPath?: string } }
    | undefined;
  try {
    cfg = (await connection.workspace.getConfiguration('smalltalk')) as typeof cfg;
  } catch {
    cfg = undefined;
  }
  const requested = cfg?.completion?.kernelLibrary ?? 'auto';
  kernelService.configure({
    kernelLibrary: requested,
    kernelPath: cfg?.completion?.kernelPath || undefined,
    gnuSmalltalkPath: cfg?.gnuSmalltalkPath || undefined,
  });
  const identity = kernelService.identity;
  connection.console.log(`Kernel completion: ${identity.label}.`);
  // Tell the client so it can show the active source in the status bar and, on a
  // fallback to the bundle, a one-time notice (AC7).
  connection.sendNotification(KERNEL_STATUS_NOTIFICATION, { ...identity, requested });
}

// Publish parser diagnostics for a document, debounced (AC1). Reads the live doc
// via the version-keyed cache, so it reflects the latest edit. Never throws — the
// front end returns diagnostics, not errors; on any failure we publish nothing.
function scheduleDiagnostics(uri: string): void {
  const existing = diagnosticTimers.get(uri);
  if (existing) {
    clearTimeout(existing);
  }
  diagnosticTimers.set(
    uri,
    setTimeout(() => {
      diagnosticTimers.delete(uri);
      publishDiagnostics(uri);
    }, DIAGNOSTIC_DEBOUNCE_MS),
  );
}

function publishDiagnostics(uri: string): void {
  const doc = documents.get(uri);
  if (!doc) {
    return;
  }
  try {
    connection.sendDiagnostics({ uri, diagnostics: toDiagnostics(getDiagnostics(doc)) });
  } catch {
    // The front end shouldn't throw; if it ever does, leave existing diagnostics untouched.
  }
}

function scheduleIndex(uri: string, text: string): void {
  const existing = indexTimers.get(uri);
  if (existing) {
    clearTimeout(existing);
  }
  indexTimers.set(
    uri,
    setTimeout(() => {
      indexTimers.delete(uri);
      indexDoc(uri, text);
    }, INDEX_DEBOUNCE_MS),
  );
}

/** Whether an open document belongs to the user's project — i.e. lives under a
 *  workspace folder. A file opened for *viewing* from outside (e.g. a kernel
 *  source file reached by clicking a cartridge cross-reference row) is NOT
 *  workspace source and must not be indexed as such, or its kernel classes would
 *  mislabel as `workspace` and shadow the cartridge tier. With no folder open
 *  (single-file mode), or a non-file URI (an `untitled:` buffer), the active
 *  document IS the context, so index it. */
function isInWorkspace(uri: string): boolean {
  if (workspaceFolders.length === 0) {
    return true;
  }
  const fsPath = uriToPath(uri);
  if (fsPath === undefined) {
    return true; // untitled / virtual document — the active editing context
  }
  return workspaceFolders.some((folder) => fsPath === folder || fsPath.startsWith(folder + path.sep));
}

/** Index a document (symbols + cross-reference) unless it is outside the
 *  workspace or excluded by `files.exclude`. */
function indexDoc(uri: string, text: string): void {
  const fsPath = uriToPath(uri);
  if (!isInWorkspace(uri) || (fsPath !== undefined && currentExclude(fsPath, path.basename(fsPath), false))) {
    index.removeFile(uri);
    workspaceXref.removeFile(uri);
    return;
  }
  index.setFile(uri, text);
  workspaceXref.setFile(uri, text);
}

function uriToPath(uri: string): string | undefined {
  try {
    return fileURLToPath(uri);
  } catch {
    return undefined;
  }
}

documents.listen(connection);
connection.listen();
