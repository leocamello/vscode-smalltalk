import { fileURLToPath } from 'node:url';
import fs from 'node:fs';
import path from 'node:path';
import {
  createConnection,
  ProposedFeatures,
  DidChangeConfigurationNotification,
  TextDocuments,
  TextDocumentSyncKind,
  CodeActionKind,
  type CodeAction,
  type CodeActionParams,
  type CompletionItem,
  type CompletionParams,
  type DefinitionParams,
  type DocumentHighlight,
  type DocumentHighlightParams,
  type DocumentSymbol,
  type DocumentSymbolParams,
  type FoldingRange,
  type FoldingRangeParams,
  type InitializeParams,
  type InitializeResult,
  type Location,
  type WorkspaceSymbol,
  type WorkspaceSymbolParams,
} from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { getAst, getDiagnostics, getSymbols, getTokens, invalidate } from './documents/parseCache';
import { toDiagnostics } from './providers/diagnostics';
import { toCodeActions } from './providers/codeAction';
import { toDocumentSymbols } from './providers/documentSymbol';
import { toFoldingRanges } from './providers/foldingRange';
import { documentHighlightsAt } from './providers/documentHighlight';
import { WorkspaceIndex, defaultExclude, excludeFromConfig, type ExcludePredicate } from './providers/workspaceIndex';
import { toWorkspaceSymbols } from './providers/workspaceSymbol';
import { findDefinitions, resolveDefinitionQuery } from './providers/definition';
import { completionsAt, type ClassCandidate, type SelectorCandidate } from './providers/completion';
import { KernelIndexService, type KernelLibrarySetting } from './kernel/kernelIndexService';
import { Provenance } from './kernel/model';
import { SymbolKind } from './parser/symbols';

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);
const index = new WorkspaceIndex();
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
  // Have a usable kernel immediately (refined from config in onInitialized).
  kernelService.configure({ kernelLibrary: 'auto' });
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      documentSymbolProvider: true,
      workspaceSymbolProvider: true,
      definitionProvider: true,
      foldingRangeProvider: true,
      documentHighlightProvider: true,
      // Trivial quick fixes (US-414 AC4): insert a missing closer (`]`/`)`/`}`/`>`)
      // or close an unterminated string.
      codeActionProvider: { codeActionKinds: [CodeActionKind.QuickFix] },
      // Space/`:` auto-trigger selector + keyword-part completion; identifier
      // typing triggers via the client's default quick-suggestions.
      completionProvider: { triggerCharacters: [' ', ':'] },
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

connection.onWorkspaceSymbol((params: WorkspaceSymbolParams): WorkspaceSymbol[] =>
  toWorkspaceSymbols(index.query(params.query)),
);

connection.onDefinition((params: DefinitionParams): Location[] => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) {
    return [];
  }
  const query = resolveDefinitionQuery(doc.getText(), doc.offsetAt(params.position));
  return query ? findDefinitions(index, query, doc.uri) : [];
});

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
  for (const folder of workspaceFolders) {
    index.indexFolder(folder, currentExclude);
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

/** Index a document unless `files.exclude` excludes it. */
function indexDoc(uri: string, text: string): void {
  const fsPath = uriToPath(uri);
  if (fsPath !== undefined && currentExclude(fsPath, path.basename(fsPath), false)) {
    index.removeFile(uri);
    return;
  }
  index.setFile(uri, text);
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
