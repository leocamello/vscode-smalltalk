import { fileURLToPath } from 'node:url';
import fs from 'node:fs';
import path from 'node:path';
import {
  createConnection,
  ProposedFeatures,
  DidChangeConfigurationNotification,
  TextDocuments,
  TextDocumentSyncKind,
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
import { getAst, getSymbols, getTokens, invalidate } from './documents/parseCache';
import { toDocumentSymbols } from './providers/documentSymbol';
import { toFoldingRanges } from './providers/foldingRange';
import { documentHighlightsAt } from './providers/documentHighlight';
import { WorkspaceIndex, defaultExclude, excludeFromConfig, type ExcludePredicate } from './providers/workspaceIndex';
import { toWorkspaceSymbols } from './providers/workspaceSymbol';
import { findDefinitions, resolveDefinitionQuery } from './providers/definition';

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);
const index = new WorkspaceIndex();

const INDEX_DEBOUNCE_MS = 250;
const indexTimers = new Map<string, ReturnType<typeof setTimeout>>();
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
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      documentSymbolProvider: true,
      workspaceSymbolProvider: true,
      definitionProvider: true,
      foldingRangeProvider: true,
      documentHighlightProvider: true,
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
  await rebuildIndex();
});

// Re-scan when settings change so edits to `files.exclude` take effect.
connection.onDidChangeConfiguration(() => {
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

// Keep the workspace index fresh, debounced so rapid typing does not reparse on
// every keystroke. (The outline reads the live doc via the version-keyed cache,
// so it stays immediate.)
documents.onDidChangeContent((e) => scheduleIndex(e.document.uri, e.document.getText()));

documents.onDidClose((e) => {
  invalidate(e.document.uri);
  const timer = indexTimers.get(e.document.uri);
  if (timer) {
    clearTimeout(timer);
    indexTimers.delete(e.document.uri);
  }
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
