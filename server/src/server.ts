import { fileURLToPath } from 'node:url';
import fs from 'node:fs';
import {
  createConnection,
  ProposedFeatures,
  TextDocuments,
  TextDocumentSyncKind,
  type DefinitionParams,
  type DocumentSymbol,
  type DocumentSymbolParams,
  type InitializeParams,
  type InitializeResult,
  type Location,
  type WorkspaceSymbol,
  type WorkspaceSymbolParams,
} from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { getSymbols, invalidate } from './documents/parseCache';
import { toDocumentSymbols } from './providers/documentSymbol';
import { WorkspaceIndex, defaultExclude, excludeFromConfig, type ExcludePredicate } from './providers/workspaceIndex';
import { toWorkspaceSymbols } from './providers/workspaceSymbol';
import { findDefinitions, resolveDefinitionQuery } from './providers/definition';

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);
const index = new WorkspaceIndex();

const INDEX_DEBOUNCE_MS = 250;
const indexTimers = new Map<string, ReturnType<typeof setTimeout>>();
let workspaceFolders: string[] = [];

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
    },
  };
});

connection.onInitialized(async () => {
  connection.console.log('Smalltalk language server initialized.');
  let exclude: ExcludePredicate = defaultExclude;
  try {
    const files = (await connection.workspace.getConfiguration('files')) as
      | { exclude?: Record<string, boolean> }
      | undefined;
    exclude = excludeFromConfig(files?.exclude);
  } catch {
    // Client without configuration support — fall back to the default exclude.
  }
  for (const folder of workspaceFolders) {
    index.indexFolder(folder, exclude);
  }
  connection.console.log(`Indexed ${index.size} Smalltalk file(s).`);
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
      index.setFile(e.document.uri, fs.readFileSync(fsPath, 'utf8'));
      return;
    } catch {
      // fall through to removal
    }
  }
  index.removeFile(e.document.uri);
});

function scheduleIndex(uri: string, text: string): void {
  const existing = indexTimers.get(uri);
  if (existing) {
    clearTimeout(existing);
  }
  indexTimers.set(
    uri,
    setTimeout(() => {
      indexTimers.delete(uri);
      index.setFile(uri, text);
    }, INDEX_DEBOUNCE_MS),
  );
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
