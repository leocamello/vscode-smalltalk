import { fileURLToPath } from 'node:url';
import fs from 'node:fs';
import {
  createConnection,
  ProposedFeatures,
  TextDocuments,
  TextDocumentSyncKind,
  type DocumentSymbol,
  type DocumentSymbolParams,
  type InitializeParams,
  type InitializeResult,
  type WorkspaceSymbol,
  type WorkspaceSymbolParams,
} from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { getSymbols, invalidate } from './documents/parseCache';
import { toDocumentSymbols } from './providers/documentSymbol';
import { WorkspaceIndex, defaultExclude, excludeFromConfig, type ExcludePredicate } from './providers/workspaceIndex';
import { toWorkspaceSymbols } from './providers/workspaceSymbol';

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);
const index = new WorkspaceIndex();

let workspaceFolders: string[] = [];

connection.onInitialize((params: InitializeParams): InitializeResult => {
  // Language-intelligence providers are built on the US-411 front end and run
  // with no `gst`. US-412: document symbols (outline) + workspace symbol search;
  // go-to-definition follows in the next slice.
  workspaceFolders = (params.workspaceFolders ?? [])
    .map((folder) => uriToPath(folder.uri))
    .filter((p): p is string => p !== undefined);
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      documentSymbolProvider: true,
      workspaceSymbolProvider: true,
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

// Keep the index fresh: open documents reflect their live text.
documents.onDidChangeContent((e) => index.setFile(e.document.uri, e.document.getText()));

documents.onDidClose((e) => {
  invalidate(e.document.uri);
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

function uriToPath(uri: string): string | undefined {
  try {
    return fileURLToPath(uri);
  } catch {
    return undefined;
  }
}

documents.listen(connection);
connection.listen();
