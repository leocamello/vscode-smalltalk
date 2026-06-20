import {
  createConnection,
  ProposedFeatures,
  TextDocuments,
  TextDocumentSyncKind,
  type DocumentSymbol,
  type DocumentSymbolParams,
  type InitializeParams,
  type InitializeResult,
} from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { getSymbols, invalidate } from './documents/parseCache';
import { toDocumentSymbols } from './providers/documentSymbol';

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

connection.onInitialize((_params: InitializeParams): InitializeResult => {
  // Language-intelligence providers are built on the US-411 front end and run
  // with no `gst`. US-412 adds document symbols (outline); workspace symbols and
  // go-to-definition follow in the next slices.
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      documentSymbolProvider: true,
    },
  };
});

connection.onInitialized(() => {
  connection.console.log('Smalltalk language server initialized.');
});

connection.onDocumentSymbol((params: DocumentSymbolParams): DocumentSymbol[] => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) {
    return [];
  }
  return toDocumentSymbols(getSymbols(doc));
});

// Drop cached symbols when a document is closed.
documents.onDidClose((e) => invalidate(e.document.uri));

documents.listen(connection);
connection.listen();
