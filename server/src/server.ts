import {
  createConnection,
  ProposedFeatures,
  TextDocuments,
  TextDocumentSyncKind,
  type InitializeParams,
  type InitializeResult,
} from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

connection.onInitialize((_params: InitializeParams): InitializeResult => {
  // No-op scaffold (US-410): advertise capabilities only. Feature providers
  // (document symbols, definition, completion, diagnostics, hover, formatting)
  // are added in US-412..416 once the parser (US-411) exists.
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
    },
  };
});

connection.onInitialized(() => {
  connection.console.log('Smalltalk language server initialized.');
});

// Track open documents (no handlers yet — wired up by later stories).
documents.listen(connection);
connection.listen();
