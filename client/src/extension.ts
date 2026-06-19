import * as path from 'path';
import { type ExtensionContext, window } from 'vscode';
import {
  LanguageClient,
  TransportKind,
  type LanguageClientOptions,
  type ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext): void {
  // The server is bundled next to the client in dist/.
  const serverModule = context.asAbsolutePath(path.join('dist', 'server.js'));
  const serverOptions: ServerOptions = {
    run: { module: serverModule, transport: TransportKind.ipc },
    debug: { module: serverModule, transport: TransportKind.ipc },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ language: 'smalltalk' }],
    outputChannel: window.createOutputChannel('Smalltalk Language Server'),
  };

  // The client id ('smalltalk') is what binds the `smalltalk.trace.server`
  // setting to this client. start() launches the server and performs the
  // handshake; the client restarts the server on crash by default.
  client = new LanguageClient(
    'smalltalk',
    'Smalltalk Language Server',
    serverOptions,
    clientOptions,
  );

  void client.start();
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
