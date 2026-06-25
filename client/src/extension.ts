import * as path from 'path';
import { commands, StatusBarAlignment, type StatusBarItem, type ExtensionContext, window } from 'vscode';
import {
  LanguageClient,
  TransportKind,
  type LanguageClientOptions,
  type ServerOptions,
} from 'vscode-languageclient/node';
import { runCurrentFile } from './commands/runCurrentFile';
import { registerCrossReferences } from './crossReferences';

let client: LanguageClient | undefined;

/** Custom notification from the server: the resolved kernel-completion source (US-413). */
const KERNEL_STATUS_NOTIFICATION = 'smalltalk/kernelStatus';

interface KernelStatus {
  readonly source: 'installed' | 'bundled' | 'off';
  readonly label: string;
  /** The configured `kernelLibrary` (`auto` falling back to `bundled` is a fallback). */
  readonly requested?: 'auto' | 'bundled' | 'off';
}

export function activate(context: ExtensionContext): void {
  // Commands (workflow features; independent of the language server).
  context.subscriptions.push(
    commands.registerCommand('smalltalk.runCurrentFile', runCurrentFile),
  );

  // Status bar: which kernel-library completions are active (US-413 / AC7).
  const kernelStatus = window.createStatusBarItem(StatusBarAlignment.Right, 90);
  kernelStatus.command = {
    title: 'Configure Smalltalk kernel completions',
    command: 'workbench.action.openSettings',
    arguments: ['smalltalk.completion.kernelLibrary'],
  };
  context.subscriptions.push(kernelStatus);

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

  // Senders/Implementors commands + the cross-reference panel + the read-only
  // cartridge virtual-document provider (US-423 AC2). Registered eagerly; the
  // requests they fire wait for the client to be ready.
  registerCrossReferences(context, client);

  let fallbackNoticeShown = false;
  void client.start().then(() => {
    client?.onNotification(KERNEL_STATUS_NOTIFICATION, (status: KernelStatus) => {
      updateKernelStatusBar(kernelStatus, status);
      // One-time notice when `auto` falls back to the bundled reference.
      if (!fallbackNoticeShown && status.requested === 'auto' && status.source === 'bundled') {
        fallbackNoticeShown = true;
        void window
          .showInformationMessage(
            'No GNU Smalltalk installation found — kernel completions use a bundled reference ' +
              '(GNU Smalltalk 3.2.5). Install gst or set "smalltalk.completion.kernelPath" for ' +
              'version-correct completions.',
            'Open Settings',
          )
          .then((choice) => {
            if (choice === 'Open Settings') {
              void commands.executeCommand('workbench.action.openSettings', 'smalltalk.completion');
            }
          });
      }
    });
  });
}

function updateKernelStatusBar(item: StatusBarItem, status: KernelStatus): void {
  if (status.source === 'off') {
    item.text = '$(circle-slash) Smalltalk kernel: off';
    item.tooltip = 'Kernel-library completions are disabled. Click to change.';
  } else {
    item.text = `$(library) Smalltalk kernel: ${status.label}`;
    item.tooltip = `Kernel-library completions: ${status.label}. Click to change.`;
  }
  item.show();
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
