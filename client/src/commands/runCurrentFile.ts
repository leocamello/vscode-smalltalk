import * as fs from 'fs';
import * as path from 'path';
import { commands, window, workspace, type Terminal } from 'vscode';
import { buildRunCommand, defaultExeNames, resolveGst } from '../gstLocator';

const TERMINAL_NAME = 'Smalltalk';

function isExecutableFile(candidate: string): boolean {
  try {
    if (!fs.statSync(candidate).isFile()) {
      return false;
    }
    if (process.platform === 'win32') {
      return true;
    }
    fs.accessSync(candidate, fs.constants.X_OK);
    return true;
  } catch {
    return false;
  }
}

function getOrCreateTerminal(): Terminal {
  return window.terminals.find((t) => t.name === TERMINAL_NAME) ?? window.createTerminal(TERMINAL_NAME);
}

/** Command handler for `smalltalk.runCurrentFile`. */
export async function runCurrentFile(): Promise<void> {
  const editor = window.activeTextEditor;
  if (!editor || editor.document.languageId !== 'smalltalk') {
    void window.showErrorMessage('Open a Smalltalk file to run it.');
    return;
  }

  // Execute the current on-disk content.
  await editor.document.save();
  const filePath = editor.document.uri.fsPath;

  const configuredPath = workspace
    .getConfiguration('smalltalk')
    .get<string>('gnuSmalltalkPath', '');

  const resolution = resolveGst({
    configuredPath,
    pathEnv: process.env.PATH,
    delimiter: path.delimiter,
    exeNames: defaultExeNames(process.platform),
    isExecutable: isExecutableFile,
  });

  if (!resolution) {
    const openSettings = 'Open Settings';
    const choice = await window.showErrorMessage(
      'GNU Smalltalk (gst) was not found. Set "smalltalk.gnuSmalltalkPath" or add gst to your PATH.',
      openSettings,
    );
    if (choice === openSettings) {
      void commands.executeCommand('workbench.action.openSettings', 'smalltalk.gnuSmalltalkPath');
    }
    return;
  }

  const terminal = getOrCreateTerminal();
  terminal.show(true);
  terminal.sendText(buildRunCommand(resolution.path, filePath));
}
