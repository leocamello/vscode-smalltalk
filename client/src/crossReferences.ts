// Branded "Senders of…" / "Implementors of…" UI (US-423, Slice C / AC2).
//
// The server's `smalltalk/crossReference` request returns an honest, ranked
// LEXICAL UNION (workspace + cartridge) with a header disclaimer + per-row
// provenance. This module renders it natively:
//   - a TreeView whose root is the header (title + count + the union/uncertainty
//     contract as its tooltip) and whose children are the navigable rows;
//   - a read-only `smalltalk-cartridge:` virtual document so cartridge rows
//     (which have no bundled source body) still peek/open instead of erroring.
// Clicking a row reveals its location; cartridge rows open the virtual doc.

import {
  commands,
  EventEmitter,
  Selection,
  ThemeIcon,
  TreeItem,
  TreeItemCollapsibleState,
  Uri,
  window,
  workspace,
  type ExtensionContext,
  type TextDocumentContentProvider,
  type TreeDataProvider,
} from 'vscode';
import type { LanguageClient } from 'vscode-languageclient/node';

type XrefDirection = 'senders' | 'implementors';

interface XrefResultRange {
  readonly start: { readonly line: number; readonly character: number };
  readonly end: { readonly line: number; readonly character: number };
}

interface CrossReferenceRow {
  readonly uri: string;
  readonly range: XrefResultRange;
  readonly label: string;
  readonly detail: string;
  readonly provenance: string;
  readonly kind: 'sender' | 'implementor';
}

interface CrossReferenceResult {
  readonly direction: XrefDirection;
  readonly selector: string;
  readonly title: string;
  readonly disclaimer: string;
  readonly count: number;
  readonly rows: CrossReferenceRow[];
}

const CARTRIDGE_SCHEME = 'smalltalk-cartridge';
const VIEW_ID = 'smalltalkCrossReferences';

type TreeNode = { readonly type: 'header' } | { readonly type: 'row'; readonly row: CrossReferenceRow };

/** Backs the Senders/Implementors panel view: a header node + the result rows. */
class CrossReferenceTree implements TreeDataProvider<TreeNode> {
  private result: CrossReferenceResult | undefined;
  private readonly changed = new EventEmitter<void>();
  readonly onDidChangeTreeData = this.changed.event;

  setResult(result: CrossReferenceResult | undefined): void {
    this.result = result;
    this.changed.fire();
  }

  getChildren(node?: TreeNode): TreeNode[] {
    if (!this.result) {
      return [];
    }
    if (!node) {
      return [{ type: 'header' }];
    }
    return node.type === 'header' ? this.result.rows.map((row) => ({ type: 'row', row })) : [];
  }

  getTreeItem(node: TreeNode): TreeItem {
    const result = this.result;
    if (node.type === 'header' && result) {
      const item = new TreeItem(`${result.title} — ${result.count}`, TreeItemCollapsibleState.Expanded);
      // The union/uncertainty contract lives on the header (AC2). Shown inline as
      // a description and in full on hover.
      item.description = 'lexical union — dynamic dispatch is not statically resolved';
      item.tooltip = result.disclaimer;
      item.iconPath = new ThemeIcon('references');
      item.contextValue = 'smalltalkXrefHeader';
      return item;
    }
    if (node.type !== 'row') {
      return new TreeItem('');
    }
    const { row } = node;
    const item = new TreeItem(row.label);
    item.description = row.detail; // provenance + receiver-hint badge
    item.tooltip = `${row.label}\n${row.detail}`;
    item.iconPath = new ThemeIcon(
      row.kind === 'implementor' ? 'symbol-method' : row.provenance.startsWith('cartridge') ? 'library' : 'arrow-small-right',
    );
    item.contextValue = `smalltalkXref:${row.kind}`;
    item.command = {
      title: 'Open',
      command: 'smalltalk.openCrossReference',
      arguments: [row.uri, row.range],
    };
    return item;
  }
}

/** Read-only virtual document for a cartridge cross-reference fact. The frozen
 *  cartridge is facts-only (no bodies), so the document states the fact rather
 *  than pretending to show source — honest, and it makes the row openable. */
const cartridgeContentProvider: TextDocumentContentProvider = {
  provideTextDocumentContent(uri: Uri): string {
    // Path shape: /<dialect>/<version>/<ClassId>/<side>/<selector> (encoded segments).
    const [dialect, version, classId, side, selector] = uri.path
      .split('/')
      .filter((s) => s.length > 0)
      .map((s) => decodeURIComponent(s));
    const sideWord = side === 'class' ? ' class' : '';
    return [
      `"${classId ?? '?'}${sideWord} >> ${selector ?? '?'}"`,
      '',
      `Cross-reference fact from the ${dialect ?? '?'} ${version ?? ''} cartridge (facts-only).`,
      'The method source body is not bundled offline, so only the coordinates are shown.',
      'Install the dialect or open its source in your workspace to read the implementation.',
    ].join('\n');
  },
};

async function openLocation(uriString: string, range: XrefResultRange): Promise<void> {
  const uri = Uri.parse(uriString);
  const selection = new Selection(range.start.line, range.start.character, range.end.line, range.end.character);
  if (uri.scheme === CARTRIDGE_SCHEME) {
    const doc = await workspace.openTextDocument(uri);
    await window.showTextDocument(doc, { preview: true });
    return;
  }
  await window.showTextDocument(uri, { selection, preview: true });
}

/** Wire the commands, the tree view, and the virtual-document provider. Returns
 *  nothing; everything is registered on `context.subscriptions`. */
export function registerCrossReferences(context: ExtensionContext, client: LanguageClient): void {
  const tree = new CrossReferenceTree();
  const view = window.createTreeView(VIEW_ID, { treeDataProvider: tree, showCollapseAll: false });

  const run = async (direction: XrefDirection): Promise<CrossReferenceResult | undefined> => {
    const editor = window.activeTextEditor;
    if (!editor || editor.document.languageId !== 'smalltalk') {
      void window.showInformationMessage('Open a Smalltalk file and place the cursor on a selector first.');
      return undefined;
    }
    const pos = editor.selection.active;
    const result = await client.sendRequest<CrossReferenceResult | null>('smalltalk/crossReference', {
      textDocument: { uri: editor.document.uri.toString() },
      position: { line: pos.line, character: pos.character },
      direction,
    });
    if (!result) {
      void window.showInformationMessage('Place the cursor on a Smalltalk selector first.');
      return undefined;
    }
    tree.setResult(result);
    // Reveal the panel view so the result is visible immediately.
    try {
      await commands.executeCommand(`${VIEW_ID}.focus`);
    } catch {
      // View not focusable in this host (e.g. headless test) — the data is set regardless.
    }
    if (result.count === 0) {
      void window.showInformationMessage(
        `No ${result.direction} of #${result.selector} found in your workspace or the cartridge.`,
      );
    }
    void view; // retain the TreeView handle for the session
    return result;
  };

  context.subscriptions.push(
    view,
    workspace.registerTextDocumentContentProvider(CARTRIDGE_SCHEME, cartridgeContentProvider),
    commands.registerCommand('smalltalk.sendersOf', () => run('senders')),
    commands.registerCommand('smalltalk.implementorsOf', () => run('implementors')),
    commands.registerCommand('smalltalk.openCrossReference', (uri: string, range: XrefResultRange) =>
      openLocation(uri, range),
    ),
  );
}
