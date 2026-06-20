// @vscode/test-cli config — end-to-end integration tests that launch VS Code
// with this extension and a fixture workspace, then drive the LSP providers via
// the `vscode.execute*Provider` commands (US-412 DoD).
//
// Run with: npm run test:e2e  (requires a display; on headless Linux use xvfb).
import { defineConfig } from '@vscode/test-cli';

export default defineConfig({
  label: 'e2e',
  files: 'client/test-e2e/**/*.test.js',
  workspaceFolder: 'client/test-e2e/fixtures',
  mocha: { timeout: 60000, ui: 'tdd' },
});
