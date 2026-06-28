# Smalltalk Language Support for Visual Studio Code

<!-- Badges: GitHub Actions CI, VS Code Marketplace Version / Installs / Rating.
     shields.io retired its visual-studio-marketplace badges, so the Marketplace
     ones use vsmarketplacebadges.dev. -->
[![Build Status](https://img.shields.io/github/actions/workflow/status/leocamello/vscode-smalltalk/main.yml?branch=master&style=flat-square&logo=github)](https://github.com/leocamello/vscode-smalltalk/actions?query=workflow:main)
[![Visual Studio Marketplace Version](https://vsmarketplacebadges.dev/version-short/leocamello.vscode-smalltalk.svg)](https://marketplace.visualstudio.com/items?itemName=leocamello.vscode-smalltalk)
[![Visual Studio Marketplace Installs](https://vsmarketplacebadges.dev/installs-short/leocamello.vscode-smalltalk.svg)](https://marketplace.visualstudio.com/items?itemName=leocamello.vscode-smalltalk)
[![Visual Studio Marketplace Rating](https://vsmarketplacebadges.dev/rating-short/leocamello.vscode-smalltalk.svg)](https://marketplace.visualstudio.com/items?itemName=leocamello.vscode-smalltalk)

<!-- Introduction (US-101 AC2) -->
## Introduction

Provides comprehensive language support for **Smalltalk**, with an initial focus on **GNU Smalltalk** and a file-based (`.st`) development workflow within Visual Studio Code. Our goal is to offer a seamless and productive environment for Smalltalk developers using VS Code.

<!-- Prerequisites (US-102) -->
## Prerequisites

**GNU Smalltalk (`gst`) is optional.** Editing and language intelligence — syntax highlighting, snippets, the outline, workspace symbol search, and go-to-definition — all work **without** it. You only need `gst` installed and on your `PATH` (or set via `smalltalk.gnuSmalltalkPath`) for the **Run Current File** command, which executes your code.

1.  **Install GNU Smalltalk:**
    *   You need a working installation of GNU Smalltalk. We recommend using the **latest stable version**.
    *   Download and find installation instructions for your operating system on the official **[GNU Smalltalk Website](https://www.gnu.org/software/smalltalk/)**.
    *   Ensure the GNU Smalltalk executable (`gst`) is available in your system's `PATH`.

2.  **Verify Installation (Optional but Recommended):**
    *   Open your terminal or command prompt and run `gst --version`. This should output the installed version number.

**Note:** While the extension primarily focuses on GNU Smalltalk, basic syntax highlighting might work for other Smalltalk dialects using `.st` files, but compatibility is not guaranteed. Future features requiring execution or analysis will depend specifically on `gst`.

<!-- Installation -->
## Installation

Install the **Smalltalk Language Support** extension from the [Visual Studio Code Marketplace](https://marketplace.visualstudio.com/items?itemName=leocamello.vscode-smalltalk) or by searching within VS Code:

1.  Open **Extensions** view (`Ctrl+Shift+X` or `Cmd+Shift+X`).
2.  Search for `leocamello.vscode-smalltalk`.
3.  Click **Install**.

<!-- Quick Start (US-103) -->
## Quick Start

1. **Ensure Prerequisites:** Verify that GNU Smalltalk is installed by running `gst --version` in your terminal. If not installed, follow the instructions in the [Prerequisites](#prerequisites) section. If `gst` is installed but not found, see [Configuration](#configuration) to set the path.

2. **Install the Extension:** Install the Smalltalk Language Support extension from the Visual Studio Code Marketplace (see [Installation](#installation)).

3. **Create a Smalltalk File:** Create a new file named `hello.st` in your project folder.

4. **Write Code:** Add the following Smalltalk code to the file:
   ```smalltalk
   Transcript show: 'Hello, Smalltalk!'; cr.
   ```

5. **Run the File:** Open your terminal in the project folder and execute:
   ```bash
   gst hello.st
   ```
   You should see the output: `Hello, Smalltalk!`

6. **Enjoy Syntax Highlighting:** As you write more Smalltalk code, you'll benefit from accurate syntax highlighting, snippets, and other editing features provided by the extension.

<!-- Features (US-104) -->
## Features

<!-- TODO: Update content for US-104 -->
<!-- Keep the structure but ensure accuracy based on current implementation -->
<!-- Add screenshots/GIFs later if helpful -->

### Foundational Editing Features (Declarative)

*   **Syntax Highlighting:** Accurate highlighting for GNU Smalltalk syntax in `.st` files.
*   **Snippet Completion:** Tab-completable templates for common Smalltalk constructs, including idiomatic **block-bearing** idioms with tab-stops — loops (`whileTrue:`, `to:do:`), exception handling (`on:do:`, `ensure:`), nil-guards (`ifNil:ifNotNil:`), collection iteration (`keysAndValuesDo:`, `inject:into:`, `detect:ifNone:`), and class/method scaffolds.
*   **Bracket Matching & Autoclosing:** Helps with code structure.
*   **Comment Toggling:** Easily comment/uncomment lines or blocks (`Ctrl+/` or `Cmd+/`).
*   **Auto Indentation:** Basic indentation support.
*   **Folding:** Code folding based on markers (e.g., comments like `"{--"` and `"--}"`).

### Language Intelligence Features (LSP)

Powered by a bundled language engine — **no GNU Smalltalk (`gst`) installation required** for these:

*   **Outline & Breadcrumbs:** the document structure (classes → methods, with instance/class variables) for both brace- and chunk-format files.
*   **Workspace Symbol Search:** `Ctrl/Cmd+T` to find classes and method selectors across the workspace.
*   **Go to Definition:** `F12` / `Ctrl/Cmd+Click` — from a class reference to its definition, or from a message send to every implementor.
*   **Code Folding:** collapse class/method/block bodies and multi-line comments.
*   **Highlight Occurrences:** put the cursor on a selector or variable to highlight its other uses in the file (scope-aware).
*   **Auto Completion:** selectors after a receiver, class names, and in-scope variables — including **standard kernel-library** classes/selectors. Multi-part keyword selectors insert as snippets (`at:put:` → `at:⟨1⟩ put:⟨2⟩`). Kernel completions come from your **installed** GNU Smalltalk when found, otherwise a **bundled** reference (GNU Smalltalk 3.2.5); the active source is shown in the status bar. Configurable via [`smalltalk.completion.kernelLibrary`](#configuration).
*   **Diagnostics (Error Checking):** syntax errors are flagged with squiggles **as you type** — no `gst` needed — badged `smalltalk(parse)`, with severity as the parser reports it; they clear as you fix the code. Quick fixes insert a missing closer (`]`, `)`, `}`, `>`) or close an unterminated string.
*   **Hover Information:** hover a selector (signature + implementors), a class (superclass chain), a variable (kind + declaration site), or a numeric literal (e.g. `16rFF` → `255`). Class/method comments are shown from your own workspace source and your installed kernel; the bundled reference stays facts-only.
*   **Semantic Highlighting:** role-accurate coloring — instance/class variables, temporaries, parameters, selectors, and pseudo-variables are each colored by meaning, and a capitalized name is colored as a **class** only when it's a real class in your workspace or the kernel (otherwise a global). Enable `editor.semanticHighlighting.enabled` if your theme defaults it off.
*   **References · Senders · Implementors:** the System Browser's cross-reference muscle memory, **offline** — **Find All References** (`Shift+F12`), **plural Go to Definition**, **Call Hierarchy**, and the **`Smalltalk: Senders of…` / `Implementors of…`** commands (Command Palette + right-click) feed a **Smalltalk References** panel. Results are an **honest union** of your workspace and the GNU Smalltalk kernel — each row tagged with its source (`workspace` / `installed (gst)` / `reference (gst 3.2.5)`) — because dynamic dispatch can't be resolved statically: likely responders rank first, none are hidden. Kernel rows from your installed GNU Smalltalk open the real source file; the bundled reference opens a read-only fact card.
*   **Signature Help:** typing a keyword message (`aDictionary at: key put: …`) pops the matching keyword selector(s) with the **active parameter** tracked — the keyword you're currently filling. Signatures are an honest prefix union of your workspace and the kernel (each tagged with its source); keyword-only by design.
*   **Formatting:** conservative, **idempotent** code formatting — **Format Document**, **Format Selection**, and on-type — that normalizes layout (indentation, spacing, blank-line runs, cascade alignment, long-keyword-message wrapping) **without ever changing your code**: it only rewrites whitespace between tokens, so comments and blank lines are preserved and the result is stable (formatting twice changes nothing). A file with a syntax error is left untouched. **Off by default** — enable [`smalltalk.format.enable`](#configuration); an optional [`smalltalk.format.blockStyle`](#configuration) `expand` reflows bodies one-statement-per-line.

<!-- Configuration (US-105) -->
## Configuration

You can configure the Smalltalk extension settings through the standard Visual Studio Code settings interface (UI or `settings.json`). See the [VS Code User Settings documentation](https://code.visualstudio.com/docs/getstarted/settings) for more details on how to modify settings.

The following settings are available:

*   **`smalltalk.gnuSmalltalkPath`**
    *   **Description:** Specifies the absolute path to the GNU Smalltalk executable (`gst`). This setting is used by features that need to invoke GNU Smalltalk (like running files or future LSP integration) if the executable cannot be found in the system's `PATH` environment variable.
    *   **Type:** `string`
    *   **Default:** `""` (The extension will first attempt to find `gst` in the system `PATH`).

*   **`smalltalk.completion.kernelLibrary`**
    *   **Description:** Source for standard kernel-library completions (classes/selectors). `auto` prefers your **installed** GNU Smalltalk kernel and falls back to the **bundled** reference (GNU Smalltalk 3.2.5); `bundled` always uses the bundled reference; `off` disables kernel completions (workspace symbols remain). The active source is shown in the status bar.
    *   **Type:** `string` — `auto` | `bundled` | `off`
    *   **Default:** `auto`

*   **`smalltalk.completion.kernelPath`**
    *   **Description:** Optional path to a GNU Smalltalk **kernel source directory** (the folder of `.st` files, e.g. `…/share/smalltalk/kernel`) used by `auto`. If empty, the kernel directory is discovered from `smalltalk.gnuSmalltalkPath` and common install locations.

*   **`smalltalk.format.enable`**
    *   **Description:** Enable conservative, idempotent Smalltalk formatting (document, range, and on-type). Off by default — formatting only rewrites whitespace and never changes your code, but you opt in.
    *   **Type:** `boolean`
    *   **Default:** `false`

*   **`smalltalk.format.indentSize`**
    *   **Description:** Spaces per indent level when formatting. Tabs vs. spaces follow the editor's `editor.insertSpaces`.
    *   **Type:** `number`
    *   **Default:** `4`

*   **`smalltalk.format.cascades`**
    *   **Description:** How to lay out message cascades (`receiver msg1; msg2; …`). `align` breaks each segment onto its own line under the receiver; `preserve` keeps the author's line breaks.
    *   **Type:** `string` — `align` | `preserve`
    *   **Default:** `align`

*   **`smalltalk.format.keywordWrap`**
    *   **Description:** Wrap a keyword message one-keyword-per-line when its single-line width would exceed this column. `0` disables wrapping.
    *   **Type:** `number`
    *   **Default:** `100`

*   **`smalltalk.format.blockStyle`**
    *   **Description:** How to lay out block, method, and class bodies. `preserve` keeps your line breaks; `expand` reflows each body onto its own indented lines, one statement per line (single-statement argument blocks stay inline).
    *   **Type:** `string` — `preserve` | `expand`
    *   **Default:** `preserve`
    *   **Type:** `string`
    *   **Default:** `""`

<!-- Commands (US-301) -->
## Commands

Available from the Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`) and the editor
right-click menu when a Smalltalk file is active:

| Command | Description |
| --- | --- |
| **Smalltalk: Run Current File** | Saves and runs the active `.st`/`.gst` file with your configured GNU Smalltalk interpreter in the integrated terminal. Uses [`smalltalk.gnuSmalltalkPath`](#configuration) if set, otherwise looks for `gst` on your `PATH`; if neither is found, it offers to open the setting. |

<!-- Troubleshooting (US-106) -->
## Troubleshooting

### Issue: 'gst' not found

**Problem:** You receive an error message like `gst: command not found` when trying to run a Smalltalk file.

**Solution:**
- Ensure GNU Smalltalk is properly installed on your system. Refer to the [Prerequisites](#prerequisites) section for installation instructions.
- Verify that the `gst` executable is available in your system's `PATH` environment variable.
- If GNU Smalltalk is installed but not in your `PATH`, configure the `smalltalk.gnuSmalltalkPath` setting to point to the absolute path of the `gst` executable (e.g., `/usr/local/bin/gst` on macOS/Linux or `C:\Program Files\GnuSmalltalk\gst.exe` on Windows).

### Issue: Syntax Highlighting not working

**Problem:** Syntax highlighting is not displayed in your `.st` files.

**Solution:**
- Ensure the file has the correct `.st` file extension.
- Verify that the "Smalltalk" language mode is selected. You can check this in the bottom-right corner of VS Code and switch to "Smalltalk" if needed.
- Try reloading the window (`Ctrl+R` or `Cmd+R`) to refresh the extension.

### Need More Help?

If you encounter issues not covered here, please report them on the [GitHub Issues page](https://github.com/leocamello/vscode-smalltalk/issues).

### Logs
Detailed logging features are currently planned for future releases. In the meantime, please include any error messages you see in VS Code notifications when reporting bugs.

### Known Issues
*   No common issues identified yet. Please search the [GitHub Issues](https://github.com/leocamello/vscode-smalltalk/issues) page for existing reports.

<!-- Contributing -->
## Contributing

Contributions are welcome! We follow a spec-driven process — see the [Contribution Guide](CONTRIBUTING.md) for how we work (specs, issues, conventions) and how to submit pull requests, report issues, and suggest features. The [Roadmap](docs/ROADMAP.md) shows where the project is headed.

<!-- Contact Us / Support -->
## Support & Feedback

If you encounter any issues or have suggestions, please file them on the [GitHub Issues page](https://github.com/leocamello/vscode-smalltalk/issues).

<!-- License -->
## License

This extension is licensed under the [MIT License](LICENSE). <!-- Ensure LICENSE file exists -->
