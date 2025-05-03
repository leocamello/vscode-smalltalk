# Smalltalk Language Support for Visual Studio Code

<!-- Badges: VS Code Marketplace Version, Installs, Rating, GitHub Actions CI -->
[![Build Status](https://img.shields.io/github/actions/workflow/status/leocamello/vscode-smalltalk/main.yml?branch=master&style=flat-square&logo=github)](https://github.com/leocamello/vscode-smalltalk/actions?query=workflow:main)
[![Visual Studio Marketplace Version](https://img.shields.io/visual-studio-marketplace/v/leocamello.vscode-smalltalk?style=flat-square&label=Marketplace)](https://marketplace.visualstudio.com/items?itemName=leocamello.vscode-smalltalk)
[![Visual Studio Marketplace Rating](https://img.shields.io/visual-studio-marketplace/r/leocamello.vscode-smalltalk?style=flat-square)](https://marketplace.visualstudio.com/items?itemName=leocamello.vscode-smalltalk)
[![Visual Studio Marketplace Installs](https://img.shields.io/visual-studio-marketplace/i/leocamello.vscode-smalltalk?style=flat-square)](https://marketplace.visualstudio.com/items?itemName=leocamello.vscode-smalltalk)

<!-- Introduction (US-101 AC2) -->
## Introduction

Provides comprehensive language support for **Smalltalk**, with an initial focus on **GNU Smalltalk** and a file-based (`.st`) development workflow within Visual Studio Code. Our goal is to offer a seamless and productive environment for Smalltalk developers using VS Code.

<!-- Prerequisites (US-102) -->
## Prerequisites

This extension requires **GNU Smalltalk** to be installed on your system to provide its full functionality, especially for features planned in future phases (like code execution or language server integration).

1.  **Install GNU Smalltalk:**
    *   You need a working installation of GNU Smalltalk. We recommend using the **latest stable version**.
    *   Download and find installation instructions for your operating system on the official **[GNU Smalltalk Website](https://www.gnu.org/software/smalltalk/)**.
    *   Ensure the GNU Smalltalk executable (`gst`) is available in your system's `PATH`.

2.  **Verify Installation (Optional but Recommended):**
    *   Open your terminal or command prompt and run `gst --version`. This should output the installed version number.

**Note:** While the extension primarily focuses on GNU Smalltalk, basic syntax highlighting might work for other Smalltalk dialects using `.st` files, but compatibility is not guaranteed. Future features requiring execution or analysis will depend specifically on `gst`.

*(Self-correction: We will add details about the `smalltalk.gnuSmalltalkPath` setting in the Configuration section under US-105, but it's good to ensure `gst` is findable via PATH as the primary method).*

<!-- Installation -->
## Installation

Install the **Smalltalk Language Support** extension from the [Visual Studio Code Marketplace](https://marketplace.visualstudio.com/items?itemName=leocamello.vscode-smalltalk) or by searching within VS Code:

1.  Open **Extensions** view (`Ctrl+Shift+X` or `Cmd+Shift+X`).
2.  Search for `leocamello.vscode-smalltalk`.
3.  Click **Install**.

<!-- Quick Start (US-103) -->
## Quick Start

<!-- TODO: Content for US-103 -->
<!-- Simple steps: -->
<!-- 1. Ensure Prerequisites are met. -->
<!-- 2. Install the extension. -->
<!-- 3. Open a folder containing .st files or create a new .st file. -->
<!-- 4. Observe syntax highlighting. -->
<!-- 5. (Optional) Show a basic snippet usage. -->
<!-- 6. (Optional, depends on US-301) Show how to run a simple file. -->

<!-- Features (US-104) -->
## Features

<!-- TODO: Update content for US-104 -->
<!-- Keep the structure but ensure accuracy based on current implementation -->
<!-- Add screenshots/GIFs later if helpful -->

### Foundational Editing Features (Declarative)

*   **Syntax Highlighting:** Accurate highlighting for GNU Smalltalk syntax in `.st` files.
*   **Snippet Completion:** Useful snippets for common Smalltalk constructs.
*   **Bracket Matching & Autoclosing:** Helps with code structure.
*   **Comment Toggling:** Easily comment/uncomment lines or blocks (`Ctrl+/` or `Cmd+/`).
*   **Auto Indentation:** Basic indentation support.
*   **Folding:** Code folding based on markers (e.g., comments like `"{--"` and `"--}"`).

### Language Intelligence Features (LSP - Planned for Phase 2)

*   _(Coming Soon)_ Hover Information
*   _(Coming Soon)_ Auto Completion
*   _(Coming Soon)_ Jump to Definition
*   _(Coming Soon)_ Diagnostics (Error Checking)
*   _(Coming Soon)_ Formatting
*   _(Coming Soon)_ Refactoring

<!-- Configuration (US-105) -->
## Configuration

You can configure the Smalltalk extension settings through the standard Visual Studio Code settings interface (UI or `settings.json`). See the [VS Code User Settings documentation](https://code.visualstudio.com/docs/getstarted/settings) for more details on how to modify settings.

The following settings are available:

*   **`smalltalk.gnuSmalltalkPath`**
    *   **Description:** Specifies the absolute path to the GNU Smalltalk executable (`gst`). This setting is used by features that need to invoke GNU Smalltalk (like running files or future LSP integration) if the executable cannot be found in the system's `PATH` environment variable.
    *   **Type:** `string`
    *   **Default:** `""` (The extension will first attempt to find `gst` in the system `PATH`).

<!-- Commands (US-301) -->
## Commands

<!-- TODO: Content for US-301 -->
<!-- List commands added to the Command Palette, e.g.: -->
<!-- - `Smalltalk: Run Current File`: Executes the active `.st` file using the configured `gst` interpreter. -->

<!-- Troubleshooting (US-106) -->
## Troubleshooting

<!-- TODO: Content for US-106 -->
<!-- Common issues and solutions, e.g.: -->
<!-- - Syntax highlighting not working? (Check file association) -->
<!-- - 'Run File' command fails? (Check `smalltalk.gnuSmalltalkPath` setting) -->
<!-- - Link to GitHub Issues for reporting problems. -->

<!-- Contributing -->
## Contributing

Contributions are welcome! Please see our [Contribution Guide](CONTRIBUTING.md) (TODO: Create this file) for details on how to submit pull requests, report issues, and suggest features.

<!-- Contact Us / Support -->
## Support & Feedback

If you encounter any issues or have suggestions, please file them on the [GitHub Issues page](https://github.com/leocamello/vscode-smalltalk/issues).

<!-- License -->
## License

This extension is licensed under the [MIT License](LICENSE). <!-- Ensure LICENSE file exists -->
