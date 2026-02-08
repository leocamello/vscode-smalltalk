# Implementation Plan: Documentation & Onboarding

## Phase 1: Structure & Introduction (US-101)
- [x] Define README structure.
- [x] Write Introduction.
- [x] Add Badges.

## Phase 2: Prerequisites & Configuration (US-102, US-105)
- [x] Write Prerequisites section (GNU Smalltalk).
- [x] Write Configuration section (`smalltalk.gnuSmalltalkPath`).

## Phase 3: Features & Quick Start (US-103, US-104)
- [x] Write Features list (Declarative focus).
- [ ] Write Quick Start guide (Pending content).

## Phase 4: Support & Maintenance (US-106)
- [ ] Write Troubleshooting section (Pending content).
- [x] Add License and Support links.

## Phase 5: Completion & Fixes (Gap Resolution)
- [ ] **5.1 Update `package.json` Configuration Definition**
    - [ ] Add `configuration` field to `contributes`.
    - [ ] Define `smalltalk.gnuSmalltalkPath` (type: string, default: "").
    - [ ] Verify setting appears in Settings UI.
- [ ] **5.2 Complete Quick Start Section (US-103)**
    - [ ] Remove TODO placeholders.
    - [ ] Add step: Verify `gst` installation.
    - [ ] Add step: Create `hello.st` file.
    - [ ] Add step: Verify highlighting.
- [ ] **5.3 Complete Troubleshooting Section (US-106)**
    - [ ] Remove TODO placeholders.
    - [ ] Add issue: `gst` not found on PATH.
    - [ ] Add issue: Syntax highlighting not working.

## Phase 5: Completion & Fixes (Gap Resolution)

### 5.1 Update `package.json` Configuration Definition
**Objective:** Ensure the `smalltalk.gnuSmalltalkPath` setting documented in README is properly defined in the manifest.

**Tasks:**
- [ ] Add `configuration` field to `contributes` section in `package.json`
- [ ] Define `smalltalk.gnuSmalltalkPath` setting with:
  - Type: `string`
  - Default: `""` (empty string, uses PATH by default)
  - Description: "Absolute path to the GNU Smalltalk executable (gst). If not set, the extension will attempt to find gst in the system PATH."
- [ ] Verify setting is discoverable in VS Code settings UI
- [ ] Create/update `configuration.json` file if using external schema file

**Details:**
The README documents the `smalltalk.gnuSmalltalkPath` configuration setting, but it is not currently registered in `package.json`. This is a critical gap that prevents:
- The setting from appearing in VS Code's UI
- Documentation from being accurate
- Future features from accessing the configured path

### 5.2 Complete Quick Start Section (US-103)
**Objective:** Fill in the Quick Start section with actionable steps for new users.

**Tasks:**
- [ ] Replace TODO comment with actual Quick Start content
- [ ] Include these steps:
  1. Verify GNU Smalltalk is installed (`gst --version`)
  2. Install the extension from VS Code Marketplace
  3. Create or open a `.st` file in VS Code
  4. Observe syntax highlighting in action
  5. (Optional) Try a basic snippet completion by typing a keyword
  6. (Optional) Test bracket matching and auto-indentation

**Details:**
Quick Start should be concise (5-7 lines max) and actionable. The goal is to get a user to a "working" state within 2 minutes.

**Example Content Structure:**
```markdown
## Quick Start

1. **Verify GNU Smalltalk Installation:**
   - Open a terminal and run `gst --version`
   - Ensure it outputs a version number (e.g., `3.2.5`)

2. **Install the Extension:**
   - Open VS Code
   - Go to Extensions (Ctrl+Shift+X / Cmd+Shift+X)
   - Search for "vscode-smalltalk"
   - Click Install

3. **Create Your First File:**
   - Create a new file named `hello.st`
   - Add this code:
     ```smalltalk
     'Hello, Smalltalk!' printNl.
     ```

4. **Verify Syntax Highlighting:**
   - Notice the code is highlighted with colors
   - Strings appear in green, keywords in blue, etc.

5. **Next Steps:**
   - See the **Features** section for more capabilities
   - Check **Configuration** to customize settings like `smalltalk.gnuSmalltalkPath`
```

### 5.3 Complete Troubleshooting Section (US-106)
**Objective:** Provide solutions to common setup and usage issues.

**Tasks:**
- [ ] Replace TODO comment with actual Troubleshooting content
- [ ] Cover at least these scenarios:
  1. **Syntax highlighting not working**
     - Verify file has `.st` or `.gst` extension
     - Check that the Smalltalk extension is enabled
     - Confirm the language is set to "Smalltalk" in VS Code
  2. **GNU Smalltalk not found**
     - Verify `gst` is in system PATH (`which gst` or `where gst`)
     - Use the `smalltalk.gnuSmalltalkPath` setting if `gst` is not in PATH
     - Provide path example: `/usr/local/bin/gst` (Linux/Mac) or `C:\GnuSmalltalk\bin\gst.exe` (Windows)
  3. **Commands fail or don't appear**
     - Reload VS Code (`Ctrl+R` or `Cmd+R`)
     - Verify the extension is properly installed
     - Check the Extensions panel to ensure it's "Enabled"
  4. **Still having issues?**
     - File a bug report on GitHub Issues with:
       - Your OS and VS Code version
       - GNU Smalltalk version output (`gst --version`)
       - Error message or logs
       - Steps to reproduce

**Details:**
Troubleshooting should be organized by symptom/error, not solution. Each entry should follow the format:
- **Problem Statement**
- Potential causes (2-3)
- Solution steps (3-5 steps)
- Link to GitHub Issues for unsolved problems

**Example Content Structure:**
```markdown
## Troubleshooting

### Syntax Highlighting Not Working
**Problem:** I created a `.st` file but the code is not highlighted with colors.

**Solutions:**
1. **Verify file extension:** Ensure your file has `.st` or `.gst` extension (not `.txt` or no extension).
2. **Verify language association:** 
   - Click the language selector in the bottom right of the editor
   - Ensure "Smalltalk" is selected (not "Plain Text")
3. **Reload VS Code:** Press `Ctrl+R` (Windows/Linux) or `Cmd+R` (Mac).
4. **Reinstall extension:** Uninstall and reinstall the extension from the Marketplace.

### GNU Smalltalk Not Found
**Problem:** Features that require GNU Smalltalk fail, or I see an error about `gst` not being found.

**Solutions:**
1. **Check installation:** Open a terminal and run:
   ```bash
   gst --version
   ```
   If this command works, `gst` is in your PATH.

2. **Add to PATH (if missing):**
   - **Linux/Mac:** Add the installation directory to your `~/.bashrc` or `~/.zshrc`
   - **Windows:** Add the installation directory to your system PATH environment variable

3. **Use the setting (alternative):** If you can't add `gst` to PATH, configure the extension:
   - Open VS Code Settings (`Ctrl+,` / `Cmd+,`)
   - Search for `smalltalk.gnuSmalltalkPath`
   - Enter the full path to `gst` executable, e.g.:
     - Linux/Mac: `/usr/local/bin/gst`
     - Windows: `C:\Program Files\GnuSmalltalk\bin\gst.exe`

### Extension Commands Don't Appear
**Problem:** I don't see expected commands in the Command Palette or they fail when I run them.

**Solutions:**
1. **Reload VS Code:** Press `Ctrl+R` (Windows/Linux) or `Cmd+R` (Mac).
2. **Check extension status:**
   - Open the Extensions panel (`Ctrl+Shift+X` / `Cmd+Shift+X`)
   - Find "Smalltalk Language Support"
   - Ensure it shows "Enabled" (not "Disabled")
3. **Reinstall if necessary:** Click the settings gear > Uninstall, then Install again.

### Still Having Issues?
If none of the above solutions work, please report the issue:
1. Go to [GitHub Issues](https://github.com/leocamello/vscode-smalltalk/issues)
2. Click "New Issue"
3. Include:
   - Your operating system and VS Code version
   - Output of `gst --version`
   - The error message or description of unexpected behavior
   - Steps to reproduce the issue

We appreciate your feedback and will help you resolve the problem!
```

---

## Completion Criteria

All phases are complete when:
1. ✅ README.md has no TODO comments
2. ✅ All sections (Introduction, Prerequisites, Installation, Quick Start, Features, Configuration, Troubleshooting, Contributing, Support, License) are filled with substantive content
3. ✅ `package.json` includes the `configuration` section defining `smalltalk.gnuSmalltalkPath`
4. ✅ Configuration setting is accessible in VS Code settings UI
5. ✅ Documentation is reviewed and verified by a team member
6. ✅ All user stories (US-101 through US-106) acceptance criteria are met
