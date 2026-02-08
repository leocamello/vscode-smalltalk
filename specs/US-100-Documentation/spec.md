# Specification: Documentation & Onboarding

**ID**: US-100 (Consolidates US-101, US-102, US-103, US-104, US-105, US-106)
**Feature**: Documentation & Onboarding (README)
**Status**: Implemented (with some pending content)
**Owner**: Leonardo Nascimento

## 1. Overview
A comprehensive README is essential for user adoption. It guides new users on how to install, configure, and use the extension, specifically targeting GNU Smalltalk users.

## 2. Goals
*   Provide a clear Introduction and Value Proposition.
*   List Prerequisites (GNU Smalltalk).
*   Explain Installation and Quick Start steps.
*   Document Features and Configuration.
*   Provide Troubleshooting and Contribution guidelines.

## 3. Non-Goals
*   Detailed language tutorial (Smalltalk itself).
*   Documentation for future features not yet implemented (LSP/DAP details beyond "Planned").

## 4. User Stories

**US-101**: As a potential user, I want a well-structured README so that I can understand the extension's purpose.
*   **AC1**: Standard sections (Intro, Features, Config, etc.) present.
*   **AC2**: Introduction clearly states GNU Smalltalk target.

**US-102**: As a new user, I want a "Prerequisites" section so I know what to install.
*   **AC1**: Lists "GNU Smalltalk" as a requirement.
*   **AC2**: Provides a link to the official GNU Smalltalk website.
*   **AC3**: Mentions the need for `gst` to be accessible (PATH or setting).
*   **AC4**: Specifies that the "latest stable version" is recommended.
*   **AC5**: Section is easy to find.

**US-103**: As a new user, I want a "Quick Start" guide so I can get running fast.
*   **AC1**: Steps include installing the extension.
*   **AC2**: Steps include opening a folder/workspace.
*   **AC3**: Steps include creating a simple `.st` file.
*   **AC4**: Steps demonstrate basic syntax highlighting.
*   **AC5**: Guide explains how to ensure 'gst' is accessible (PATH or `smalltalk.gnuSmalltalkPath`).
*   **AC6**: Guide is concise.
*   **AC7**: Guide uses clear, numbered steps.

**US-104**: As a user, I want a "Features" list to know what the extension does.
*   **AC1**: Lists current declarative features (Syntax, Snippets).
*   **AC2**: Marks LSP/DAP as "Planned".

**US-105**: As a user, I want a "Configuration" section to customize behavior.
*   **AC1**: Documents `smalltalk.gnuSmalltalkPath`.
*   **AC2**: Explains how to change settings.

**US-106**: As a user, I want "Troubleshooting" to solve common issues.
*   **AC1**: Troubleshooting section exists.
*   **AC2**: Guidance on basic setup (GST installed, PATH configured).
*   **AC3**: Explains where to find logs (or states planned).
*   **AC4**: Instructions to report issues.
*   **AC5**: "Known Issues" subsection exists.

## 5. Technical Design
*   **File**: `README.md` at project root.
*   **Format**: Standard Markdown.
*   **Badges**: CI/CD status, Marketplace version/installs.

## 6. Risks & Dependencies
*   **Risk**: Documentation becoming stale. (Mitigation: Update with every feature release).
*   **Dependency**: US-105 depends on the actual setting being defined in `package.json`.
