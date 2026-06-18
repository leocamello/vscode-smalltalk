#!/usr/bin/env bash
#
# new-story.sh — scaffold a canonical spec package for a user story.
#
# Usage:
#   bash scripts/new-story.sh <US-ID> <Title...> [--branch]
#   npm run new-story -- <US-ID> <Title...> [--branch]
#
# Examples:
#   npm run new-story -- US-410 "LSP scaffold"
#   npm run new-story -- 410 "LSP scaffold" --branch
#
# Creates specs/US-XXX-Title-Case/ with the five-file package
# (spec.md, plan.md, tasks.md, requirements-validation.md, verification.md)
# from .specify/templates/, substituting {{US_ID}}, {{TITLE}}, {{DATE}}, {{BRANCH}}.
# With --branch it also creates and checks out feature/US-XXX-slug.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
TPL="$REPO_ROOT/.specify/templates"

MAKE_BRANCH=false
ARGS=()
for arg in "$@"; do
  case "$arg" in
    --branch) MAKE_BRANCH=true ;;
    --help|-h)
      sed -n '3,18p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
      exit 0 ;;
    *) ARGS+=("$arg") ;;
  esac
done

if [ "${#ARGS[@]}" -lt 2 ]; then
  echo "Error: need a US id and a title. Example: npm run new-story -- US-410 \"LSP scaffold\"" >&2
  exit 1
fi

# --- Normalize the US id: accept "410", "us-410", "US-410" -> "US-410" ---
RAW_ID="${ARGS[0]}"
NUM="$(printf '%s' "$RAW_ID" | grep -oE '[0-9]+' | head -1 || true)"
if [ -z "$NUM" ]; then
  echo "Error: could not find a number in US id '$RAW_ID'." >&2
  exit 1
fi
US_ID="US-${NUM}"

# --- Title is everything after the id ---
TITLE="${ARGS[*]:1}"

# --- Title-Case-Hyphenated dir suffix (preserve acronyms typed in caps) ---
TITLE_SLUG="$(printf '%s' "$TITLE" \
  | sed 's/[^A-Za-z0-9 ]/ /g' \
  | awk '{ for (i=1;i<=NF;i++) { $i=toupper(substr($i,1,1)) substr($i,2) } printf "%s", $0 }' \
  | tr ' ' '-' | sed 's/-\{2,\}/-/g; s/^-//; s/-$//')"

# --- lowercase kebab for the branch ---
BRANCH_SLUG="$(printf '%s' "$TITLE" | tr '[:upper:]' '[:lower:]' \
  | sed 's/[^a-z0-9]/-/g; s/-\{2,\}/-/g; s/^-//; s/-$//')"

DIR_NAME="${US_ID}-${TITLE_SLUG}"
DEST="$REPO_ROOT/specs/$DIR_NAME"
BRANCH="feature/${US_ID}-${BRANCH_SLUG}"
DATE="$(date +%Y-%m-%d)"

if [ -e "$DEST" ]; then
  echo "Error: $DEST already exists — refusing to overwrite." >&2
  exit 1
fi

# --- Render a template file with token substitution ---
render() {
  local src="$1" dest="$2"
  sed -e "s|{{US_ID}}|${US_ID}|g" \
      -e "s|{{TITLE}}|${TITLE}|g" \
      -e "s|{{DATE}}|${DATE}|g" \
      -e "s|{{BRANCH}}|${BRANCH}|g" \
      "$src" > "$dest"
}

mkdir -p "$DEST"
render "$TPL/story/spec.md"                  "$DEST/spec.md"
render "$TPL/story/plan.md"                  "$DEST/plan.md"
render "$TPL/story/tasks.md"                 "$DEST/tasks.md"
render "$TPL/requirements-validation.md"     "$DEST/requirements-validation.md"
render "$TPL/implementation-verification.md" "$DEST/verification.md"

echo "Created spec package: specs/$DIR_NAME/"
ls -1 "$DEST" | sed 's/^/  - /'

if $MAKE_BRANCH; then
  if git -C "$REPO_ROOT" rev-parse --git-dir >/dev/null 2>&1; then
    git -C "$REPO_ROOT" checkout -b "$BRANCH"
    echo "Checked out branch: $BRANCH"
  else
    echo "Warning: not a git repo; skipped branch creation ($BRANCH)." >&2
  fi
else
  echo "Next: git checkout -b $BRANCH   (or re-run with --branch)"
fi

echo "Then: Clarify -> fill spec.md -> requirements-validation.md gate -> plan.md -> tasks.md -> implement -> verify."
