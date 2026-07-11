#!/usr/bin/env bash
#
# make-gif.sh — convert a screen recording into an optimized, README-ready GIF.
#
# Turns a .webm/.mp4/.mov capture into media/demo-<slug>.gif using a two-pass
# ffmpeg palette (palettegen + paletteuse) for clean colors at a small size.
# Used to produce the five demo GIFs the README references (US-902); the WHAT
# to capture is in docs/media-shotlist.md.
#
# Usage:
#   scripts/make-gif.sh <input-video> <slug> [options]
#
#   <slug>  one of: highlighting outline completion diagnostics run
#           (writes media/demo-<slug>.gif)
#
# Options:
#   --width N     output width in px (default 900; height auto, keeps aspect)
#   --fps N       frames per second (default 15; lower = smaller file)
#   --start T     trim: start at T (e.g. 3 or 00:00:03)
#   --dur T       trim: capture only T seconds from --start (e.g. 6)
#   --speed N     play faster (e.g. 1.6 = 1.6x; effective length = dur/N; default 1)
#   --crop WxH+X+Y  crop the source before scaling (e.g. 1280x720+320+180)
#   -h, --help    this help
#
# Examples:
#   scripts/make-gif.sh ~/Videos/rec.webm highlighting --dur 5
#   scripts/make-gif.sh ~/Videos/rec.webm run --start 2 --dur 6 --width 860
#
# Tip: record the whole screen/window (GNOME recorder or OBS), then use --crop
# to frame the editor and --start/--dur to trim — no need to record precisely.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
MEDIA_DIR="$REPO_ROOT/media"

WIDTH=900
FPS=15
START=""
DUR=""
SPEED=1
CROP=""
ARGS=()

while [ $# -gt 0 ]; do
  case "$1" in
    --width) WIDTH="$2"; shift 2 ;;
    --fps)   FPS="$2";   shift 2 ;;
    --start) START="$2"; shift 2 ;;
    --dur)   DUR="$2";   shift 2 ;;
    --speed) SPEED="$2"; shift 2 ;;
    --crop)  CROP="$2";  shift 2 ;;
    -h|--help) sed -n '3,34p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
    -*) echo "Unknown option: $1" >&2; exit 1 ;;
    *)  ARGS+=("$1"); shift ;;
  esac
done

if [ "${#ARGS[@]}" -lt 2 ]; then
  echo "Error: need <input-video> and <slug>. See --help." >&2
  exit 1
fi

INPUT="${ARGS[0]}"
SLUG="${ARGS[1]}"

if [ ! -f "$INPUT" ]; then
  echo "Error: input video not found: $INPUT" >&2
  exit 1
fi

case "$SLUG" in
  highlighting|outline|completion|diagnostics|run) ;;
  *) echo "Warning: '$SLUG' is not one of the five README slugs (highlighting/outline/completion/diagnostics/run)." >&2 ;;
esac

command -v ffmpeg >/dev/null 2>&1 || { echo "Error: ffmpeg not found on PATH." >&2; exit 1; }

mkdir -p "$MEDIA_DIR"
OUT="$MEDIA_DIR/demo-${SLUG}.gif"
PALETTE="$(mktemp --suffix=.png)"
trap 'rm -f "$PALETTE"' EXIT

# Build the trim + filter chain.
TRIM=()
[ -n "$START" ] && TRIM+=(-ss "$START")
[ -n "$DUR" ]   && TRIM+=(-t "$DUR")

VF_PRE=""
[ -n "$CROP" ] && VF_PRE="crop=${CROP},"
# Speed-up (setpts=PTS/N) is applied first, before fps sampling, so a 1.6x pass
# on a 20s trim yields a ~12.5s GIF. `awk` guards against a non-1 default.
SPEED_F=""
if awk "BEGIN{exit !($SPEED != 1)}"; then SPEED_F="setpts=PTS/${SPEED},"; fi
FILTERS="${VF_PRE}${SPEED_F}fps=${FPS},scale=${WIDTH}:-1:flags=lanczos"

echo "→ Pass 1/2: generating palette…"
ffmpeg -hide_banner -loglevel error -y "${TRIM[@]}" -i "$INPUT" \
  -vf "${FILTERS},palettegen=stats_mode=diff" "$PALETTE"

echo "→ Pass 2/2: encoding GIF…"
ffmpeg -hide_banner -loglevel error -y "${TRIM[@]}" -i "$INPUT" -i "$PALETTE" \
  -lavfi "${FILTERS} [x]; [x][1:v] paletteuse=dither=bayer:bayer_scale=3:diff_mode=rectangle" \
  "$OUT"

SIZE="$(du -h "$OUT" | cut -f1)"
echo "✓ Wrote $OUT ($SIZE, ${WIDTH}px, ${FPS} fps)"
[ "$(du -k "$OUT" | cut -f1)" -gt 3072 ] && \
  echo "  Note: >3 MB — consider --fps 12 or --width 800 to shrink it." || true
