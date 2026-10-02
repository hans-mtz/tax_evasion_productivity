#!/usr/bin/env bash
# Local speech-to-text with whisper.cpp. Nothing is uploaded; only the model is downloaded once.
#
#   transcribe.sh check                       report what is installed, exit 0 if ready, 2 if not
#   transcribe.sh install                     build whisper.cpp from source and download the model
#   transcribe.sh run <audio> [outdir] [prompt]
#                                             convert to 16 kHz mono WAV, transcribe, write <name>.txt and <name>.srt
#
# Environment: WHISPER_HOME (default ~/.local/share/whisper.cpp), WHISPER_MODEL (default medium.en),
#              WHISPER_THREADS (default: physical cores).
set -euo pipefail

ROOT="${WHISPER_HOME:-$HOME/.local/share/whisper.cpp}"
MODEL="${WHISPER_MODEL:-medium.en}"
BIN="$ROOT/build/bin/whisper-cli"
MODEL_FILE="$ROOT/models/ggml-$MODEL.bin"
ARCH="$(uname -m)"
OS="$(uname -s)"

threads() {
  if [ -n "${WHISPER_THREADS:-}" ]; then echo "$WHISPER_THREADS"
  elif [ "$OS" = Darwin ]; then sysctl -n hw.physicalcpu
  else nproc; fi
}

# GPU only on Apple Silicon. On Intel Macs the Metal backend crashed on an AMD GPU (GGML_ASSERT in
# ggml_metal_buffer_get_tensor, 2026-09-30), so run on the CPU there.
gpu_flag() { if [ "$OS" = Darwin ] && [ "$ARCH" = arm64 ]; then echo ""; else echo "-ng"; fi; }

converter() {
  if command -v afconvert >/dev/null 2>&1; then echo afconvert
  elif command -v ffmpeg >/dev/null 2>&1; then echo ffmpeg
  else echo none; fi
}

check() {
  local ok=0
  echo "platform:   $OS $ARCH"
  if [ -x "$BIN" ]; then echo "whisper-cli: OK ($BIN)"; else echo "whisper-cli: MISSING ($BIN)"; ok=2; fi
  if [ -f "$MODEL_FILE" ]; then echo "model:       OK ($MODEL_FILE)"; else echo "model:       MISSING ($MODEL_FILE)"; ok=2; fi
  echo "converter:  $(converter)"; [ "$(converter)" = none ] && ok=2
  for t in git cmake; do
    if command -v "$t" >/dev/null 2>&1; then echo "$t:         present"; else echo "$t:         MISSING (needed only to install)"; fi
  done
  if [ "$OS" = Darwin ] && ! xcode-select -p >/dev/null 2>&1; then echo "Xcode command line tools: MISSING (run xcode-select --install)"; fi
  [ "$ok" -eq 0 ] && echo "READY" || echo "NOT READY: run '$0 install'"
  return "$ok"
}

install() {
  for t in git cmake; do command -v "$t" >/dev/null 2>&1 || { echo "need $t first (Homebrew: brew install $t)"; exit 1; }; done
  [ "$(converter)" = none ] && { echo "need ffmpeg (macOS has afconvert built in)"; exit 1; }
  # Do not use 'brew install whisper-cpp' on an Intel Mac with a recent macOS: there are no prebuilt
  # packages, so Homebrew compiles LLVM from source and upgrades other libraries.
  [ -d "$ROOT/.git" ] || git clone --depth 1 https://github.com/ggml-org/whisper.cpp "$ROOT"
  cmake -S "$ROOT" -B "$ROOT/build" -DCMAKE_BUILD_TYPE=Release
  cmake --build "$ROOT/build" -j "$(threads)" --config Release
  [ -f "$MODEL_FILE" ] || sh "$ROOT/models/download-ggml-model.sh" "$MODEL"
  check
}

run() {
  local audio="${1:?audio file}" outdir="${2:-.}" prompt="${3:-}"
  check >/dev/null || { echo "whisper.cpp not ready; run '$0 install' (see '$0 check')"; exit 2; }
  mkdir -p "$outdir"
  local name wav; name="$(basename "${audio%.*}")"; wav="$outdir/$name.16k.wav"
  case "$(converter)" in
    afconvert) afconvert -f WAVE -d LEI16@16000 -c 1 "$audio" "$wav" ;;
    ffmpeg) ffmpeg -y -loglevel error -i "$audio" -ar 16000 -ac 1 -c:a pcm_s16le "$wav" ;;
  esac
  # --prompt (not -p, which is the number of processors) biases spelling toward the listed vocabulary.
  local args=(-m "$MODEL_FILE" -f "$wav" -t "$(threads)" -otxt -osrt -of "$outdir/$name")
  [ -n "$(gpu_flag)" ] && args+=("$(gpu_flag)")
  [ -n "$prompt" ] && args+=(--prompt "$prompt")
  "$BIN" "${args[@]}" > "$outdir/$name.log" 2>&1
  rm -f "$wav"
  echo "wrote $outdir/$name.txt and $outdir/$name.srt"
  # Quality flag: runs of identical one-word segments usually mean the decoder stalled on overlapping speech.
  awk 'BEGIN{RS="";FS="\n"} {t=$3; if (t==prev) {n++; if (n==5) print "WARNING: repeated segments near " $2 " (check the audio there)"} else n=0; prev=t}' "$outdir/$name.srt" || true
}

case "${1:-}" in
  check) check ;;
  install) install ;;
  run) shift; run "$@" ;;
  *) sed -n 2,12p "$0"; exit 1 ;;
esac
