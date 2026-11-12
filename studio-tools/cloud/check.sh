#!/usr/bin/env bash
# Checks that this machine can do the studio's work, by doing it: reports the
# tools found, then creates, renders, revises, verifies, re-renders and
# publishes a small composition in a temporary directory. The repository is
# not touched.
#
#   studio-tools/cloud/check.sh            everything that is configured
#   studio-tools/cloud/check.sh --tests    also run the automated tests
#
# Works on macOS and Linux. Exits non-zero when a required part fails.
# Csound 7 is required; CsoundAC (Python), CsoundAC (WebAssembly), C++ and
# ffmpeg are each checked when configured and reported as missing otherwise.

set -euo pipefail

TOOLS_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
STUDIO="$TOOLS_DIR/bin/studio"
WORK=$(mktemp -d "${TMPDIR:-/tmp}/studio-check.XXXXXX")
trap 'rm -rf "$WORK"' EXIT
export STUDIO_ROOT="$WORK/repo"
mkdir -p "$STUDIO_ROOT"

failures=0
missing=()
ok() { printf '  ok       %s\n' "$*"; }
bad() { printf '  FAILED   %s\n' "$*"; failures=$((failures + 1)); }
skip() { printf '  missing  %s\n' "$*"; missing+=("$1"); }
# Runs a command quietly; shows its output only when it fails.
try() {
    local label=$1; shift
    if "$@" > "$WORK/output.txt" 2>&1; then ok "$label"; else bad "$label"; sed 's/^/           | /' "$WORK/output.txt"; fi
}

echo "Tools:"
"$STUDIO" env | sed 's/^/  /' || true
echo
echo "Checks:"

write_csd() {  # write_csd FILE FREQUENCY
    cat > "$1" <<EOF
<CsoundSynthesizer>
<CsOptions>
-d
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 32
nchnls = 2
0dbfs = 1
instr 1
  aEnvelope linseg 0, 0.02, 1, p3 - 0.04, 1, 0.02, 0
  aSignal poscil aEnvelope * 0.3 * (p5 / 127), cpsmidinn(p4) * $2 / 440
  aLeft, aRight pan2 aSignal, p7
  outs aLeft, aRight
endin
</CsInstruments>
<CsScore>
i 1 0 1 69 90 0 0.5
</CsScore>
</CsoundSynthesizer>
EOF
}

# 1. Csound 7: a direct .csd, rendered, accepted, revised and re-rendered.
if "$STUDIO" env --json | python3 -c 'import json,sys; raise SystemExit(0 if json.load(sys.stdin)["csound"].get("major") == 7 else 1)'; then
    ok "Csound 7 found"
else
    bad "Csound 7 not found (set STUDIO_CSOUND)"
fi
staging=$("$STUDIO" new check 2>/dev/null | tail -1) || true
if [ ! -d "$staging" ]; then
    bad "studio new did not report a staging directory"
else
    write_csd "$staging/check.csd" 440
    "$STUDIO" prompt "$staging" --text "Environment check: one note." --source agent > /dev/null
    try "render a .csd with Csound" "$STUDIO" render "$staging"
    try "accept it as the current version" "$STUDIO" accept "$staging" --model "environment check"
    staging=$("$STUDIO" stage check 2>/dev/null | tail -1) || true
    if [ -d "$staging" ]; then
        write_csd "$staging/check.csd" 660
        "$STUDIO" prompt "$staging" --text "Environment check: a fifth higher." --source agent > /dev/null
        try "render a revision" "$STUDIO" render "$staging"
        try "archive version-0001 and promote the revision" "$STUDIO" accept "$staging" --model "environment check"
        try "verify the archive" "$STUDIO" verify check
        try "re-render the archived version identically" bash -c "\"$STUDIO\" rerender check --version 1 | grep -q 'identical to the stored render'"
        try "publish and build the catalog" "$STUDIO" publish check
    else
        bad "the first version was not accepted, so no revision could be staged"
    fi
    if [ -f "$STUDIO_ROOT/music/ai-compositions/check/outputs/check.mp3" ]; then
        ok "FLAC master and MP3 listening copy encoded"
    else
        skip "ffmpeg (the WAV render is kept as the master; no MP3 for the viewer)"
    fi
fi

# 2. Generating programs, through the same tests the test suite uses.
generator() {  # generator LABEL TEST
    local output
    if output=$(cd "$TOOLS_DIR/tests" && env -u STUDIO_ROOT python3 -m unittest -v "test_generators.GeneratorTests.$2" 2>&1); then
        if printf '%s' "$output" | grep -q "skipped"; then skip "$1"; else ok "$1"; fi
    else
        bad "$1"; printf '%s\n' "$output" | tail -15 | sed 's/^/           | /'
    fi
}
generator "CsoundAC from Python (STUDIO_PYTHON)" test_python_with_csoundac
generator "CsoundAC WebAssembly from Node.js (CSOUNDAC_WASM_DIR)" test_javascript_with_csoundac_webassembly
generator "CsoundAC from C++ (STUDIO_CSOUNDAC_CXXFLAGS, STUDIO_CSOUNDAC_LDFLAGS)" test_cpp_with_csoundac

# 3. Optionally, the whole test suite.
if [ "${1:-}" = "--tests" ]; then
    echo
    echo "Automated tests:"
    if (unset STUDIO_ROOT; "$TOOLS_DIR/run-tests.sh"); then ok "test suite"; else bad "test suite"; fi
fi

echo
if [ "${#missing[@]}" -gt 0 ]; then
    echo "Not configured on this machine:"
    printf '  - %s\n' "${missing[@]}"
fi
if [ "$failures" -gt 0 ]; then
    echo "$failures check(s) FAILED."
    exit 1
fi
echo "All checks that could run passed."
