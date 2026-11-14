#!/usr/bin/env bash
# Builds the rendering environment of the AI composition studio on Ubuntu,
# for a Cursor Cloud Agent or any other Linux machine:
#
#   Csound 7 (from source), CsoundAC with its Python bindings (from source),
#   the CsoundAC WebAssembly build (from a csound-wasm release), Node.js,
#   Node.js, and a C++ toolchain.
#
# STATUS: this script has NOT been run on Linux. It was written on macOS,
# where neither Docker nor a Cloud Agent was available, from the recipes that
# the Csound and CsoundAC projects use themselves:
#
#   csound/BUILD.md ("Ubuntu Linux")
#   csound-ac/.github/workflows/cmake.yml (the "Linux" job: package list,
#       "Install Csound 7 from GitHub - Linux", "Configure - Linux")
#
# Expect to correct it the first time it runs; see docs/cloud-agents.md.
# Run cloud/check.sh afterwards: only that script says whether the
# environment works.
#
# Nothing is installed into the repository. Everything goes under
# STUDIO_PREFIX; sources and build trees go under STUDIO_SOURCE_DIR.
#
# Configuration (environment variables):
#
#   STUDIO_PREFIX        install prefix        (default: $HOME/.local/ai-composition-studio)
#   STUDIO_SOURCE_DIR    sources and builds    (default: $HOME/.cache/ai-composition-studio)
#   STUDIO_INSTALL_STEPS steps to run          (default: "packages csound csoundac wasm env")
#   CSOUND_REPO, CSOUND_REF          Csound repository and branch, tag or commit
#   CSOUNDAC_REPO, CSOUNDAC_REF      CsoundAC repository and branch, tag or commit
#   CSOUND_WASM_REPO, CSOUND_WASM_RELEASE, CSOUND_WASM_ASSET   csound-wasm release
#   STUDIO_JOBS          parallel build jobs   (default: number of processors)
#
# With no *_REF the default branch of each repository is built, as the
# CsoundAC continuous integration does. For reproducible renders, pin both
# to commits once a combination is known to work. The commits actually
# built are written to $STUDIO_PREFIX/studio-versions.txt and recorded in
# every manifest through STUDIO_CSOUNDAC_REVISION.

set -euo pipefail

STUDIO_PREFIX=${STUDIO_PREFIX:-$HOME/.local/ai-composition-studio}
STUDIO_SOURCE_DIR=${STUDIO_SOURCE_DIR:-$HOME/.cache/ai-composition-studio}
STUDIO_INSTALL_STEPS=${STUDIO_INSTALL_STEPS:-packages csound csoundac wasm env}
CSOUND_REPO=${CSOUND_REPO:-https://github.com/csound/csound.git}
CSOUND_REF=${CSOUND_REF:-}
CSOUNDAC_REPO=${CSOUNDAC_REPO:-https://github.com/gogins/csound-ac.git}
CSOUNDAC_REF=${CSOUNDAC_REF:-}
CSOUND_WASM_REPO=${CSOUND_WASM_REPO:-gogins/csound-wasm}
CSOUND_WASM_RELEASE=${CSOUND_WASM_RELEASE:-v4.1.2-beta}
CSOUND_WASM_ASSET=${CSOUND_WASM_ASSET:-csound-wasm-4.0.0-beta.zip}
STUDIO_JOBS=${STUDIO_JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 2)}

TOOLS_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

say() { printf '\n==> %s\n' "$*"; }
fail() { printf 'install.sh: %s\n' "$*" >&2; exit 1; }

if [ "$(uname -s)" != "Linux" ]; then
    fail "this script is for Linux; on macOS see studio-tools/docs/dependencies.md"
fi

as_root() {
    if [ "$(id -u)" -eq 0 ]; then "$@"; else sudo "$@"; fi
}

# Clones a repository, or updates an existing clone, and checks out a ref.
fetch() {
    local repo=$1 dir=$2 ref=$3
    if [ ! -d "$dir/.git" ]; then
        git clone --recurse-submodules "$repo" "$dir"
    else
        git -C "$dir" fetch --tags origin
    fi
    if [ -n "$ref" ]; then
        git -C "$dir" checkout --detach "$ref"
    elif [ -z "$(git -C "$dir" status --porcelain --untracked-files=no)" ]; then
        git -C "$dir" pull --ff-only || true
    fi
    git -C "$dir" submodule update --init --recursive
}

step_packages() {
    say "Installing Ubuntu packages"
    export DEBIAN_FRONTEND=noninteractive
    as_root apt-get update
    # The list of the CsoundAC Linux build, plus what the studio itself uses:
    # Node.js (JavaScript generators and the viewer-engine check), unzip and curl
    # (the WebAssembly release), libsamplerate and gettext (csound/BUILD.md).
    as_root apt-get install -y --no-install-recommends \
        git ca-certificates curl unzip \
        build-essential cmake ninja-build pkg-config \
        bison flex gettext \
        swig python3 python3-dev python3-pip \
        libboost-all-dev libeigen3-dev \
        libsndfile1-dev libsamplerate0-dev \
        libasound2-dev libjack-jackd2-dev libpulse-dev portaudio19-dev libportaudio2 \
        liblo-dev libffi-dev libcurl4-openssl-dev \
        doxygen graphviz \
        nodejs
}

step_csound() {
    say "Building Csound 7"
    mkdir -p "$STUDIO_SOURCE_DIR" "$STUDIO_PREFIX"
    fetch "$CSOUND_REPO" "$STUDIO_SOURCE_DIR/csound" "$CSOUND_REF"
    cmake -S "$STUDIO_SOURCE_DIR/csound" -B "$STUDIO_SOURCE_DIR/csound/build-studio" -G Ninja \
        -DCMAKE_BUILD_TYPE=Release \
        -DBUILD_TESTS=OFF \
        -DBUILD_JAVA_INTERFACE=OFF \
        -DBUILD_PYTHON_INTERFACE=OFF \
        -DINSTALL_PYTHON_INTERFACE=OFF \
        -DBUILD_PYTHON_OPCODES=OFF \
        -DUSE_PYTHON=OFF \
        -DCMAKE_INSTALL_PREFIX="$STUDIO_PREFIX"
    cmake --build "$STUDIO_SOURCE_DIR/csound/build-studio" --parallel "$STUDIO_JOBS"
    cmake --install "$STUDIO_SOURCE_DIR/csound/build-studio"
}

step_csoundac() {
    say "Building CsoundAC and its Python bindings"
    # CsoundAC/CMakeLists.txt: find_package(Python3 3.12 REQUIRED ...).
    if ! python3 -c 'import sys; raise SystemExit(0 if sys.version_info >= (3, 12) else 1)'; then
        fail "CsoundAC needs Python 3.12 or later with development headers; this system has $(python3 --version 2>&1). Use Ubuntu 24.04 or later, or run with STUDIO_INSTALL_STEPS=\"packages csound wasm env\" to do without native CsoundAC."
    fi
    [ -x "$STUDIO_PREFIX/bin/csound" ] || fail "Csound is not installed in $STUDIO_PREFIX; run the csound step first"
    fetch "$CSOUNDAC_REPO" "$STUDIO_SOURCE_DIR/csound-ac" "$CSOUNDAC_REF"
    export LD_LIBRARY_PATH="$STUDIO_PREFIX/lib:$STUDIO_PREFIX/lib64${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
    cmake -S "$STUDIO_SOURCE_DIR/csound-ac" -B "$STUDIO_SOURCE_DIR/csound-ac/build-studio" -G Ninja \
        -DCMAKE_BUILD_TYPE=RelWithDebInfo \
        -DCMAKE_POLICY_VERSION_MINIMUM=3.5 \
        -DCMAKE_INSTALL_PREFIX="$STUDIO_PREFIX" \
        -DGENERATE_DOCUMENTATION=OFF \
        -DCMAKE_PREFIX_PATH="$STUDIO_PREFIX;/usr/local;/usr" \
        -DCSOUND_ROOT_HINT="$STUDIO_PREFIX" \
        -DPython3_EXECUTABLE="$(command -v python3)" \
        -DCSOUND_AC_INSTALL_PYTHON_TO_SITEARCH=OFF
    cmake --build "$STUDIO_SOURCE_DIR/csound-ac/build-studio" --parallel "$STUDIO_JOBS"
    cmake --install "$STUDIO_SOURCE_DIR/csound-ac/build-studio"
}

step_wasm() {
    say "Fetching the CsoundAC WebAssembly build ($CSOUND_WASM_RELEASE)"
    local dir="$STUDIO_PREFIX/csound-wasm"
    local url="https://github.com/$CSOUND_WASM_REPO/releases/download/$CSOUND_WASM_RELEASE/$CSOUND_WASM_ASSET"
    mkdir -p "$dir" "$STUDIO_SOURCE_DIR"
    curl --fail --location --retry 3 --output "$STUDIO_SOURCE_DIR/$CSOUND_WASM_ASSET" "$url"
    # Only the two files the studio's Node loader needs.
    unzip -o -q "$STUDIO_SOURCE_DIR/$CSOUND_WASM_ASSET" CsoundAC.js CsoundAC.wasm -d "$dir"
    printf '%s %s\n' "$CSOUND_WASM_RELEASE" "$CSOUND_WASM_ASSET" > "$dir/RELEASE"
}

revision_of() {
    if [ -d "$1/.git" ]; then git -C "$1" describe --tags --always --dirty 2>/dev/null || true; fi
}

step_env() {
    say "Writing $STUDIO_PREFIX/studio-env.sh"
    mkdir -p "$STUDIO_PREFIX"
    local csound_rev csoundac_rev python_dir
    csound_rev=$(revision_of "$STUDIO_SOURCE_DIR/csound")
    csoundac_rev=$(revision_of "$STUDIO_SOURCE_DIR/csound-ac")
    # CsoundAC installs its Python module here when
    # CSOUND_AC_INSTALL_PYTHON_TO_SITEARCH is OFF (CsoundAC/CMakeLists.txt).
    python_dir="$STUDIO_PREFIX/lib/python$(python3 -c 'import sys; print("%d.%d" % sys.version_info[:2])')/site-packages"
    {
        echo "csound ${csound_rev:-not built} ($CSOUND_REPO)"
        echo "csound-ac ${csoundac_rev:-not built} ($CSOUNDAC_REPO)"
        echo "csound-wasm $(cat "$STUDIO_PREFIX/csound-wasm/RELEASE" 2>/dev/null || echo 'not installed')"
    } > "$STUDIO_PREFIX/studio-versions.txt"
    cat > "$STUDIO_PREFIX/studio-env.sh" <<EOF
# Environment of the AI composition studio. Generated by studio-tools/cloud/install.sh.
# Use:  . "$STUDIO_PREFIX/studio-env.sh"
export STUDIO_PREFIX="$STUDIO_PREFIX"
export PATH="$TOOLS_DIR/bin:$STUDIO_PREFIX/bin:\$PATH"
export LD_LIBRARY_PATH="$STUDIO_PREFIX/lib:$STUDIO_PREFIX/lib64\${LD_LIBRARY_PATH:+:\$LD_LIBRARY_PATH}"
export PYTHONPATH="$python_dir\${PYTHONPATH:+:\$PYTHONPATH}"
export STUDIO_CSOUND="$STUDIO_PREFIX/bin/csound"
export STUDIO_PYTHON="$(command -v python3)"
export CSOUNDAC_WASM_DIR="$STUDIO_PREFIX/csound-wasm"
export STUDIO_CSOUNDAC_REVISION="${csoundac_rev:-unknown}"
export STUDIO_CSOUNDAC_CXXFLAGS="-I$STUDIO_PREFIX/include/csound/CsoundAC -I$STUDIO_PREFIX/include/csound -I/usr/include/eigen3"
export STUDIO_CSOUNDAC_LDFLAGS="-L$STUDIO_PREFIX/lib -lCsoundAC -Wl,-rpath,$STUDIO_PREFIX/lib"
EOF
    cat "$STUDIO_PREFIX/studio-versions.txt"
}

for step in $STUDIO_INSTALL_STEPS; do
    case "$step" in
        packages) step_packages ;;
        csound) step_csound ;;
        csoundac) step_csoundac ;;
        wasm) step_wasm ;;
        env) step_env ;;
        *) fail "unknown step: $step" ;;
    esac
done

say "Done. Now check the environment:"
echo "    . \"$STUDIO_PREFIX/studio-env.sh\" && \"$TOOLS_DIR/cloud/check.sh\""
