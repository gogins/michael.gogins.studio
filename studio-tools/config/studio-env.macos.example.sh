# Example environment for the AI composition studio on macOS.
#
# The studio finds Csound and Node.js on the PATH by itself, and the viewer's
# @csound/browser engine under docs/studio/csound/. This file
# tells it where the optional parts are. Adjust the paths, then either source
# it in your shell:
#
#     . studio-tools/config/studio-env.macos.example.sh
#
# or, to have the `studio` command load it automatically, save your copy as
#
#     ~/.local/ai-composition-studio/studio-env.sh
#
# (or anywhere else, and set STUDIO_ENV_FILE to it). Nothing here is secret;
# do not add credentials to this file.
#
# The values below are the ones the studio was developed and tested with on
# Michael Gogins's MacBook in November 2026. Check them with `studio env` and
# `studio-tools/cloud/check.sh`.

# A Python that can import CsoundAC. Homebrew's python3 cannot; CsoundAC is
# built for Python 3.12.
export STUDIO_PYTHON="$HOME/venv/csound/bin/python"

# The CsoundAC source checkout, used only to record its revision in manifests.
export CSOUNDAC_SOURCE_DIR="$HOME/csound-ac"

# CsoundAC.js and CsoundAC.wasm, from a csound-wasm build or release.
export CSOUNDAC_WASM_DIR="$HOME/csound-wasm/dist"

# For C++ generating programs (studio-cxx): CsoundAC, Csound, Boost and Eigen.
_csoundac="$HOME/csound-ac/dist/csound-ac"
export STUDIO_CSOUNDAC_CXXFLAGS="-I$_csoundac/include/csound/CsoundAC -I/Library/Frameworks/CsoundLib64.framework/Headers -I/opt/homebrew/include -I/opt/homebrew/include/eigen3"
export STUDIO_CSOUNDAC_LDFLAGS="-L$_csoundac/lib -lCsoundAC -Wl,-rpath,$_csoundac/lib"
unset _csoundac

# The studio command.
case ":$PATH:" in
    *"/studio-tools/bin:"*) ;;
    *) export PATH="$HOME/michael.gogins.studio/studio-tools/bin:$PATH" ;;
esac
