#!/bin/sh
# Runs the automated tests of the AI composition studio.
#
#   studio-tools/run-tests.sh              all tests
#   studio-tools/run-tests.sh -v           all tests, one line each
#   studio-tools/run-tests.sh test_publish one module
#
# The tests work in temporary directories; they read the repository but never
# write to it. Tests whose tools are missing (real Csound, CsoundAC, Node.js,
# Chrome, ffmpeg) are skipped and reported as skipped. See docs/testing.md.
set -eu
cd "$(dirname "$0")/tests"
PYTHONDONTWRITEBYTECODE=1
export PYTHONDONTWRITEBYTECODE
verbosity=
if [ "${1:-}" = "-v" ]; then
    verbosity=-v
    shift
fi
if [ "$#" -gt 0 ]; then
    exec "${PYTHON:-python3}" -m unittest -b $verbosity "$@"
fi
exec "${PYTHON:-python3}" -m unittest discover -b $verbosity -p 'test_*.py'
