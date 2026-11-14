#!/usr/bin/env bash
# Install or update the Csound WebAssembly engine the viewer plays with.
#
#   studio-tools/js/update-csound-browser.sh [VERSION]      (default 7.0.0)
#
# Downloads the official @csound/browser package from the npm registry,
# checks the download against the registry's integrity hash, and copies the
# single-file ES module, unmodified, with its licence files into
# docs/studio/csound/. VERSION.json there records what was installed.
#
# Nothing is installed globally and no node_modules directory is created.
# Review and commit the result yourself.
set -euo pipefail

VERSION="${1:-7.0.0}"
PACKAGE="@csound/browser"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="${STUDIO_ROOT:-$(cd "$HERE/../.." && pwd)}"
DEST="$ROOT/docs/studio/csound"

for tool in npm tar openssl; do
  command -v "$tool" >/dev/null || { echo "update-csound-browser: $tool is required" >&2; exit 1; }
done
sha256() { openssl dgst -sha256 "$1" | awk '{print $NF}'; }

WORK="$(mktemp -d "${TMPDIR:-/tmp}/studio-csound-browser.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
cd "$WORK"

EXPECTED="$(npm view "$PACKAGE@$VERSION" dist.integrity)"
[ -n "$EXPECTED" ] || { echo "update-csound-browser: $PACKAGE@$VERSION is not in the npm registry" >&2; exit 1; }
TARBALL="$(npm pack "$PACKAGE@$VERSION" --silent | tail -1)"
ACTUAL="sha512-$(openssl dgst -sha512 -binary "$TARBALL" | openssl base64 -A)"
if [ "$ACTUAL" != "$EXPECTED" ]; then
  echo "update-csound-browser: integrity mismatch for $TARBALL" >&2
  echo "  registry: $EXPECTED" >&2
  echo "  download: $ACTUAL" >&2
  exit 1
fi
tar -xzf "$TARBALL"
[ -f package/dist/csound.js ] || { echo "update-csound-browser: no dist/csound.js in $PACKAGE@$VERSION" >&2; exit 1; }

mkdir -p "$DEST"
cp package/dist/csound.js "$DEST/csound.js"
cp package/LICENSE "$DEST/LICENSE"
cp package/THIRD_PARTY.md "$DEST/THIRD_PARTY.md"
cat > "$DEST/VERSION.json" <<EOF
{
  "package": "$PACKAGE",
  "version": "$VERSION",
  "license": "Apache-2.0",
  "source": "https://www.npmjs.com/package/@csound/browser/v/$VERSION",
  "repository": "https://github.com/csound/csound",
  "npm_integrity": "$EXPECTED",
  "files": {
    "csound.js": {"from": "dist/csound.js", "sha256": "$(sha256 "$DEST/csound.js")"},
    "LICENSE": {"from": "LICENSE", "sha256": "$(sha256 "$DEST/LICENSE")"},
    "THIRD_PARTY.md": {"from": "THIRD_PARTY.md", "sha256": "$(sha256 "$DEST/THIRD_PARTY.md")"}
  },
  "note": "Unmodified files from the npm package; installed by studio-tools/js/update-csound-browser.sh."
}
EOF
echo "installed $PACKAGE $VERSION in ${DEST#"$ROOT"/}"
ls -l "$DEST"
