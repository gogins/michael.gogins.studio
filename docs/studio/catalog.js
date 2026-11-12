// Access to the published catalog and files.
//
// Everything is addressed relative to ../compositions/catalog.json, so the
// viewer works wherever the docs/ directory is served from.

export const CATALOG_URL = new URL('../compositions/catalog.json', import.meta.url);
const BASE_URL = new URL('./', CATALOG_URL);
const textCache = new Map();

export async function loadCatalog() {
  const response = await fetch(CATALOG_URL, { cache: 'no-cache' });
  if (!response.ok) {
    throw new Error(`catalog.json: HTTP ${response.status}`);
  }
  const catalog = await response.json();
  if (!catalog || !Array.isArray(catalog.compositions)) {
    throw new Error('catalog.json has an unexpected format');
  }
  return catalog;
}

// URL of a published file. Paths that would leave the compositions
// directory are refused, whatever the catalog says.
export function fileUrl(path) {
  const url = new URL(path, BASE_URL);
  if (!url.href.startsWith(BASE_URL.href)) {
    throw new Error(`path outside the published compositions: ${path}`);
  }
  return url.href;
}

export async function fetchText(path) {
  if (!textCache.has(path)) {
    const promise = fetch(fileUrl(path), { cache: 'no-cache' }).then((response) => {
      if (!response.ok) throw new Error(`${path}: HTTP ${response.status}`);
      return response.text();
    });
    promise.catch(() => textCache.delete(path));
    textCache.set(path, promise);
  }
  return textCache.get(path);
}

export function findComposition(catalog, name) {
  return catalog.compositions.find((composition) => composition.name === name) || null;
}

export function findVersion(composition, id) {
  return composition.versions.find((version) => version.id === id) || null;
}

export function versionLabel(version) {
  // The ordinal comes from the manifest, which may not have been published.
  if (!version.ordinal) return version.is_current ? 'Current' : version.id;
  const number = `version ${version.ordinal}`;
  return version.is_current ? `Current (${number})` : `${number} (${version.id})`;
}

// A prompt file is a heading, a metadata comment, and the verbatim text.
// The text itself is embedded in the catalog, because some static hosts
// (GitHub Pages with Jekyll) turn published .md files into HTML pages.
export function promptParts(composition, prompt) {
  const raw = (composition.prompt_texts || {})[prompt.sha256];
  if (typeof raw !== 'string') return { text: null, recorded: null, source: null };
  const match = raw.match(/^# [^\n]*\n\n<!-- recorded: ([^;]*); source: ([^ ]*) -->\n\n([\s\S]*)$/);
  if (!match) return { text: raw, recorded: null, source: null };
  return { text: match[3], recorded: match[1], source: match[2], raw };
}
