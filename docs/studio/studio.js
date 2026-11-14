// AI Composition Studio viewer.
//
// A static page: it reads ../compositions/catalog.json and shows the
// published compositions, their versions, prompts, Csound files and
// generating programs. A version is heard by performing its .csd here, in
// the browser, with Csound for WebAssembly (player.js); no soundfiles are
// published. Location: #<composition>/<version>/<tab>.

import {
  CATALOG_URL, fetchText, fileUrl, findComposition, findVersion, loadCatalog, promptParts,
  versionLabel,
} from './catalog.js';
import { diffLines, hunks, summarize } from './diff.js';
import { h, replaceChildren, formatBytes, formatDate, formatDuration } from './dom.js';
import { createPlayer, stopAllPlayers } from './player.js';

const TABS = [
  ['prompts', 'Prompts'],
  ['csound', 'Csound'],
  ['code', 'Code'],
  ['files', 'Files'],
  ['compare', 'Compare'],
  ['info', 'Info'],
];
const MAX_INLINE_BYTES = 1500000;

const app = document.getElementById('app');
const footer = document.getElementById('footer');
let catalog = null;
const compareChoice = new Map(); // composition name -> { other, what }
let renderToken = 0;

function setState(state) {
  document.documentElement.dataset.studioState = state;
}

// ---------------------------------------------------------------- routing

function parseLocation() {
  const parts = decodeURIComponent(location.hash.replace(/^#\/?/, '')).split('/').filter(Boolean);
  return { name: parts[0] || null, version: parts[1] || null, tab: parts[2] || null };
}

function link(name, version, tab) {
  let target = '#';
  if (name) target += encodeURIComponent(name);
  if (name && version) target += `/${encodeURIComponent(version)}`;
  if (name && version && tab) target += `/${encodeURIComponent(tab)}`;
  return target;
}

function go(name, version, tab) {
  location.hash = link(name, version, tab);
}

// ---------------------------------------------------------------- library

function renderLibrary() {
  document.title = 'AI Composition Studio';
  if (catalog.compositions.length === 0) {
    replaceChildren(app,
      h('h1', {}, 'Compositions'),
      h('p', { class: 'status', id: 'empty-library' },
        'No compositions have been published yet. Publish one with ',
        h('code', {}, 'studio-tools/bin/studio publish NAME'), '.'));
    return;
  }
  const items = catalog.compositions.map((composition) => {
    const newest = composition.versions[0];
    const details = [
      `${composition.versions.length} version${composition.versions.length === 1 ? '' : 's'}`,
      newest.info.duration_seconds ? formatDuration(newest.info.duration_seconds) : null,
      composition.updated_at ? formatDate(composition.updated_at) : null,
    ].filter(Boolean).join(' · ');
    return h('li', {},
      h('a', { class: 'card', href: link(composition.name), dataset: { composition: composition.name } },
        h('span', { class: 'card-title' }, composition.title || composition.name),
        composition.title && composition.title !== composition.name
          ? h('span', { class: 'card-name' }, composition.name) : null,
        h('span', { class: 'card-details' }, details)));
  });
  replaceChildren(app, h('h1', {}, 'Compositions'), h('ul', { class: 'library' }, items));
}

// ------------------------------------------------------------ composition

// The page for one version is built once; changing tabs only replaces the
// panel, so the piece keeps playing while you read the prompts or the code.
let view = null;

function renderComposition(composition, version, tab) {
  if (!view || view.composition !== composition || view.version !== version) {
    buildComposition(composition, version);
  }
  for (const anchor of view.tabBar.children) {
    const selected = anchor.dataset.tab === tab;
    anchor.className = selected ? 'tab selected' : 'tab';
    anchor.setAttribute('aria-selected', selected ? 'true' : 'false');
    if (selected && view.tabBar.scrollWidth > view.tabBar.clientWidth) {
      // On a narrow screen, bring the selected tab into view within the tab bar only.
      view.tabBar.scrollLeft = Math.max(0, anchor.offsetLeft - view.tabBar.offsetLeft
        - (view.tabBar.clientWidth - anchor.offsetWidth) / 2);
    }
  }
  view.panel.dataset.tab = tab;
  const token = renderToken;
  const fill = { prompts: promptsPanel, csound: csoundPanel, code: codePanel, files: filesPanel,
    compare: comparePanel, info: infoPanel }[tab];
  fill(view.panel, composition, version, () => token === renderToken);
}

function buildComposition(composition, version) {
  stopAllPlayers(); // another version: what was playing belongs to the page being replaced
  document.title = `${composition.title || composition.name} · AI Composition Studio`;
  const versions = composition.versions; // newest first
  const index = versions.indexOf(version);
  const older = versions[index + 1] || null;
  const newer = versions[index - 1] || null;
  const currentTab = () => parseLocation().tab;

  const select = h('select', {
    id: 'version-select',
    'aria-label': 'Version',
    onchange: (event) => go(composition.name, event.target.value, currentTab()),
  }, versions.map((item) => h('option', { value: item.id, selected: item === version },
    versionLabel(item) + (item.info.created_at ? ` — ${formatDate(item.info.created_at)}` : ''))));

  const versionBar = h('div', { class: 'version-bar' },
    h('button', {
      type: 'button', id: 'older', disabled: !older, title: 'Older version',
      onclick: () => older && go(composition.name, older.id, currentTab()),
    }, '◀ Older'),
    select,
    h('button', {
      type: 'button', id: 'newer', disabled: !newer, title: 'Newer version',
      onclick: () => newer && go(composition.name, newer.id, currentTab()),
    }, 'Newer ▶'));

  const tabBar = h('nav', { class: 'tabs', role: 'tablist' }, TABS.map(([id, label]) => h('a', {
    href: link(composition.name, version.id, id),
    role: 'tab',
    class: 'tab',
    dataset: { tab: id },
  }, label + tabCount(id, version))));

  const panel = h('section', { class: 'panel', id: 'panel' });
  const isNewComposition = !view || view.composition !== composition;
  replaceChildren(app,
    h('p', { class: 'back' }, h('a', { href: '#' }, '← All compositions')),
    h('h1', { id: 'composition-title' }, composition.title || composition.name),
    versionBar,
    createPlayer({ id: 'player', csd: version.csd, info: version.info }),
    tabBar,
    panel);
  view = { composition, version, tabBar, panel };
  if (isNewComposition) window.scrollTo(0, 0);
}

function tabCount(id, version) {
  if (id === 'prompts' && version.prompts.length) return ` (${version.prompts.length})`;
  if (id === 'code' && version.code.length) return ` (${version.code.length})`;
  return '';
}

function download(entry, label) {
  return h('a', { href: fileUrl(entry.path), download: entry.file, class: 'download' }, label || entry.file);
}

// ----------------------------------------------------------------- panels

function promptsPanel(panel, composition, version) {
  if (version.prompts.length === 0) {
    replaceChildren(panel, h('p', { class: 'status' }, 'No prompts were published for this version.'));
    return;
  }
  replaceChildren(panel, version.prompts.map((prompt) => {
    const parts = promptParts(composition, prompt);
    const meta = [parts.source ? `from the ${parts.source}` : null,
      parts.recorded ? formatDate(parts.recorded) : null].filter(Boolean).join(' · ');
    return h('article', { class: prompt.new ? 'prompt new' : 'prompt' },
      h('header', {},
        h('h2', {}, `Prompt ${prompt.number}`),
        prompt.new ? h('span', { class: 'badge' }, 'new in this version') : null,
        h('span', { class: 'meta' }, meta),
        parts.text === null ? download(prompt, 'download') : saveLink(prompt.file, parts.raw || parts.text)),
      parts.text === null
        ? h('p', { class: 'status' }, 'The text of this prompt is not in the catalog.')
        : h('pre', { class: 'prose' }, parts.text));
  }));
}

// A download link made from text already in memory.
function saveLink(filename, text) {
  const anchor = h('a', { href: '#', class: 'download', download: filename }, 'download');
  anchor.addEventListener('click', () => {
    const url = URL.createObjectURL(new Blob([text], { type: 'text/plain;charset=utf-8' }));
    anchor.href = url;
    setTimeout(() => URL.revokeObjectURL(url), 60000);
  });
  return anchor;
}

function sourceView(entry, isCurrent) {
  const body = h('div', { class: 'source-body' }, h('p', { class: 'status' }, 'Loading…'));
  const show = async () => {
    try {
      const text = await fetchText(entry.path);
      if (!isCurrent()) return;
      const lines = text.split('\n');
      if (lines.length > 1 && lines[lines.length - 1] === '') lines.pop();
      const numbers = lines.map((_, index) => index + 1).join('\n');
      replaceChildren(body, h('div', { class: 'source' },
        h('pre', { class: 'gutter', 'aria-hidden': 'true' }, numbers),
        h('pre', { class: 'code' }, h('code', {}, lines.join('\n')))));
      body.dataset.loaded = 'true';
    } catch (error) {
      replaceChildren(body, h('p', { class: 'status error' }, `Could not load ${entry.file}: ${error.message}`));
    }
  };
  if (entry.bytes > MAX_INLINE_BYTES) {
    replaceChildren(body, h('p', { class: 'status' },
      `${entry.file} is large (${formatBytes(entry.bytes)}). `,
      h('button', { type: 'button', onclick: show }, 'Show it anyway')));
  } else {
    show();
  }
  return h('article', { class: 'source-file', dataset: { file: entry.file } },
    h('header', {},
      h('h2', {}, entry.file),
      h('span', { class: 'meta' }, [entry.language, formatBytes(entry.bytes)].filter(Boolean).join(' · ')),
      download(entry, 'download'),
      h('a', { href: fileUrl(entry.path), target: '_blank', rel: 'noopener', class: 'download' }, 'open')),
    body);
}

function csoundPanel(panel, composition, version, isCurrent) {
  if (!version.csd) {
    replaceChildren(panel, h('p', { class: 'status' }, 'The .csd file was not published for this version.'));
    return;
  }
  replaceChildren(panel, sourceView(version.csd, isCurrent));
}

function codePanel(panel, composition, version, isCurrent) {
  if (version.code.length === 0) {
    const direct = version.info.generator === 'direct-csd';
    replaceChildren(panel, h('p', { class: 'status' }, direct
      ? 'This version was written directly as a .csd file; there is no generating program.'
      : 'No generating program was published for this version.'));
    return;
  }
  replaceChildren(panel, version.code.map((entry) => sourceView(entry, isCurrent)));
}

function filesPanel(panel, composition, version) {
  const rows = [];
  const add = (kind, entry) => rows.push(h('tr', {},
    h('td', {}, kind),
    h('td', {}, h('a', { href: fileUrl(entry.path), target: '_blank', rel: 'noopener' }, entry.file)),
    h('td', { class: 'number' }, formatBytes(entry.bytes)),
    h('td', {}, download(entry, 'download'))));
  if (version.csd) add('Csound', version.csd);
  version.code.forEach((entry) => add(`Code (${entry.language})`, entry));
  const names = { midi: 'MIDI', musicxml: 'MusicXML', pdf: 'Score (PDF)', image: 'Image', lilypond: 'LilyPond' };
  version.derived.forEach((entry) => add(names[entry.kind] || entry.kind, entry));
  version.logs.forEach((entry) => add('Log', entry));
  if (version.manifest) add('Provenance manifest', version.manifest);
  replaceChildren(panel,
    rows.length
      ? h('div', { class: 'table-wrap' }, h('table', { class: 'files' },
        h('thead', {}, h('tr', {}, ['Kind', 'File', 'Size', ''].map((text) => h('th', {}, text)))),
        h('tbody', {}, rows)))
      : h('p', { class: 'status' }, 'No files were published for this version.'),
    version.prompts.length
      ? h('p', { class: 'meta' }, `${version.prompts.length} prompt file(s) are shown under Prompts.`) : null);
}

function infoPanel(panel, composition, version) {
  const info = version.info;
  const rows = [
    ['Composition', composition.name],
    ['Version', versionLabel(version)],
    ['Created', formatDate(info.created_at)],
    ['AI model', info.ai_model],
    ['Agent', info.ai_agent],
    ['Generated by', { 'direct-csd': 'Csound file written directly',
      'python-csoundac': 'Python program using CsoundAC', python: 'Python program',
      'javascript-csoundac-wasm': 'JavaScript program using CsoundAC (WebAssembly)',
      javascript: 'JavaScript program', 'cpp-csoundac': 'C++ program using CsoundAC',
      cpp: 'C++ program' }[info.generator] || info.generator],
    ['Random seeds', (info.seeds || []).join(', ')],
    ['Csound', info.csound],
    ['CsoundAC', info.csoundac_revision],
    ['Python', info.python],
    ['Node.js', info.node],
    ['Duration', info.duration_seconds ? `${formatDuration(info.duration_seconds)} (${info.duration_seconds} s)` : ''],
    ['Audio', info.sample_rate ? `${info.sample_rate} Hz, ${info.channels} channel(s)` : ''],
    ['Level', info.peak_dbfs !== undefined ? `peak ${info.peak_dbfs} dBFS, RMS ${info.rms_dbfs} dBFS` : ''],
    ['Playing here', browserVerdict(info)],
    ['Notes', info.notes],
  ].filter(([, value]) => value !== undefined && value !== null && value !== '');
  if (rows.length <= 2 && !version.manifest) {
    rows.push(['Provenance', 'The manifest was not published for this version.']);
  }
  replaceChildren(panel,
    h('dl', { class: 'info' }, rows.map(([key, value]) => [h('dt', {}, key), h('dd', {}, value)])),
    (info.warnings || []).length
      ? h('div', { class: 'warnings' }, h('h2', {}, 'Render warnings'),
        h('ul', {}, info.warnings.map((warning) => h('li', {}, warning)))) : null,
    version.manifest ? h('p', {}, h('a', { href: fileUrl(version.manifest.path), target: '_blank', rel: 'noopener' },
      'Complete provenance manifest (JSON)')) : null);
}

// What was found when the version was made and its .csd was performed in
// the same Csound engine that the player uses.
function browserVerdict(info) {
  if (info.browser_checked === undefined) return '';
  const engine = info.browser_engine || 'the Csound engine of this viewer';
  if (!info.browser_checked) return `Not checked in ${engine} when this version was made.`;
  if (!info.browser_playable) return `Did not perform in ${engine} when this version was made.`;
  const speed = info.browser_realtime_factor
    ? ` (${info.browser_realtime_factor} times faster than real time on the machine that made it)` : '';
  return `Performed completely in ${engine} when this version was made${speed}.`;
}

// ---------------------------------------------------------------- compare

function comparable(version, other) {
  const options = [];
  if (version.csd && other.csd) options.push(['csd', version.csd.file]);
  for (const entry of version.code) {
    if (other.code.some((item) => item.file === entry.file)) options.push([`code:${entry.file}`, entry.file]);
  }
  return options;
}

function comparePanel(panel, composition, version, isCurrent) {
  const others = composition.versions.filter((item) => item !== version);
  if (others.length === 0) {
    replaceChildren(panel, h('p', { class: 'status' }, 'Only one version is published; there is nothing to compare with.'));
    return;
  }
  const index = composition.versions.indexOf(version);
  const remembered = compareChoice.get(composition.name) || {};
  const other = others.find((item) => item.id === remembered.other)
    || composition.versions[index + 1] || composition.versions[index - 1];
  const options = comparable(version, other);
  const what = options.some(([id]) => id === remembered.what) ? remembered.what : (options[0] || [null])[0];
  const choose = (change) => {
    compareChoice.set(composition.name, { other: other.id, what, ...change });
    comparePanel(panel, composition, version, isCurrent);
  };
  const result = h('div', { class: 'diff-result', id: 'diff-result' });
  replaceChildren(panel,
    h('div', { class: 'compare-controls' },
      h('label', {}, 'Compare with ',
        h('select', { id: 'compare-other', onchange: (event) => choose({ other: event.target.value }) },
          others.map((item) => h('option', { value: item.id, selected: item === other }, versionLabel(item))))),
      options.length > 1 ? h('label', {}, 'File ',
        h('select', { id: 'compare-what', onchange: (event) => choose({ what: event.target.value }) },
          options.map(([id, label]) => h('option', { value: id, selected: id === what }, label)))) : null),
    h('div', { class: 'compare-players' },
      h('div', {}, h('h2', {}, versionLabel(other)),
        createPlayer({ id: 'player-other', csd: other.csd, info: other.info })),
      h('div', {}, h('h2', {}, `${versionLabel(version)} (selected)`),
        h('p', { class: 'meta' }, 'Use the player at the top of the page. Only one version plays at a time: '
          + 'starting one stops the other.'))),
    promptDifference(version, other),
    result);
  if (!what) {
    replaceChildren(result, h('p', { class: 'status' }, 'These versions have no published file in common to compare.'));
    return;
  }
  const pick = (item) => (what === 'csd' ? item.csd : item.code.find((entry) => `code:${entry.file}` === what));
  // Always show the change from the older to the newer version.
  const versionIsNewer = composition.versions.indexOf(version) < composition.versions.indexOf(other);
  const [from, to] = versionIsNewer ? [other, version] : [version, other];
  replaceChildren(result, h('p', { class: 'status' }, 'Comparing…'));
  Promise.all([fetchText(pick(from).path), fetchText(pick(to).path)]).then(([oldText, newText]) => {
    if (!isCurrent() || !result.isConnected) return;
    renderDiff(result, oldText, newText, versionLabel(from), versionLabel(to));
  }).catch((error) => {
    replaceChildren(result, h('p', { class: 'status error' }, `Could not compare: ${error.message}`));
  });
}

function promptDifference(version, other) {
  const mine = new Set(version.prompts.map((prompt) => prompt.sha256));
  const theirs = new Set(other.prompts.map((prompt) => prompt.sha256));
  const onlyMine = version.prompts.filter((prompt) => !theirs.has(prompt.sha256));
  const onlyTheirs = other.prompts.filter((prompt) => !mine.has(prompt.sha256));
  if (onlyMine.length === 0 && onlyTheirs.length === 0) return null;
  const list = (prompts) => prompts.map((prompt) => `prompt ${prompt.number}`).join(', ');
  return h('p', { class: 'meta', id: 'prompt-difference' },
    onlyMine.length ? `Only in ${versionLabel(version)}: ${list(onlyMine)}. ` : null,
    onlyTheirs.length ? `Only in ${versionLabel(other)}: ${list(onlyTheirs)}.` : null);
}

function renderDiff(container, oldText, newText, oldLabel, newLabel) {
  const diff = diffLines(oldText, newText);
  if (diff === null) {
    replaceChildren(container, h('p', { class: 'status' },
      'These files differ too much for a line-by-line comparison. Open each version to read it.'));
    return;
  }
  const { added, removed } = summarize(diff);
  if (added === 0 && removed === 0) {
    replaceChildren(container, h('p', { class: 'status', id: 'diff-summary' }, 'The files are identical.'));
    return;
  }
  const rows = [];
  for (const hunk of hunks(diff)) {
    if (hunk.type === 'skip') {
      rows.push(h('div', { class: 'diff-skip' }, `⋯ ${hunk.count} unchanged line${hunk.count === 1 ? '' : 's'}`));
      continue;
    }
    for (const item of hunk.items) {
      const sign = { add: '+', remove: '−', same: ' ' }[item.type];
      rows.push(h('div', { class: `diff-line ${item.type}` },
        h('span', { class: 'diff-number' }, item.a === null ? '' : item.a),
        h('span', { class: 'diff-number' }, item.b === null ? '' : item.b),
        h('span', { class: 'diff-sign' }, sign),
        h('span', { class: 'diff-text' }, item.line)));
    }
  }
  replaceChildren(container,
    h('p', { class: 'meta', id: 'diff-summary' },
      `From ${oldLabel} to ${newLabel}: `,
      h('span', { class: 'added' }, `+${added}`), ' ', h('span', { class: 'removed' }, `−${removed}`), ' lines'),
    h('div', { class: 'diff' }, rows));
}

// ------------------------------------------------------------------- main

function render() {
  renderToken += 1;
  const { name, version: versionId, tab } = parseLocation();
  if (!name) {
    view = null;
    stopAllPlayers();
    renderLibrary();
    setState('ready');
    return;
  }
  const composition = findComposition(catalog, name);
  if (!composition) {
    view = null;
    stopAllPlayers();
    replaceChildren(app,
      h('p', { class: 'back' }, h('a', { href: '#' }, '← All compositions')),
      h('p', { class: 'status error', id: 'not-found' }, `There is no published composition named “${name}”.`));
    setState('ready');
    return;
  }
  const version = (versionId && findVersion(composition, versionId)) || composition.versions[0];
  const validTab = TABS.some(([id]) => id === tab) ? tab : 'prompts';
  renderComposition(composition, version, validTab);
  setState('ready');
}

async function main() {
  setState('loading');
  try {
    catalog = await loadCatalog();
  } catch (error) {
    replaceChildren(app,
      h('h1', {}, 'Compositions'),
      h('p', { class: 'status error', id: 'catalog-error' },
        `The catalog could not be loaded (${error.message}). `,
        'It is expected at ', h('code', {}, new URL(CATALOG_URL).pathname), '.'));
    setState('error');
    return;
  }
  if (catalog.generated_at) {
    replaceChildren(footer, `Catalog generated ${formatDate(catalog.generated_at)}`);
  }
  window.addEventListener('hashchange', render);
  render();
}

main();
