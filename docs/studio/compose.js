// Discover and call the local compose endpoint that studio serve exposes.
//
// On GitHub Pages there is no server-side compose; config.json may name a
// hosted base URL. Locally, studio serve provides ../api/v1/ next to this
// viewer. All default discovery stays relative so the page works under any
// site prefix.

const CONFIG_URL = new URL('./config.json', import.meta.url);
const LOCAL_API = new URL('../api/v1/', import.meta.url);

let cached = undefined; // undefined = not tried; null = missing; object = ready

export function resetComposeDiscovery() {
  cached = undefined;
}

export async function discoverComposeApi({ force = false } = {}) {
  if (!force && cached !== undefined) return cached;
  const bases = [];
  try {
    const response = await fetch(CONFIG_URL, { cache: 'no-cache' });
    if (response.ok) {
      const config = await response.json();
      if (config && typeof config.composeApi === 'string' && config.composeApi.trim()) {
        bases.push(config.composeApi.trim());
      }
    }
  } catch (_) {
    // Missing or unreadable config is fine; try the local serve path.
  }
  bases.push(LOCAL_API.href);
  for (const base of bases) {
    const found = await probe(base);
    if (found) {
      cached = found;
      return cached;
    }
  }
  cached = null;
  return null;
}

async function probe(base) {
  const root = String(base).replace(/\/?$/, '/');
  try {
    const response = await fetch(new URL('status', root), {
      cache: 'no-cache',
      headers: { Accept: 'application/json' },
    });
    if (!response.ok) return null;
    const status = await response.json();
    if (!status || status.ok !== true) return null;
    return { base: root.replace(/\/$/, ''), status };
  } catch (_) {
    return null;
  }
}

export async function submitCompose(body) {
  const api = await discoverComposeApi();
  if (!api) {
    throw new Error('Compose endpoint not available. Run studio serve and reload.');
  }
  const response = await fetch(`${api.base}/compose`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json', Accept: 'application/json' },
    body: JSON.stringify(body || {}),
  });
  let payload = null;
  try {
    payload = await response.json();
  } catch (_) {
    payload = null;
  }
  if (!response.ok || !payload || payload.ok !== true) {
    const detail = (payload && (payload.error || payload.message)) || `HTTP ${response.status}`;
    throw new Error(detail);
  }
  return payload;
}
