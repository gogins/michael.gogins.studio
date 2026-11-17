/** Discover and call the local compose API exposed by `studio serve`. */

const CANDIDATES = [
  '/api/v1',
  `${window.location.origin}/api/v1`,
];

let cached = null; // { base, status } | null | undefined (not yet tried)
let inflight = null;

function configComposeApi() {
  try {
    // Optional static override for a hosted compose service.
    const node = document.querySelector('meta[name="compose-api"]');
    if (node && node.content) return node.content.replace(/\/$/, '');
  } catch (_) { /* ignore */ }
  return null;
}

async function probe(base) {
  const url = `${base.replace(/\/$/, '')}/status`;
  const response = await fetch(url, { method: 'GET', cache: 'no-store' });
  if (!response.ok) throw new Error(`status ${response.status}`);
  const status = await response.json();
  if (!status || status.ok !== true) throw new Error('compose status not ok');
  return { base: base.replace(/\/$/, ''), status };
}

export function resetComposeDiscovery() {
  cached = undefined;
  inflight = null;
}

export async function discoverComposeApi({ force = false } = {}) {
  if (!force && cached !== undefined) return cached;
  if (!force && inflight) return inflight;

  inflight = (async () => {
    const bases = [];
    const fromConfig = configComposeApi();
    if (fromConfig) bases.push(fromConfig);
    for (const candidate of CANDIDATES) {
      if (!bases.includes(candidate)) bases.push(candidate);
    }
    for (const base of bases) {
      try {
        const found = await probe(base);
        cached = found;
        return found;
      } catch (_) {
        /* try next */
      }
    }
    cached = null;
    return null;
  })();

  try {
    return await inflight;
  } finally {
    inflight = null;
  }
}

export async function submitCompose(body) {
  const api = await discoverComposeApi();
  if (!api) {
    throw new Error(
      'Compose API not found. Keep studio serve running, then open /studio/ on that same origin.',
    );
  }
  const response = await fetch(`${api.base}/compose`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body || {}),
  });
  let payload = null;
  try {
    payload = await response.json();
  } catch (_) {
    payload = null;
  }
  if (!response.ok || !payload || payload.ok !== true) {
    const message = (payload && payload.error) || `compose failed (${response.status})`;
    throw new Error(message);
  }
  return payload;
}
