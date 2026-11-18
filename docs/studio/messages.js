// Page-level Csound message dock: always visible at the bottom of the window,
// with a windowshade to enlarge, shrink or hide the log. The main content
// scrolls above it. Cleared when the library is shown, when another
// composition or version is selected, or when a new piece is created.

import { h } from './dom.js';

const MAX_LINES = 3000;
const SHADE_HEIGHT = 28; // px; collapsed = shade only
const DEFAULT_HEIGHT = 160;
const STORAGE_KEY = 'studio-csound-messages-height';

let lines = [];
let dirty = false;
let rootEl = null;
let textEl = null;
let bodyEl = null;
let heightPx = DEFAULT_HEIGHT;
let expandedPx = DEFAULT_HEIGHT;
let collapsed = false;
let mounted = false;

export function mountMessagesPanel() {
  if (mounted && rootEl && rootEl.isConnected) return rootEl;
  heightPx = readStoredHeight();
  expandedPx = Math.max(DEFAULT_HEIGHT, heightPx);
  collapsed = heightPx <= SHADE_HEIGHT;
  if (collapsed) heightPx = SHADE_HEIGHT;

  textEl = h('pre', {
    class: 'csound-messages-text',
    id: 'csound-messages-text',
  });
  bodyEl = h('div', { class: 'csound-messages-body' }, textEl);
  const shade = h('div', {
    class: 'csound-messages-shade',
    id: 'csound-messages-shade',
    role: 'separator',
    tabindex: '0',
    'aria-orientation': 'horizontal',
    'aria-controls': 'csound-messages-text',
    'aria-label': 'Resize Csound messages. Double-click to hide or show.',
    title: 'Drag to resize · double-click to hide or show',
  },
    h('span', { class: 'csound-messages-grip', 'aria-hidden': 'true' }),
    h('span', { class: 'csound-messages-label' }, 'Csound messages'));

  rootEl = h('aside', {
    class: 'csound-messages',
    id: 'csound-messages',
  }, shade, bodyEl);

  bindShade(shade);
  applyHeight();
  lines = [];
  dirty = false;
  textEl.textContent = '';

  const host = document.getElementById('csound-messages-host') || document.body;
  host.append(rootEl);
  document.documentElement.classList.add('has-csound-messages');
  mounted = true;
  return rootEl;
}

export function clearMessages() {
  lines = [];
  dirty = false;
  if (textEl) textEl.textContent = '';
}

export function appendMessages(message) {
  for (const line of String(message).split('\n')) lines.push(line);
  if (lines.length > MAX_LINES) lines.splice(0, lines.length - MAX_LINES);
  dirty = true;
  flush();
  if (textEl && !collapsed) textEl.scrollTop = textEl.scrollHeight;
}

export function revealMessages() {
  if (collapsed) {
    heightPx = expandedPx;
    collapsed = false;
    applyHeight();
    storeHeight(heightPx);
  }
  flush();
  if (textEl) textEl.scrollTop = textEl.scrollHeight;
}

function flush() {
  if (!textEl || !dirty) return;
  textEl.textContent = lines.join('\n');
  dirty = false;
}

function readStoredHeight() {
  try {
    const value = Number(sessionStorage.getItem(STORAGE_KEY));
    if (Number.isFinite(value) && value >= SHADE_HEIGHT) return value;
  } catch {
    // private mode
  }
  return DEFAULT_HEIGHT;
}

function storeHeight(value) {
  try {
    sessionStorage.setItem(STORAGE_KEY, String(value));
  } catch {
    // private mode
  }
}

function maxHeight() {
  return Math.max(SHADE_HEIGHT, Math.floor(window.innerHeight * 0.7));
}

function applyHeight() {
  if (!rootEl) return;
  const height = collapsed ? SHADE_HEIGHT : Math.min(Math.max(heightPx, SHADE_HEIGHT + 1), maxHeight());
  if (!collapsed) {
    heightPx = height;
    expandedPx = height;
  }
  rootEl.style.height = `${height}px`;
  rootEl.dataset.collapsed = collapsed ? 'true' : 'false';
  document.documentElement.style.setProperty('--csound-messages-height', `${height}px`);
}

function bindShade(shade) {
  let dragging = false;
  let startY = 0;
  let startHeight = 0;

  const onMove = (event) => {
    if (!dragging) return;
    const point = event.touches ? event.touches[0] : event;
    const delta = startY - point.clientY; // drag up → taller
    const next = Math.min(maxHeight(), Math.max(SHADE_HEIGHT, startHeight + delta));
    if (next <= SHADE_HEIGHT + 4) {
      collapsed = true;
      heightPx = SHADE_HEIGHT;
    } else {
      collapsed = false;
      heightPx = next;
      expandedPx = next;
    }
    applyHeight();
    if (event.cancelable) event.preventDefault();
  };

  const onUp = () => {
    if (!dragging) return;
    dragging = false;
    document.documentElement.classList.remove('csound-messages-dragging');
    window.removeEventListener('pointermove', onMove);
    window.removeEventListener('pointerup', onUp);
    window.removeEventListener('touchmove', onMove);
    window.removeEventListener('touchend', onUp);
    storeHeight(collapsed ? SHADE_HEIGHT : heightPx);
  };

  const onDown = (event) => {
    if (event.button !== undefined && event.button !== 0) return;
    const point = event.touches ? event.touches[0] : event;
    dragging = true;
    startY = point.clientY;
    startHeight = collapsed ? SHADE_HEIGHT : heightPx;
    document.documentElement.classList.add('csound-messages-dragging');
    window.addEventListener('pointermove', onMove);
    window.addEventListener('pointerup', onUp);
    window.addEventListener('touchmove', onMove, { passive: false });
    window.addEventListener('touchend', onUp);
    event.preventDefault();
  };

  const toggle = () => {
    if (collapsed) {
      heightPx = Math.max(DEFAULT_HEIGHT, expandedPx);
      collapsed = false;
    } else {
      expandedPx = Math.max(DEFAULT_HEIGHT, heightPx);
      heightPx = SHADE_HEIGHT;
      collapsed = true;
    }
    applyHeight();
    storeHeight(collapsed ? SHADE_HEIGHT : heightPx);
  };

  shade.addEventListener('pointerdown', onDown);
  shade.addEventListener('touchstart', onDown, { passive: false });
  shade.addEventListener('dblclick', toggle);
  shade.addEventListener('keydown', (event) => {
    if (event.key === 'ArrowUp') {
      const base = collapsed ? expandedPx : heightPx;
      collapsed = false;
      heightPx = Math.min(maxHeight(), base + 24);
      applyHeight();
      storeHeight(heightPx);
      event.preventDefault();
    } else if (event.key === 'ArrowDown') {
      heightPx = Math.max(SHADE_HEIGHT, heightPx - 24);
      collapsed = heightPx <= SHADE_HEIGHT;
      applyHeight();
      storeHeight(collapsed ? SHADE_HEIGHT : heightPx);
      event.preventDefault();
    } else if (event.key === 'Enter' || event.key === ' ') {
      toggle();
      event.preventDefault();
    }
  });

  window.addEventListener('resize', () => applyHeight());
}
