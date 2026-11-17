/** Docked Csound message windowshade for the viewer. */

import { h, replaceChildren } from './dom.js';

const HOST_ID = 'csound-messages-host';
const ROOT_ID = 'csound-messages';
const SHADE_ID = 'csound-messages-shade';
const TEXT_ID = 'csound-messages-text';
const MIN_HEIGHT = 72;
const MAX_HEIGHT = 480;
const DEFAULT_HEIGHT = 160;

let root = null;
let textNode = null;
let buffer = '';
let collapsed = false;
let height = DEFAULT_HEIGHT;

function applyHeight() {
  document.documentElement.style.setProperty('--csound-messages-height', `${height}px`);
  if (root && !collapsed) {
    root.style.height = `${height}px`;
  }
}

function setCollapsed(next) {
  collapsed = Boolean(next);
  if (!root) return;
  root.dataset.collapsed = collapsed ? 'true' : 'false';
  if (collapsed) {
    root.style.height = '';
  } else {
    applyHeight();
  }
}

function ensureMounted() {
  if (root && root.isConnected) return root;
  const host = document.getElementById(HOST_ID);
  if (!host) return null;

  textNode = h('pre', {
    class: 'csound-messages-text',
    id: TEXT_ID,
    'aria-live': 'polite',
  });
  const body = h('div', { class: 'csound-messages-body' }, textNode);
  const shade = h('div', {
    class: 'csound-messages-shade',
    id: SHADE_ID,
    role: 'button',
    tabindex: '0',
    title: 'Drag to resize; double-click to collapse',
    'aria-controls': ROOT_ID,
    'aria-expanded': 'true',
  },
    h('span', { class: 'csound-messages-grip', 'aria-hidden': 'true' }),
    h('span', { class: 'csound-messages-label' }, 'Csound messages'));

  root = h('div', {
    class: 'csound-messages',
    id: ROOT_ID,
    'data-collapsed': 'false',
  }, shade, body);

  replaceChildren(host, root);
  document.documentElement.classList.add('has-csound-messages');
  applyHeight();

  let dragging = false;
  let startY = 0;
  let startHeight = height;

  const onMove = (event) => {
    if (!dragging) return;
    const clientY = event.touches ? event.touches[0].clientY : event.clientY;
    const delta = startY - clientY;
    height = Math.max(MIN_HEIGHT, Math.min(MAX_HEIGHT, startHeight + delta));
    setCollapsed(false);
    applyHeight();
  };
  const onUp = () => {
    if (!dragging) return;
    dragging = false;
    document.documentElement.classList.remove('csound-messages-dragging');
    window.removeEventListener('pointermove', onMove);
    window.removeEventListener('pointerup', onUp);
    window.removeEventListener('touchmove', onMove);
    window.removeEventListener('touchend', onUp);
  };
  const onDown = (event) => {
    if (event.button != null && event.button !== 0) return;
    dragging = true;
    startY = event.touches ? event.touches[0].clientY : event.clientY;
    startHeight = height;
    document.documentElement.classList.add('csound-messages-dragging');
    window.addEventListener('pointermove', onMove);
    window.addEventListener('pointerup', onUp);
    window.addEventListener('touchmove', onMove, { passive: false });
    window.addEventListener('touchend', onUp);
    event.preventDefault();
  };

  shade.addEventListener('pointerdown', onDown);
  shade.addEventListener('touchstart', onDown, { passive: false });
  shade.addEventListener('dblclick', () => {
    setCollapsed(!collapsed);
    shade.setAttribute('aria-expanded', collapsed ? 'false' : 'true');
  });
  shade.addEventListener('keydown', (event) => {
    if (event.key === 'Enter' || event.key === ' ') {
      event.preventDefault();
      setCollapsed(!collapsed);
      shade.setAttribute('aria-expanded', collapsed ? 'false' : 'true');
    }
  });

  return root;
}

export function mountMessagesPanel() {
  ensureMounted();
}

export function clearMessages() {
  buffer = '';
  if (!textNode) ensureMounted();
  if (textNode) replaceChildren(textNode, '');
}

export function appendMessages(message) {
  if (!textNode) ensureMounted();
  if (!textNode) return;
  const chunk = String(message == null ? '' : message);
  if (!chunk) return;
  buffer += chunk.endsWith('\n') ? chunk : `${chunk}\n`;
  if (buffer.length > 200000) buffer = buffer.slice(buffer.length - 200000);
  replaceChildren(textNode, buffer);
  textNode.scrollTop = textNode.scrollHeight;
}

export function revealMessages() {
  ensureMounted();
  setCollapsed(false);
  if (root) {
    try { root.scrollIntoView({ block: 'nearest' }); } catch (_) { /* old browsers */ }
  }
}
