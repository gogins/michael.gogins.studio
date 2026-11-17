// Docked Csound message pane under the viewer.
//
// player.js streams compile and performance messages here. The pane is a
// windowshade: drag the header to resize, click the label to collapse.

import { h, replaceChildren } from './dom.js';

const MIN_HEIGHT = 72;
const MAX_HEIGHT = 480;
const DEFAULT_HEIGHT = 160;

let host = null;
let root = null;
let body = null;
let text = null;
let collapsed = false;
let height = DEFAULT_HEIGHT;

export function mountMessagesPanel() {
  host = document.getElementById('csound-messages-host');
  if (!host || root) return;
  const grip = h('span', { class: 'csound-messages-grip', 'aria-hidden': 'true' });
  const label = h('span', { class: 'csound-messages-label' }, 'Csound messages');
  const shade = h('div', {
    class: 'csound-messages-shade',
    role: 'button',
    tabindex: '0',
    'aria-expanded': 'true',
    title: 'Drag to resize; click to collapse',
  }, grip, label);
  text = h('pre', { class: 'csound-messages-text', id: 'csound-messages-text' });
  body = h('div', { class: 'csound-messages-body' }, text);
  root = h('div', {
    class: 'csound-messages',
    id: 'csound-messages',
    'data-collapsed': 'false',
  }, shade, body);
  replaceChildren(host, root);
  applyHeight();
  document.documentElement.classList.add('has-csound-messages');

  shade.addEventListener('pointerdown', (event) => {
    if (event.button !== 0) return;
    const startY = event.clientY;
    const startHeight = height;
    let dragged = false;
    document.documentElement.classList.add('csound-messages-dragging');
    const onMove = (move) => {
      const delta = startY - move.clientY;
      if (Math.abs(delta) > 3) dragged = true;
      height = Math.max(MIN_HEIGHT, Math.min(MAX_HEIGHT, startHeight + delta));
      if (collapsed) {
        collapsed = false;
        root.dataset.collapsed = 'false';
        shade.setAttribute('aria-expanded', 'true');
      }
      applyHeight();
    };
    const onUp = () => {
      document.documentElement.classList.remove('csound-messages-dragging');
      window.removeEventListener('pointermove', onMove);
      window.removeEventListener('pointerup', onUp);
      if (!dragged) toggleCollapsed();
    };
    window.addEventListener('pointermove', onMove);
    window.addEventListener('pointerup', onUp);
  });
  shade.addEventListener('keydown', (event) => {
    if (event.key === 'Enter' || event.key === ' ') {
      event.preventDefault();
      toggleCollapsed();
    }
  });
}

function toggleCollapsed() {
  if (!root) return;
  collapsed = !collapsed;
  root.dataset.collapsed = collapsed ? 'true' : 'false';
  root.querySelector('.csound-messages-shade')?.setAttribute(
    'aria-expanded',
    collapsed ? 'false' : 'true',
  );
  applyHeight();
}

function applyHeight() {
  document.documentElement.style.setProperty(
    '--csound-messages-height',
    `${collapsed ? 28 : height}px`,
  );
  if (body) body.style.height = collapsed ? '0px' : `${height - 28}px`;
}

export function clearMessages() {
  if (text) replaceChildren(text, '');
}

export function appendMessages(message) {
  if (!text) return;
  const chunk = String(message);
  text.appendChild(document.createTextNode(chunk.endsWith('\n') ? chunk : `${chunk}\n`));
  text.scrollTop = text.scrollHeight;
}

export function revealMessages() {
  if (!root) return;
  if (collapsed) toggleCollapsed();
  try {
    root.scrollIntoView({ block: 'nearest' });
  } catch (_) {
    /* old browsers */
  }
}
