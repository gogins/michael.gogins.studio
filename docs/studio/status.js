// The one status line for the player. Compose, validation, and playback
// messages all land here: a single line fixed to the bottom of the screen.

const BAR_ID = 'site-readiness';

export function setStatus(text) {
  const bar = document.getElementById(BAR_ID);
  if (!bar) return;
  const line = String(text ?? '').replace(/\s+/g, ' ').trim();
  bar.textContent = line;
  bar.title = line;
  bar.scrollLeft = 0;
}
