// The player: performs a published .csd in real time, in this browser, with
// the official Csound for WebAssembly (@csound/browser, in ./csound/).
//
// No soundfile is involved anywhere. The same player is used wherever the
// viewer is served from, so listening is the same locally and on the site.
//
// One piece plays at a time. Each performance gets a fresh Csound instance
// and audio context, and both are released when it stops or ends.
//
// State is mirrored on the root element for styling and for tests:
//   data-state      idle | loading | playing | paused | error
//   data-time       score time in seconds
//   data-peak       highest output level since the performance started (0..1)
//   data-completed  number of performances that reached the end of the score

import { fetchText } from './catalog.js';
import { h, replaceChildren, formatDuration } from './dom.js';

const ENGINE_URL = new URL('./csound/csound.js', import.meta.url);
const TICK_MS = 100;
const MAX_LOG_LINES = 3000;

let enginePromise = null;
let active = null; // the player that is loading, playing or paused

// The engine is several megabytes, so it is fetched only when first needed.
function loadEngine() {
  if (!enginePromise) {
    enginePromise = import(ENGINE_URL.href).then((module) => {
      if (typeof module.Csound !== 'function') throw new Error('unexpected module format');
      return module.Csound;
    });
    enginePromise.catch(() => { enginePromise = null; });
  }
  return enginePromise;
}

// An audio context at the sample rate the piece was written for, so that it
// is performed here as it was rendered when the version was made.
function makeContext(sampleRate) {
  const Context = window.AudioContext || window.webkitAudioContext;
  if (!Context) throw new Error('this browser has no Web Audio');
  if (sampleRate) {
    try {
      return new Context({ sampleRate, latencyHint: 'playback' });
    } catch (error) {
      // Not a rate this device supports: use its own.
    }
  }
  return new Context({ latencyHint: 'playback' });
}

function clock(seconds) {
  return formatDuration(Math.max(0, seconds)) || '0:00';
}

export function stopAllPlayers() {
  if (active) active.stop();
}

// csd: the catalog entry of the version's .csd, or null if not published.
// info: the version's "info" from the catalog (duration, sample rate, ...).
export function createPlayer({ id, csd, info }) {
  if (!csd) {
    return h('p', { class: 'status no-player', id: `${id}-none` },
      'The .csd file was not published for this version, so it cannot be played here.');
  }
  const duration = typeof info.duration_seconds === 'number' && info.duration_seconds > 0
    ? info.duration_seconds : null;

  const playButton = h('button', { type: 'button', class: 'play' }, '▶ Play');
  const stopButton = h('button', { type: 'button', class: 'stop', disabled: true }, '■ Stop');
  const time = h('span', { class: 'player-time' });
  const seek = h('input', {
    type: 'range', class: 'seek', min: 0, max: duration || 1, step: 0.1, value: 0,
    'aria-label': 'Position in the piece', disabled: !duration,
    title: 'Go to a point in the piece. Notes that began before it are not heard.',
  });
  const level = h('div', { class: 'meter-level' });
  const status = h('p', { class: 'player-status', role: 'status' });
  const logText = h('pre', { class: 'player-log-text' });
  const log = h('details', { class: 'player-log' }, h('summary', {}, 'Csound messages'), logText);
  const caption = h('p', { class: 'player-caption' },
    ['Performed in this browser by Csound (WebAssembly); no soundfile',
      duration ? formatDuration(duration) : null,
      info.peak_dbfs !== undefined ? `peak ${info.peak_dbfs} dBFS` : null]
      .filter(Boolean).join(' · '));
  const root = h('div', { class: 'player', id },
    h('div', { class: 'player-controls' }, playButton, stopButton, time),
    duration ? seek : null,
    h('div', { class: 'meter', 'aria-hidden': 'true' }, level),
    caption,
    info.browser_playable === false ? h('p', { class: 'player-warning' },
      'When this version was made, it did not perform in this Csound engine; it may not play.') : null,
    status,
    log);

  let state = 'idle';
  let position = 0; // where Play starts from, and the time shown when not playing
  let token = 0; // changes whenever the current performance is superseded
  let session = null; // { csound, context, analyser, samples, timer }
  let dragging = false;
  let completed = 0;
  let peak = 0;
  const lines = [];
  let logDirty = false;

  function showTime(seconds) {
    root.dataset.time = String(Math.round(seconds * 100) / 100);
    replaceChildren(time, duration ? `${clock(seconds)} / ${clock(duration)}` : clock(seconds));
    if (duration && !dragging) seek.value = String(Math.min(seconds, duration));
  }

  function setState(next, message) {
    state = next;
    root.dataset.state = next;
    replaceChildren(playButton, { loading: '… Loading', playing: '❚❚ Pause', paused: '▶ Resume' }[next] || '▶ Play');
    playButton.disabled = next === 'loading';
    stopButton.disabled = !(next === 'playing' || next === 'paused' || next === 'loading');
    status.className = next === 'error' ? 'player-status error' : 'player-status';
    replaceChildren(status, message || '');
  }

  function flushLog(force) {
    if (!logDirty || !(force || log.open)) return;
    logText.textContent = lines.join('\n');
    logDirty = false;
  }

  function note(message) {
    for (const line of String(message).split('\n')) lines.push(line);
    if (lines.length > MAX_LOG_LINES) lines.splice(0, lines.length - MAX_LOG_LINES);
    logDirty = true;
  }

  async function release(current) {
    if (!current) return;
    clearInterval(current.timer);
    try { if (current.csound) await current.csound.stop(); } catch (error) { /* already stopped */ }
    try { if (current.csound) await current.csound.terminateInstance(); } catch (error) { /* gone */ }
    try { if (current.context.state !== 'closed') await current.context.close(); } catch (error) { /* closed */ }
  }

  function setLevel(value) {
    // -60 dB .. 0 dB across the width of the meter.
    const shown = value > 0 ? Math.max(0, Math.min(1, 1 + (20 * Math.log10(value)) / 60)) : 0;
    level.style.width = `${Math.round(shown * 100)}%`;
  }

  async function tick(current, mine) {
    if (current.busy || mine !== token) return;
    if (!root.isConnected) { stop(); return; } // the page moved on
    current.busy = true;
    try {
      if (state === 'playing') {
        const seconds = await current.csound.getScoreTime();
        if (mine !== token) return;
        position = seconds;
        showTime(seconds);
        current.analyser.getFloatTimeDomainData(current.samples);
        let now = 0;
        for (const sample of current.samples) now = Math.max(now, Math.abs(sample));
        if (now > peak) { peak = now; root.dataset.peak = String(Math.round(peak * 10000) / 10000); }
        setLevel(now);
      }
      flushLog(false);
    } catch (error) {
      // The performance is ending; the end event reports it.
    } finally {
      current.busy = false;
    }
  }

  async function start(from) {
    const mine = ++token;
    const previous = session;
    session = null;
    if (active && active !== api) active.stop();
    active = api;
    const offset = duration ? Math.max(0, Math.min(from, duration - 0.1)) : Math.max(0, from);
    position = offset;
    showTime(offset);
    peak = 0;
    root.dataset.peak = '0';
    setLevel(0);
    lines.length = 0;
    logDirty = true;
    setState('loading', 'Loading Csound…');
    if (!window.isSecureContext) {
      active = null;
      setState('error', 'Playing needs a secure page (HTTPS, or the local viewer on this machine). '
        + 'This page was opened over plain HTTP from another host.');
      await release(previous);
      return;
    }
    let current = null;
    try {
      // Created here, while the click still counts as a user gesture.
      current = { csound: null, context: makeContext(info.sample_rate), timer: null, busy: false };
      session = current;
      await release(previous);
      const [Csound, text] = await Promise.all([loadEngine(), fetchText(csd.path)]);
      if (mine !== token) { await release(current); return; }
      current.csound = await Csound({ audioContext: current.context });
      current.csound.removeAllListeners('message');
      current.csound.on('message', note);
      if (mine !== token) { await release(current); return; }
      const compiled = await current.csound.compileCSD(text, 1);
      if (compiled !== 0) {
        const reason = lines.find((line) => /error/i.test(line));
        throw new Error(`Csound could not compile this version${reason ? `: ${reason.trim()}` : ''}.`);
      }
      // After compiling, so that it overrides any output named in <CsOptions>.
      await current.csound.setOption('-odac');
      if (offset > 0) await current.csound.setScoreOffsetSeconds(offset);
      current.csound.once('realtimePerformanceEnded', () => {
        if (mine !== token) return; // stopped or superseded by us
        token += 1;
        session = null;
        if (active === api) active = null;
        completed += 1;
        root.dataset.completed = String(completed);
        position = 0;
        showTime(0);
        setLevel(0);
        flushLog(false);
        setState('idle', 'Finished.');
        release(current);
      });
      if (await current.csound.start() !== 0) throw new Error('Csound could not start the performance.');
      if (mine !== token) { await release(current); return; }
      if (current.context.state === 'suspended') await current.context.resume();
      const node = await current.csound.getNode();
      current.analyser = current.context.createAnalyser();
      current.analyser.fftSize = 2048;
      current.samples = new Float32Array(current.analyser.fftSize);
      node.connect(current.analyser);
      root.dataset.sampleRate = String(current.context.sampleRate);
      setState('playing', offset > 0
        ? `Playing from ${clock(offset)}. Notes that began before this point are not heard.` : '');
      current.timer = setInterval(() => tick(current, mine), TICK_MS);
    } catch (error) {
      if (mine !== token) { await release(current); return; }
      token += 1;
      session = null;
      if (active === api) active = null;
      await release(current);
      flushLog(true);
      if (lines.length) log.open = true;
      setLevel(0);
      setState('error', error && error.message ? error.message : String(error));
    }
  }

  async function stop() {
    token += 1;
    const current = session;
    session = null;
    if (active === api) active = null;
    position = 0;
    showTime(0);
    setLevel(0);
    setState('idle', '');
    flushLog(false);
    await release(current);
  }

  async function toggle() {
    if (state === 'playing') {
      const mine = token;
      try { await session.csound.pause(); } catch (error) { return; }
      if (mine === token) { setLevel(0); setState('paused', 'Paused.'); }
    } else if (state === 'paused') {
      const mine = token;
      try { await session.csound.resume(); } catch (error) { return; }
      if (mine === token) setState('playing', '');
    } else if (state !== 'loading') {
      await start(position);
    }
  }

  async function seekTo(seconds) {
    if (state === 'playing' || state === 'loading') {
      await start(seconds);
    } else {
      if (state === 'paused') await stop();
      position = seconds;
      showTime(seconds);
      setState('idle', seconds > 0 ? `Play will start from ${clock(seconds)}.` : '');
    }
  }

  playButton.addEventListener('click', toggle);
  stopButton.addEventListener('click', stop);
  seek.addEventListener('input', () => {
    dragging = true;
    replaceChildren(time, `${clock(Number(seek.value))} / ${clock(duration)}`);
  });
  seek.addEventListener('change', () => {
    dragging = false;
    seekTo(Number(seek.value));
  });
  log.addEventListener('toggle', () => flushLog(false));

  const api = { stop };
  root.dataset.completed = '0';
  root.dataset.peak = '0';
  showTime(0);
  setState('idle', '');
  return root;
}
