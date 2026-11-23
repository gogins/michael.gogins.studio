// General MIDI preview.
//
// Plays a published .mid in this browser with Web Audio. There is no MIDI
// interface and no sample library: each program is a simple voice, and
// channel 10 is a short noise hit. The same page is served locally and on
// the site, so a computer and a phone hear the same preview. The Csound
// performance remains the piece.

import { fileUrl } from './catalog.js';
import { h, replaceChildren, formatDuration } from './dom.js';
import { setExtraStop, stopAllPlayers } from './player.js';
import { setStatus } from './status.js';

const TICK_MS = 100;

export function midiEntry(composition, version) {
  const files = (version.derived || []).filter((entry) => entry.kind === 'midi');
  const named = files.find((entry) => entry.file === `${composition.name}.mid`
    || entry.file === `${composition.name}.midi`);
  return named || files[0] || null;
}

export function createMidiPlayer({ id, composition, version }) {
  const file = midiEntry(composition, version);
  if (!file) {
    setExtraStop(null);
    return null;
  }

  const playButton = h('button', { type: 'button', class: 'play', 'aria-label': 'Play MIDI' }, 'Play MIDI');
  const openButton = h('button', { type: 'button', class: 'midi-open' }, 'Open MIDI');
  const stopButton = h('button', { type: 'button', class: 'stop', disabled: true }, '■ Stop');
  const time = h('span', { class: 'player-time' });
  const seek = h('input', {
    type: 'range', class: 'seek', min: 0, max: 1, step: 0.1, value: 0,
    'aria-label': 'Position in the General MIDI preview',
    title: 'Go to a point in the MIDI file. Notes that began before it are not heard.',
  });
  const status = h('p', { class: 'player-status', role: 'status' });
  const caption = h('p', { class: 'player-caption' },
    'Play MIDI previews the file in this browser. Open MIDI shows it in its folder.');
  openButton.addEventListener('click', () => { showInFolder(file, status); });
  const root = h('div', { class: 'player midi-player', id },
    h('p', { class: 'player-kind' }, 'MIDI'),
    h('div', { class: 'player-controls' }, playButton, openButton, stopButton, time),
    seek,
    caption,
    status);

  let state = 'idle';
  let position = 0;
  let duration = 0;
  let token = 0;
  let session = null;
  let sequence = null;

  function clock(seconds) {
    return formatDuration(Math.max(0, seconds)) || '0:00';
  }

  function showTime(seconds) {
    root.dataset.time = String(Math.round(seconds * 100) / 100);
    replaceChildren(time, duration ? `${clock(seconds)} / ${clock(duration)}` : clock(seconds));
    if (state !== 'playing') seek.value = String(Math.min(seconds, duration || seconds));
  }

  function setState(next, message) {
    state = next;
    root.dataset.state = next;
    replaceChildren(playButton, { loading: '… Loading', playing: '❚❚ Pause', paused: 'Play MIDI' }[next] || 'Play MIDI');
    playButton.disabled = next === 'loading';
    stopButton.disabled = !(next === 'playing' || next === 'paused' || next === 'loading');
    status.className = next === 'error' ? 'player-status error' : 'player-status';
    replaceChildren(status, message || '');
    if (message) setStatus(message);
  }

  function stop() {
    token += 1;
    const current = session;
    session = null;
    position = 0;
    showTime(0);
    setState('idle', current ? 'Stopped.' : '');
    release(current);
  }

  function release(current) {
    if (!current) return;
    clearInterval(current.timer);
    for (const node of current.nodes) {
      try { node.stop(); } catch (error) { /* already ended */ }
      try { node.disconnect(); } catch (error) { /* already gone */ }
    }
    try {
      if (current.context.state !== 'closed') current.context.close();
    } catch (error) { /* closed */ }
  }

  async function start(from) {
    const mine = ++token;
    release(session);
    session = null;
    // stopAllPlayers also stops this preview. Drop the hook for that call
    // so the performance we are about to start is not cancelled.
    setExtraStop(null);
    stopAllPlayers();
    setExtraStop(stop);
    if (mine !== token) return;

    const offset = duration ? Math.max(0, Math.min(from, Math.max(0, duration - 0.05))) : Math.max(0, from);
    position = offset;
    showTime(offset);
    setState('loading', 'Loading the MIDI file…');
    if (!window.isSecureContext) {
      setState('error', 'Playing needs a secure page (HTTPS, or the local player on this machine).');
      return;
    }
    const Context = window.AudioContext || window.webkitAudioContext;
    if (!Context) {
      setState('error', 'This browser has no Web Audio, so the MIDI file cannot be previewed.');
      return;
    }
    let current = null;
    try {
      current = { context: new Context({ latencyHint: 'playback' }), nodes: [], timer: null, origin: 0 };
      session = current;
      if (!sequence) sequence = readSmf(await fetchBytes(file.path));
      if (mine !== token) { release(current); return; }
      duration = sequence.duration;
      root.dataset.duration = String(Math.round(duration * 100) / 100);
      seek.max = String(duration || 1);
      seek.disabled = !duration;
      if (current.context.state === 'suspended') await current.context.resume();
      const master = current.context.createGain();
      master.gain.value = 0.45;
      const compressor = current.context.createDynamicsCompressor();
      compressor.threshold.value = -18;
      compressor.ratio.value = 6;
      master.connect(compressor);
      compressor.connect(current.context.destination);
      const when = current.context.currentTime + 0.05;
      current.origin = when - offset;
      for (const note of sequence.notes) {
        if (note.time < offset - 0.001) continue;
        if (note.time >= duration && duration) continue;
        schedule(current, master, note, when + (note.time - offset));
      }
      if (mine !== token) { release(current); return; }
      setState('playing', offset > 0
        ? `Playing the MIDI preview from ${clock(offset)}.`
        : 'Playing the MIDI preview.');
      current.timer = setInterval(() => tick(current, mine), TICK_MS);
    } catch (error) {
      if (mine !== token) { release(current); return; }
      token += 1;
      session = null;
      release(current);
      setState('error', error && error.message ? error.message : String(error));
    }
  }

  function tick(current, mine) {
    if (mine !== token || state !== 'playing') return;
    const seconds = current.context.currentTime - current.origin;
    if (duration && seconds >= duration) {
      token += 1;
      session = null;
      position = 0;
      showTime(0);
      setState('idle', 'Finished.');
      release(current);
      return;
    }
    showTime(Math.max(0, seconds));
    if (state === 'playing') seek.value = String(Math.min(Math.max(0, seconds), duration || seconds));
  }

  async function toggle() {
    if (state === 'playing' && session) {
      try { await session.context.suspend(); } catch (error) { return; }
      setState('paused', 'Paused.');
      return;
    }
    if (state === 'paused' && session) {
      try { await session.context.resume(); } catch (error) { return; }
      setState('playing', 'Playing the MIDI preview.');
      return;
    }
    if (state !== 'loading') await start(position);
  }

  playButton.addEventListener('click', () => { toggle(); });
  stopButton.addEventListener('click', () => { stop(); });
  seek.addEventListener('change', () => {
    const seconds = Number(seek.value);
    if (state === 'playing' || state === 'loading') start(seconds);
    else {
      if (state === 'paused') stop();
      position = seconds;
      showTime(seconds);
    }
  });

  setExtraStop(stop);
  root.dataset.state = 'idle';
  root.dataset.time = '0';
  showTime(0);
  return root;
}

function schedule(current, master, note, when) {
  const voice = voiceFor(note);
  const gain = current.context.createGain();
  const panner = current.context.createStereoPanner();
  panner.pan.value = Math.max(-1, Math.min(1, ((note.pan ?? 64) - 64) / 63));
  gain.connect(panner);
  panner.connect(master);
  const peak = Math.max(0.0002, voice.level * (note.velocity / 127));
  const end = when + note.duration;
  const attack = Math.min(voice.attack, note.duration * 0.4);
  const release = Math.min(voice.release, note.duration);
  gain.gain.setValueAtTime(0.0001, when);
  gain.gain.exponentialRampToValueAtTime(peak, when + attack);
  gain.gain.setValueAtTime(peak, Math.max(when + attack, end - release));
  gain.gain.exponentialRampToValueAtTime(0.0001, end);
  if (voice.noise) {
    const source = noiseSource(current.context, note);
    source.connect(gain);
    source.start(when);
    source.stop(end + 0.02);
    current.nodes.push(source);
  } else {
    const osc = current.context.createOscillator();
    osc.type = voice.type;
    osc.frequency.value = 440 * Math.pow(2, (note.key - 69) / 12);
    osc.connect(gain);
    osc.start(when);
    osc.stop(end + 0.02);
    current.nodes.push(osc);
  }
  current.nodes.push(gain, panner);
}

function voiceFor(note) {
  if (note.channel === 9) return { noise: true, level: 0.35, attack: 0.005, release: 0.08 };
  const family = Math.floor((note.program || 0) / 8);
  const types = ['sine', 'triangle', 'sawtooth', 'square', 'triangle', 'sine', 'sawtooth', 'square'];
  const short = family === 1 || family === 3 || family === 14 || family === 15;
  const long = family === 5 || family === 6 || family === 11;
  return {
    noise: false,
    type: types[family % types.length],
    level: family === 4 ? 0.28 : 0.18,
    attack: short ? 0.005 : 0.02,
    release: short ? 0.12 : long ? 0.4 : 0.18,
  };
}

function noiseSource(context, note) {
  const length = Math.max(1, Math.floor(context.sampleRate * 0.2));
  const buffer = context.createBuffer(1, length, context.sampleRate);
  const data = buffer.getChannelData(0);
  for (let index = 0; index < length; index += 1) data[index] = Math.random() * 2 - 1;
  const source = context.createBufferSource();
  source.buffer = buffer;
  source.playbackRate.value = Math.pow(2, (note.key - 60) / 24);
  return source;
}

async function showInFolder(file, status) {
  const local = location.hostname === 'localhost' || location.hostname === '127.0.0.1';
  if (!local) {
    replaceChildren(status, 'Open MIDI shows the file in its folder from the Player on this Mac.');
    setStatus('Open MIDI shows the file in its folder from the Player on this Mac.');
    return;
  }
  try {
    const response = await fetch('/api/v1/reveal', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ path: file.path }),
    });
    const body = await response.json().catch(() => ({}));
    if (!response.ok) {
      throw new Error(body.error || (
        response.status === 501
          ? 'Open MIDI needs studio serve. This server cannot show the folder.'
          : `HTTP ${response.status}`
      ));
    }
    replaceChildren(status, 'Finder is showing the file in its folder.');
    setStatus('Finder is showing the file in its folder.');
  } catch (error) {
    const message = error && error.message ? error.message : String(error);
    replaceChildren(status, message);
    setStatus(message);
  }
}

async function fetchBytes(path) {
  const response = await fetch(fileUrl(path), { cache: 'no-cache' });
  if (!response.ok) throw new Error(`${path}: HTTP ${response.status}`);
  return new Uint8Array(await response.arrayBuffer());
}

export function readSmf(bytes) {
  if (bytes.length < 14 || ascii(bytes, 0, 4) !== 'MThd') {
    throw new Error('This is not a Standard MIDI File.');
  }
  const headerLength = u32(bytes, 4);
  const format = u16(bytes, 8);
  const tracks = u16(bytes, 10);
  const division = u16(bytes, 12);
  if (division & 0x8000) throw new Error('This MIDI file uses SMPTE time, which the preview does not play.');
  if (format > 1) throw new Error('This MIDI file is not format 0 or 1.');
  let cursor = 8 + headerLength;
  const events = [];
  let seq = 0;
  for (let track = 0; track < tracks && cursor + 8 <= bytes.length; track += 1) {
    if (ascii(bytes, cursor, 4) !== 'MTrk') break;
    const length = u32(bytes, cursor + 4);
    const end = cursor + 8 + length;
    let index = cursor + 8;
    let tick = 0;
    let status = 0;
    while (index < end) {
      const delta = readVlq(bytes, index);
      index = delta.index;
      tick += delta.value;
      if (index >= end) break;
      const byte = bytes[index];
      if (byte === 0xFF) {
        const kind = bytes[index + 1];
        const size = readVlq(bytes, index + 2);
        index = size.index + size.value;
        if (kind === 0x2F) break;
        if (kind === 0x51 && size.value === 3) {
          const at = size.index;
          events.push({
            tick, seq: seq++, kind: 'tempo',
            us: (bytes[at] << 16) | (bytes[at + 1] << 8) | bytes[at + 2],
          });
        }
        continue;
      }
      if (byte === 0xF0 || byte === 0xF7) {
        const size = readVlq(bytes, index + 1);
        index = size.index + size.value;
        continue;
      }
      if (byte & 0x80) {
        status = byte;
        index += 1;
      }
      const needed = (status & 0xF0) === 0xC0 || (status & 0xF0) === 0xD0 ? 1 : 2;
      const data = [bytes[index] || 0, bytes[index + 1] || 0];
      index += needed;
      const kind = status & 0xF0;
      const channel = status & 0x0F;
      if (kind === 0x90 || kind === 0x80) {
        events.push({
          tick, seq: seq++, channel, key: data[0], velocity: data[1],
          kind: kind === 0x90 && data[1] ? 'on' : 'off',
        });
      } else if (kind === 0xC0) {
        events.push({ tick, seq: seq++, kind: 'program', channel, program: data[0] });
      } else if (kind === 0xB0 && data[0] === 10) {
        events.push({ tick, seq: seq++, kind: 'pan', channel, pan: data[1] });
      }
    }
    cursor = end;
  }
  events.sort((a, b) => a.tick - b.tick || a.seq - b.seq);
  const tempos = events.filter((event) => event.kind === 'tempo');
  if (!tempos.length || tempos[0].tick !== 0) tempos.unshift({ tick: 0, us: 500000 });
  const ppq = division || 480;
  const program = new Array(16).fill(0);
  const pan = new Array(16).fill(64);
  const open = new Map();
  const notes = [];
  for (const event of events) {
    if (event.kind === 'program') program[event.channel] = event.program;
    else if (event.kind === 'pan') pan[event.channel] = event.pan;
    else if (event.kind === 'on') {
      const id = event.channel * 128 + event.key;
      open.set(id, event);
    } else if (event.kind === 'off') {
      const id = event.channel * 128 + event.key;
      const started = open.get(id);
      if (!started) continue;
      open.delete(id);
      const time = ticksToSeconds(started.tick, tempos, ppq);
      const end = ticksToSeconds(event.tick, tempos, ppq);
      notes.push({
        time, duration: Math.max(0.02, end - time), key: started.key,
        velocity: started.velocity, channel: started.channel,
        program: program[started.channel], pan: pan[started.channel],
      });
    }
  }
  for (const started of open.values()) {
    const time = ticksToSeconds(started.tick, tempos, ppq);
    notes.push({
      time, duration: 0.5, key: started.key, velocity: started.velocity,
      channel: started.channel, program: program[started.channel], pan: pan[started.channel],
    });
  }
  notes.sort((a, b) => a.time - b.time);
  const duration = notes.reduce((end, note) => Math.max(end, note.time + note.duration), 0);
  return { notes, duration };
}

function ticksToSeconds(tick, tempos, ppq) {
  let seconds = 0;
  let last = 0;
  let us = 500000;
  for (const point of tempos) {
    if (point.tick >= tick) break;
    seconds += ((point.tick - last) * us) / ppq / 1e6;
    last = point.tick;
    us = point.us;
  }
  return seconds + ((tick - last) * us) / ppq / 1e6;
}

function ascii(bytes, offset, length) {
  let text = '';
  for (let index = 0; index < length; index += 1) text += String.fromCharCode(bytes[offset + index]);
  return text;
}

function u16(bytes, offset) {
  return (bytes[offset] << 8) | bytes[offset + 1];
}

function u32(bytes, offset) {
  return ((bytes[offset] << 24) | (bytes[offset + 1] << 16) | (bytes[offset + 2] << 8) | bytes[offset + 3]) >>> 0;
}

function readVlq(bytes, index) {
  let value = 0;
  for (;;) {
    if (index >= bytes.length) throw new Error('The MIDI file is truncated.');
    const byte = bytes[index];
    index += 1;
    value = (value << 7) | (byte & 0x7F);
    if (!(byte & 0x80)) return { value, index };
  }
}
