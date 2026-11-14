// Does a .csd compile and perform in the Csound WebAssembly engine that the
// viewer plays with?
//
//   node csound-browser-check.mjs ENGINE.js FILE.csd [TIME_LIMIT_SECONDS]
//
// ENGINE.js is the @csound/browser module (docs/studio/csound/csound.js).
// The piece is compiled and performed from beginning to end without audio
// output, as fast as the engine goes. One line is printed:
//
//   STUDIO-BROWSER-CHECK {"compiled": true, "performed": true, ...}
//
// "realtime_factor" is the performed duration divided by the time it took
// here; the same engine in a phone's browser is several times slower.
// The exit status is 0 whenever the check itself ran; read the result.

import { readFileSync } from 'node:fs';
import { pathToFileURL } from 'node:url';

const [enginePath, csdPath, limitText] = process.argv.slice(2);
if (!enginePath || !csdPath) {
  process.stderr.write('usage: node csound-browser-check.mjs ENGINE.js FILE.csd [TIME_LIMIT_SECONDS]\n');
  process.exit(2);
}
const timeLimit = Number(limitText) > 0 ? Number(limitText) : 3600;

// Csound's messages arrive through the console; keep them out of our output.
const messages = [];
const collect = (...parts) => {
  for (const line of parts.map(String).join(' ').split('\n')) {
    if (messages.length < 20000) messages.push(line);
  }
};
for (const method of ['log', 'info', 'warn', 'error', 'debug']) console[method] = collect;

// The module is built for browsers and looks for these two names.
globalThis.self ??= globalThis;
globalThis.window ??= globalThis;

const result = { compiled: false, performed: false };
const finish = () => {
  const errors = messages.filter((line) => /\b(error|INIT ERROR|PERF ERROR)\b/.test(line)
    && !/^\s*0 errors in performance/.test(line) && !/overall (amps|samples)/.test(line));
  result.errors = errors.slice(0, 12).map((line) => line.trim().slice(0, 300));
  result.runtime_errors = messages.filter((line) => /\b(INIT ERROR|PERF ERROR)\b/.test(line)).length;
  result.messages_tail = messages.slice(-12).map((line) => line.slice(0, 300));
  process.stdout.write(`STUDIO-BROWSER-CHECK ${JSON.stringify(result)}\n`);
  process.exit(0);
};

try {
  const { libcsound } = await import(pathToFileURL(enginePath).href);
  const lib = await libcsound();
  const version = lib.csoundGetVersion();
  result.csound_version = `${Math.floor(version / 1000)}.${String(Math.floor((version % 1000) / 10)).padStart(1, '0')}`;
  const banner = messages.find((line) => /Csound version/.test(line));
  if (banner) result.banner = banner.replace(/^-+/, '').trim();
  const commit = messages.map((line) => /\[commit:\s*([0-9a-f]+)\]/.exec(line)).find(Boolean);
  if (commit) result.commit = commit[1];

  const csound = lib.csoundCreate();
  const status = lib.csoundCompileCSD(csound, readFileSync(csdPath, 'utf8'), 1);
  result.compiled = status === 0;
  if (result.compiled) {
    // After compiling, so that it overrides any output named in <CsOptions>.
    lib.csoundSetOption(csound, '-n');
    result.started = lib.csoundStart(csound) === 0;
    if (result.started) {
      const ksmps = lib.csoundGetKsmps(csound);
      result.sample_rate = lib.csoundGetSr(csound);
      result.ksmps = ksmps;
      result.channels = lib.csoundGetNchnls(csound);
      const began = performance.now();
      let frames = 0;
      let blocks = 0;
      let ended = false;
      while (!ended) {
        ended = lib.csoundPerformKsmps(csound) !== 0;
        if (!ended) frames += ksmps;
        blocks += 1;
        if (blocks % 2048 === 0 && (performance.now() - began) / 1000 > timeLimit) {
          result.timed_out = true;
          break;
        }
      }
      const seconds = (performance.now() - began) / 1000;
      result.performed = ended;
      result.duration_seconds = Math.round((frames / result.sample_rate) * 1000) / 1000;
      result.wall_seconds = Math.round(seconds * 1000) / 1000;
      result.realtime_factor = seconds > 0 ? Math.round((frames / result.sample_rate / seconds) * 10) / 10 : null;
    }
  }
  try {
    lib.csoundReset(csound);
    lib.csoundDestroy(csound);
  } catch (error) {
    // The verdict is already known.
  }
} catch (error) {
  result.engine_error = String(error && error.message ? error.message : error).slice(0, 500);
}
finish();
