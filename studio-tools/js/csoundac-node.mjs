// Loads the WebAssembly build of CsoundAC in Node.js, for generating programs
// kept in a composition's code/ directory.
//
//   import { loadCsoundAC } from '<path to studio-tools>/js/csoundac-node.mjs';
//   const CsoundAC = await loadCsoundAC();
//   const score = new CsoundAC.Score();
//   score.append(time, duration, 144.0, instrument, key, velocity, phase, pan, depth, height, 4095.0);
//   const text = score.getCsoundScore(12.0, false);
//
// CsoundAC.js and CsoundAC.wasm are not part of this repository. They come
// from a csound-wasm build or release; set CSOUNDAC_WASM_DIR to the directory
// that holds both files.
//
// Two things differ from Python and must be respected (both verified):
//  - The .wasm file has to be read here and handed to the module factory as
//    `wasmBinary`; the module cannot locate it by itself under Node.
//  - JavaScript does not supply C++ default arguments. Pass every argument:
//    all 11 to Event(...) and Score.append(...), both to getCsoundScore(...),
//    and one to toCsoundIStatement(...). A missing argument becomes NaN.
//
// Run this file directly to check the installation:
//   node studio-tools/js/csoundac-node.mjs

import { createRequire } from 'node:module';
import { readFile } from 'node:fs/promises';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

export async function loadCsoundAC(directory = process.env.CSOUNDAC_WASM_DIR, options = {}) {
  if (!directory) {
    throw new Error('CSOUNDAC_WASM_DIR is not set; it must name the directory holding CsoundAC.js and CsoundAC.wasm');
  }
  const base = path.resolve(directory);
  const require = createRequire(import.meta.url);
  const factory = require(path.join(base, 'CsoundAC.js'));
  const wasmBinary = await readFile(path.join(base, 'CsoundAC.wasm'));
  const messages = [];
  const module = await factory({
    wasmBinary,
    print: (line) => messages.push(line),
    printErr: (line) => messages.push(line),
    ...options,
  });
  module.studioMessages = messages;
  return module;
}

// One complete note, in the order of the studio's p-fields:
// p1 instrument, p2 time, p3 duration, p4 MIDI key, p5 MIDI velocity,
// p6 depth, p7 pan, p8 height, p9 phase, p10 pitch-class set.
export function appendNote(score, note) {
  const value = (name, fallback) => {
    const given = note[name] === undefined ? fallback : note[name];
    if (typeof given !== 'number' || !Number.isFinite(given)) {
      throw new Error(`note.${name} must be a finite number, not ${given}`);
    }
    return given;
  };
  score.append(
    value('time'), value('duration'), 144.0, value('instrument'), value('key'), value('velocity'),
    value('phase', 0.0), value('pan', 0.0), value('depth', 0.0), value('height', 0.0), value('pitches', 4095.0),
  );
}

// The score as Csound i-statements. tonesPerOctave 12 rounds keys to whole
// semitones; 0 leaves fractional keys as they are.
export function csoundScore(score, tonesPerOctave = 12.0) {
  const text = score.getCsoundScore(tonesPerOctave, false);
  if (/\bnan\b/i.test(text)) {
    throw new Error('the score contains NaN; an event was created without all of its fields');
  }
  return text;
}

async function selfCheck() {
  const CsoundAC = await loadCsoundAC();
  const score = new CsoundAC.Score();
  appendNote(score, {
    time: 1.5, duration: 2.5, instrument: 3, key: 61, velocity: 71, phase: 0.11, pan: 0.22, depth: 0.33, height: 0.44,
  });
  const statement = csoundScore(score).trim();
  const expected = 'i 3 1.5 2.5 61 71 0.33 0.22 0.44 0.11 4095 1';
  console.log(statement);
  if (statement !== expected) {
    console.error(`unexpected p-field layout; expected: ${expected}`);
    process.exit(1);
  }
  console.log('CsoundAC WebAssembly: ok (p6 depth, p7 pan, p8 height, p9 phase, p10 pitch-class set)');
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  selfCheck().catch((error) => {
    console.error(String(error.message || error));
    process.exit(1);
  });
}
