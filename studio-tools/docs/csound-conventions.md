# Csound and CsoundAC conventions

## The canonical format

A composition is one complete `.csd` file with an orchestra in
`<CsInstruments>` and a score in `<CsScore>`. It is what gets rendered,
archived, published and compared. There is no other score representation
in the studio. MIDI files, MusicXML or notation may be written to
`outputs/` as derived artifacts, never as sources.

## P-fields

Michael Gogins's p-field conventions are authoritative. Every instrument
and every score in the studio uses them:

| P-field | Meaning | CsoundAC `Event` field |
| --- | --- | --- |
| p1 | Instrument number (may be fractional) | instrument |
| p2 | Time in seconds (beats under a tempo statement) | time |
| p3 | Duration in seconds (beats under a tempo statement) | duration |
| p4 | Pitch as MIDI key number (may be fractional) | key |
| p5 | Loudness as MIDI velocity (may be fractional) | velocity |
| p6 | Space, front to back (Ambisonic X) | depth |
| p7 | Space, left to right (Ambisonic Y) | pan |
| p8 | Space, bottom to top (Ambisonic Z) | height |
| p9 | Audio phase | phase |
| p10 | Pitch-class set number | pitches |
| p11 | Homogeneity (always 1 from CsoundAC) | |

An instrument reads only the p-fields it needs; a hand-written score may
stop at p5 or p7. A score written by CsoundAC always has all eleven.

The range of p6 to p8 is the instrument's business. The demonstration
orchestra of `nu` uses p7 from 0 (left) to 1 (right) with `pan2`, and p6
from 0 (near) to 1 (far) as a reverberation send.

## What was verified about CsoundAC

Verified on macOS on 12 November 2026 against the CsoundAC source at
`v9.1.1-beta-26-gd884dea62`, with the Python bindings (Python 3.12), the
WebAssembly build (Node.js 26; both a local `csound-wasm` build and the
`v4.1.2-beta` release), and the C++ library. `studio env` repeats the
Python check in whatever environment it runs in and records the result in
every manifest; the tests in `tests/test_generators.py` repeat all three.

**CsoundAC already writes the p-fields above.** No compatibility methods
are missing and nothing in CsoundAC needed to change. For the event

```
time 1.5, duration 2.5, status 144, instrument 3, key 61, velocity 71,
phase 0.11, pan 0.22, depth 0.33, height 0.44, pitches 4095
```

Python, JavaScript and C++ all write

```
i 3 1.5 2.5 61 71 0.33 0.22 0.44 0.11 4095 1
```

that is p6 depth, p7 pan, p8 height, p9 phase, p10 pitches, p11 1.

Methods used by the studio, all of which exist in `CsoundAC/Event.hpp` and
`CsoundAC/Score.hpp` and in the bindings:

| Method | Use |
| --- | --- |
| `Score.append(time, duration, status, instrument, key, velocity, phase, pan, depth, height, pitches)` | Add one note. Note the argument order: phase, pan, depth, height. It is not the p-field order. |
| `Score.getCsoundScore(tonesPerOctave, conformPitches)` | The whole score as i-statements. |
| `Event(time, duration, status, instrument, key, velocity, phase, pan, depth, height, pitches)` | One event, completely initialized. |
| `Event.toCsoundIStatement(tonesPerOctave)` | One i-statement. |

Status 144 is a MIDI note-on. `pitches` 4095 means all twelve pitch-classes.

### Pitfalls found

1. **A default-constructed `Event` is not initialized (native builds).**
   In Python, `CsoundAC.Event().toCsoundIStatement()` contained `nan` in
   2000 of 2000 trials, and `Score.append` called with only six arguments
   from JavaScript left `nan` in p6 to p10. Always give all eleven values.
   `studio env` reports this (`CsoundAC.Event() leaves fields
   uninitialized`), and `csoundScore()` in `js/csoundac-node.mjs` refuses a
   score containing `nan`. This is a defect in CsoundAC, recorded here and
   not worked around by changing CsoundAC.
2. **`tonesPerOctave` rounds keys.** With 12, key 61.5 is written as 62.
   Pass 0 to keep fractional keys.
3. **JavaScript does not supply C++ default arguments.**
   `toCsoundIStatement()` without its argument writes `nan` for the key.
   Pass every argument of every call.
4. **The WebAssembly module cannot find its `.wasm` file under Node.js.**
   The file has to be read and passed to the factory as `wasmBinary`;
   `js/csoundac-node.mjs` does this.
5. **Which Python.** CsoundAC is built for one Python version. On the
   MacBook the Homebrew `python3` (3.15) cannot import it; the virtual
   environment `~/venv/csound` (3.12) can. Set `STUDIO_PYTHON` to the
   interpreter that can; `studio run -- python ...` then uses it.

When a generating program needs any other CsoundAC method, check that it
exists first: read the header in `csound-ac/CsoundAC/`, or
`dir(CsoundAC.Score)` in Python. Prefer explicitly named methods to
overloads. Do not invent method names.

## Generating programs

A generating program lives in the composition's `code/` directory, is named
`<name>-...`, and writes the complete `<name>.csd` into its working
directory. Run it through the studio so that the command, seed and
parameters are recorded in the manifest:

```sh
studio run "$S" --seed 7 -- python code/nu-generate.py           # Python + CsoundAC
studio run "$S" --seed 7 -- node code/nu-generate.mjs            # JavaScript + CsoundAC WebAssembly
studio run "$S" --seed 7 -- studio-cxx code/nu-generate.cpp      # C++ + CsoundAC
```

`python`, `node` and `studio-cxx` are resolved by the studio
(`STUDIO_PYTHON`, `STUDIO_NODE`, `studio-tools/bin/studio-cxx`), so the
recorded command means the same thing on the MacBook and in the cloud.
The program receives:

| Variable | Value |
| --- | --- |
| `STUDIO_SEED` | The seed given with `--seed`. Use it for every random choice. |
| `STUDIO_COMPOSITION` | The composition's name. |
| `STUDIO_STAGING` | The staging directory (also the working directory). |
| `STUDIO_TOOLS` | The `studio-tools` directory, to find `js/csoundac-node.mjs`. |

Python:

```python
import os, random
import CsoundAC

random.seed(int(os.environ.get("STUDIO_SEED", "1")))
score = CsoundAC.Score()
# time, duration, status, instrument, key, velocity, phase, pan, depth, height, pitches
score.append(0.0, 2.0, 144.0, 1.0, 62.0, 80.0, 0.0, 0.5, 0.3, 0.0, 4095.0)
with open("nu.csd", "w") as output:
    output.write(HEAD + score.getCsoundScore(12.0, False) + TAIL)
```

JavaScript:

```javascript
import { writeFileSync } from 'node:fs';
import { pathToFileURL } from 'node:url';
import path from 'node:path';
const { loadCsoundAC, appendNote, csoundScore } = await import(
  pathToFileURL(path.join(process.env.STUDIO_TOOLS, 'js', 'csoundac-node.mjs')).href);
const CsoundAC = await loadCsoundAC();
const score = new CsoundAC.Score();
appendNote(score, { time: 0, duration: 2, instrument: 1, key: 62, velocity: 80, pan: 0.5, depth: 0.3 });
writeFileSync('nu.csd', HEAD + csoundScore(score) + TAIL);
```

C++ (`studio-cxx` compiles with `STUDIO_CSOUNDAC_CXXFLAGS` and
`STUDIO_CSOUNDAC_LDFLAGS`, runs the program, and discards the executable):

```cpp
#include <Score.hpp>
#include <fstream>
int main() {
    csound::Score score;
    score.append(0.0, 2.0, 144.0, 1.0, 62.0, 80.0, 0.0, 0.5, 0.3, 0.0, 4095.0);
    std::ofstream("nu.csd") << HEAD << score.getCsoundScore(12.0, false) << TAIL;
}
```

`music/ai-compositions/nu/code/nu-generate.py` is a complete example.
`studio rerender NAME --regenerate` runs the recorded commands again and
checks that they reproduce the stored `.csd`.

## Real-time score lines

`scoreline` and `scorelinei` do not treat `;` as a comment. Do not pass
comment lines to them; pass i-statements only.
