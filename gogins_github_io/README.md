# Computer Music Resources

Michael Gogins<br>
http://michaelgogins.tumblr.com<br>
michael /dot/ gogins /at/ gmail /dot/ com

This Web site contains various resources for computer and electroacoustic musicians. All of these resources are tools that I have created for the purpose of composing my own music and that I have made publicly available here. These resources include tools for composing with Csound and using HTML5 with Csound; example pieces; a playable version of the Csound reference manual; Lisp code written by [Drew Krause](http://www.drew-krause.com/) that can be used for algorithmic composition with Csound; and a prototype "front end," CHSound, for composing pieces that use HTML5 with Csound.

## Talks

On Monday 26 March 2018, I gave a talk, [Algorithmic Composition (with Csound and HTML5](https://gogins.github.io/live_talks/algorithmic_composition.html), at the Universidad Nacional de Quilmes. This is a "live talk" that can be viewed online, and in which some of the example Csound pieces will run in the browser using WebAssembly.

## Silencio Library for Algorithmic Composition

The Silencio library in JavaScript (formerly in Lua) has moved to the [csound/silencio](https://github.com/gogins/gogins.github.io/tree/master/csound/silencio) subdirectory of this repository from https://github.com/gogins/silencio. That repository will remain in place, but all futher development of the JavaScript code will be done in this repository.

The <a href="https://github.com/gogins/gogins.github.io/tree/master/csound/silencio/patches">`csound/silencio/patches`</a> directory contains a library of self-contained Csound instrument defintions, a Python script for testing them and normalizing their loudness, and a system for spatializing audio based on the work of <a href='http://www.sonicarchitecture.de/en/index_janja.html'>Jan Jacob Hofmann</a>.

## Playable Version of the Csound Reference Manual

The following link will take you to an online version of the Csound Reference Manual. It contains examples that will play right in your browser. You can even edit the examples and play them again hear the results of your changes. 

https://gogins.github.io/csound/html/indexframes.html

Not all examples are playable at this time. Only csd based examples (not orc based) that output stereo audio, and do not use file-based resources (such as sample soundfiles) or network-based resources (e.g. OSC does not work), are currently working.

## Playable Example Pieces Using Csound for WebAssembly

This repository contains samples of integrating HTML5 capabilities with Csound, where the WebAssembly build of Csound is used, so that Csound itself does not need to be installed. These examples should play on current desktop Web browsers. Note that WebAssembly is supposed to replace all other means of running "native" code in Web browsers.

<a href="https://gogins.github.io/csound/minimal.html">https://gogins.github.io/csound/minimal.html</a>

<a href="https://gogins.github.io/csound/Scrims_wasm.html">https://gogins.github.io/csound/Scrims_wasm.html</a>

<a href="https://gogins.github.io/csound/Unperformed_experiments_wasm.html">https://gogins.github.io/csound/Unperformed_experiments_wasm.html</a>

## Example Pieces Using csound.node

### Getting Started with csound.node

The examples in this category require the use of csound.node, a csound "addon" for [NW.js](http://nwjs.io/)

1. Clone this repository using ```git clone --recursive https://github.com/gogins/gogins.github.io.git```.

2. Install NW.js.

3. Build or install csound.node following [these instructions](https://github.com/csound/csound/tree/develop/frontends/nwjs).

4. To run a piece using csound.node, you must provide the piece in the form of an HTML file, along with an "application manifest" named ```package.json```. Rather than writing a new manifest for each piece, it is easier to copy the piece to a file named `csound_node_run.html` and use the manifest [here](https://gogins.github.io/csound/package.json).

In my text editor, SciTE, I provide the following command in my user options file for running any piece in NW.js using this one manifest:

<pre>
command.name.8.*=Run as NW.js application
# Rather than rewrite the package.json manifest for each file to run, we use a
# static manifest, and we copy the file to run to the name in the manifest.
command.8.*=cd $(FileDir) && cp -f $(FilePath) csound_node_run.html && /home/mkg/nwjs-sdk-v0.17.6-linux-x64/nw .
</pre>

### Examples

I performed this piece for the 2016 New York City Electroacoustic Music Festival. It is an example of the use of the new parametric Lindenmayer system class along with chord transformations:

<a href="https://www.dropbox.com/s/nkcubcw3jwe3nqt/Sevier.6.html" type="text/plain"><b>Sevier</b></a>.

In order to view the code, either download the file, or right-click to view the frame source.

Here is a variant of Sevier reworked for PNaCl:

https://gogins.github.io/csound/Lindenmayer_Example_pnacl.html

Here is a variant of Sevier that uses csound.node with Silencio to provide a 3-dimensional piano roll score display:

https://gogins.github.io/csound/Lindenmayer_Example_node.html.

And a variant of Scrims reworked for csound.node:

https://gogins.github.io/csound/Scrims_node.html

## Abjad

<a href="http://abjad.mbrsi.org/">Abjad</a> is a Python library for notating music in standard Western music notation.
It is like LaTeX for music, but offers easier programmability.

https://gogins.github.io/csound/abjad_csound.py is a Python script that renders
<i><b>Cantus in Memory of Benjamin Britten (1980)</i></b> by Arvo Part, an example score from the Abjad
repository, using an embedded Csound orchestra.

In the Csound instruments, the MIDI file velocities are rescaled using the `ampmidid` opcode for a smaller dynamic range, and the resulting amplitudes
are further rescaled by dividing by the number of active instrument instances using the `active` opcode. Finally, the
`mididefault` opcode is used to turn on the instruments for indefinite operation until the MIDI off message is received, but
to turn on and off normally according to p3 for non-MIDI performance. This enables the exact same instrument definitions to be
used for both MIDI and non-MIDI performance.

In the near future I will post an original composition done in this way. I feel this approach is worth investigating because I have experimented
with various methods of notating computer music (such as Fomus) and found them either not useful, or not maintained.

## Nudruz

This archive of Lisp code from [Drew Krause](http://www.drew-krause.com/) is hosted with his permission. The code has been lightly edited to make it more usable with Steel Bank Common Lisp and the current version of Csound. Drew's code also is licensed under the terms of the GNU Lesser General Public License.

## CHSound

The ```csound_html5``` directory used to contains a prototype program written with the Qt SDK and its QtWebEngine browser with a JavaScript interface to Csound, for editing and performing pieces that use JavaScript and HTML5 to compose and Csound to render audio. This program has graduated to being a real program and is now hosted at https://github.com/gogins/csound-extended.
