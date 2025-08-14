# Computer Music Studio Workflow
## Michael Gogins

Use a consistent toolkit on both macOS and Linux. This should be as small as possible. New code should not be written, except as actual pieces, or to expand CsoundAC or cloud-5 for the express purpose of writing new pieces.

 - The toolkit should target C++, Csound, Python, and NW.js; and, for writing words, LaTeX.

 - The studio should include Visual Studio Code, Audacity, ffmpeg, sox, Reaper, MuseScore, and TexLive. Any number of plugins for these tools is permitted.

 - From my own software, the studio should include Csound, csound-ac including the playpen.py utility, cloud-5, csound-wasm, csound-node, csound-vst3-opcodes, and  csound-cxx-opcodes.

 - Each new piece should be started in its own subdirectory of michael.gogins.studio/music. The title of the piece, the name of this subdirectory, the basename of the composition code, and the basename of the output soundfile should be exactly the same. The filepath of the composition file should be embedded in the metadata of the output soundfile. As far as possible, the piece should consist of one source code file (.csd, .html, .py, .cpp).
 
 - Without exception, every piece should have a semantic version, e.g. not Pianissimo.RPP but Pianissimo-1.0.0.RPP. All files required to render the piece should be kept in the same subdiretory, e.g. `.vstpreset` files, and may also need versioned filenames. Only new major versions of a piece, representing a substantial change in its musical content, need to go in their own new subdirectory.

 - When a piece is finished, it should be post-processed using `playpen.py` to produce soundfiles for different media outlets. All prior versions and non-essential working materials for the piece should be deleted or moved to a subdirectory of the `music/attic` subdirectory. Then the subdirectory of the piece should be moved to the `michael.gogins.studio/music/finished directory`.

 - For cloud-5 and NW.js pieces, a symbolic link to the piece should be created in the `cloud-5` directory, so that the piece can be run from there without having to copy lots of things to the piece directory.

 - Pieces that are not good enough should be moved to their own subdirectory of `michael.gogins.studio/music/attic`.