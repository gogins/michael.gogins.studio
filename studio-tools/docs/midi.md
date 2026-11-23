# General MIDI

`studio render` writes `outputs/NAME.mid` from the Csound score at the same
time as it checks that score. Accept keeps the file with the version, and
publish copies it. The Player then shows two play buttons: **Csound**
performs the `.csd`, and **Play MIDI** previews the file in the browser.
**Open MIDI**, beside it, shows the file selected in its folder when the
Player is served on this Mac. The Mac's own menu then chooses the
application. The same page does this on a computer and on a phone, locally
and on the site.

`studio midi` rewrites that file without rendering. Use it for the current
version, or for a staging directory, when you want the MIDI file before the
next render. An archived version is left as it was.

```sh
studio render "$S"   # writes outputs/NAME.mid beside the render
studio midi "$S"     # rewrite only, before accept
studio midi NAME     # rewrite the current version
```

## What is written

Score times are seconds, which is Csound's default of tempo 60 (one beat per
second). The file is a Standard MIDI File, format 0, at 120 beats per minute
and 480 ticks per quarter note, so those seconds play back at the same length.
A `t` statement at some other tempo is warned about, and p2 is still written
as seconds.

A note needs p4 (MIDI key, 0–127) and p5 (velocity, 1–127). An event without
them, such as `i 99 0 120` for reverb, is not written. Pan is p7: 0 is left
and 1 is right, sent as MIDI controller 10.

An instrument whose number is 1–128 uses that General MIDI program. A comment
anywhere in the `.csd` overrides it. The program is the musician's number,
1 through 128 (1 is Acoustic Grand Piano):

```
; gm 1 49
; gm 10 drums
```

`drums` puts that instrument on channel 10. Other instruments use the
remaining 15 channels, in the order their notes begin. An instrument with no
usable program is skipped, and the command says so.

The browser preview is not a sample player. Each program is a simple voice
chosen so the instruments can be told apart, and channel 10 is a short noise
hit. Another application plays the opened file with its own sounds. Notes
that began before the seek point are not heard in the browser preview.
