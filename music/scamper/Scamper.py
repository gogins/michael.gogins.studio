'''
This piece is to use more percussive instruments and to move considerably 
faster, with a more marked harmonic structure. Potential instruments to use:

-- Banded WG, but has clicks at onsets.
-- DelayedPlucked, only a few clicks.
-- BassModel, low notes click.
-- FaustBubble.
-- FaustModularBody.
-- FaustTurenas.
-- FMBell.
-- FM_Clang.
-- FMModerate2.
-- FMModeratedChorus.
-- FMWaterBell.
-- Guitar, but kind of clicky.
-- Harpsichord.
-- HeavyMetal.
-- LivingstonGuitar, I messed it up but I will fix it.
-- Fluidsynth.
-- Pianoteq.
-- Plucked.
-- Rhodes.
-- STKPlucked.
-- TerrainMappedBass.
-- TubularBell.
-- WGPluck.
-- Xing.

'''


orc = '''
sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 1

#include "FaustBubble.inc"
#include "FaustTurenas.inc"
#include "FMWaterBell.inc"
#include "Plucked.inc"
#include "HeavyMetal.inc"
#include "FM_Clang.inc"
#include "TubularBell.inc"
#include "DelayedPlucked.inc"

#include "MVerb.inc"
#include "MasterOutput.inc"

gk_FMWaterBell_level init -12
gk_HeavyMetal_level init -60
gk_FaustTurenas_level init -18.
gk_Plucked_level init 24.
gk_FM_Clang_level init 24.
gk_MasterOutput_level init -24.

connect "FaustBubble",  "outleft",  "MVerb",        "inleft"
connect "FaustBubble",  "outright", "MVerb",        "inright"
connect "FaustTurenas",        "outleft",  "MVerb",        "inleft"
connect "FaustTurenas",        "outright", "MVerb",        "inright"
connect "FMWaterBell",       "outleft",  "MVerb",        "inleft"
connect "FMWaterBell",       "outright", "MVerb",        "inright"
connect "Plucked",       "outleft",  "MVerb",        "inleft"
connect "Plucked",       "outright", "MVerb",        "inright"
connect "HeavyMetal",      "outleft",  "MVerb",        "inleft"
connect "HeavyMetal",      "outright", "MVerb",        "inright"
connect "FM_Clang",       "outleft",  "MVerb",        "inleft"
connect "FM_Clang",       "outright", "MVerb",        "inright"
connect "Blower",       "outleft",  "MVerb",        "inleft"
connect "Blower",       "outright", "MVerb",        "inright"
connect "TubularBell",       "outleft",  "MVerb",        "inleft"
connect "TubularBell",       "outright", "MVerb",        "inright"
connect "MVerb",        "outleft",  "MasterOutput", "inleft"
connect "MVerb",        "outright", "MasterOutput",	"inright"

alwayson "MVerb"
alwayson "MasterOutput"

'''

import CsoundAC
import math

model = CsoundAC.MusicModel()
lindenmayer = CsoundAC.ChordLindenmayer()
print("lindenmayer: {}".format(lindenmayer))

score_model = CsoundAC.ScoreModel()
lindenmayer = CsoundAC.ChordLindenmayer();
lindenmayer.setAxiom("(seed P 3928394)(= P 60)(= Sc Cmajor {0,2,4,5,7,9,11})(++ C)(= C {0,4,7,11})(= M {0,4,7,11})(= N[d] 3.)(Sc P)(A)")
#lindenmayer.addRule("(A)", "(A)(W N)(+ N[k] 20 R)(C Sd 5 R)(W C R)([)(+ N[k] 7)(F N 1)(A)(])(uni V 1 24)(uni N[x] 0 1)(+ N[t] 1)(W C R)(C Sd 3)(uni N[i] 1 5)(M Sc 3 0)(Sc P)(W N)(A)(- N[k] 2)(+ N[t] 1)(W N)(A)")
#lindenmayer.addRule("(A)", "(A)(W C R)(+ N[k] 2 R)(C Sd 3 R)(W Cl R)([)(+ N[k] 7 R)(F N .1)(A)(])(M Sc 4 0)(uni V 1 24)(uni N[x] 0 1)(+ N[t] 1)(T C 5 O)(W C R)(C Sd 3)(uni N[i] 1 5)(M Sc 3 0)(Sc P)(W N R)(A)(- N[k] 2 R)(+ N[t] 1)(W N R)(A)")
lindenmayer.addRule("(A)", "(A)(W C R)(+ N[k] 2 R)(+ N[v] 2)(+ Sd 2 R)(W Cl R)([)(* S[t] .9)(+ N[k] 4 R)(F N .1)(A)(])(M Sc 3 0)(uni V 1 24)(uni N[x] 0 1)(+ N[t] 1)(- N[v] 2)(T C 5 O)(W C R)(+ Sd 3)(uni N[i] 1 5)(M Sc 3 0)(Sc P)(W N R)(A)(- N[k] 3 R)(+ N[t] 1)(W N R)(A)")
lindenmayer.setIterationCount(6)
#print("lindenmayer: " + lindenmayer)
rescale = CsoundAC.Rescale()
rescale.setRescale(CsoundAC.Event.TIME, True, False, .02, 0.)
rescale.setRescale(CsoundAC.Event.INSTRUMENT, True, True, 1., 5.999)
rescale.setRescale(CsoundAC.Event.KEY, True, False, 24., 72.)
rescale.setRescale(CsoundAC.Event.VELOCITY, True, True, 50., 18.)
CsoundAC.System.setMessageLevel(15)
rescale.addChild(lindenmayer)
score_model.addChild(rescale)
score_model.generate()
score = score_model.getScore()
score.setDuration(6 * 60)
sco = score.getCsoundScore(12, False)
print("sco:")
print(sco)

##############################################################################
# TEMPLATE CODE BEGINS
# Assumptions: 
# 1. The playpen.ini file in the home directory exists and is correct.
# 2. A Csound orchestra has been defined in the global orc string.
# 3. A Csound score has been defined in the global sco string.
# 4. All suitable widgets in main_window have exactly the IDs and 
#    names of Csound control channels, which are created using chnexport in 
#    the Csound orchestra. All such nanes and ids begin with 'gk', 'gi', 
#    or 'gS'.
##############################################################################

import datetime
import inspect
import io
import logging
import os
import sys
import traceback
import warnings

warnings.filterwarnings("ignore")
logging.getLogger().setLevel(logging.DEBUG)

def log_print(message):
    # Get the previous frame in the stack, otherwise it would
    # be this function!!!
    caller = inspect.currentframe().f_back.f_code
    #~ # Dump the message + the name of this function to the log.
    logging.debug("%s in %s:%i %s" % (
        caller.co_name, 
        caller.co_filename, 
        caller.co_firstlineno,
        message, 
    ))
    
def log_exception(message):
    # Get the previous frame in the stack, otherwise it would
    # be this function!!!
    caller = inspect.currentframe().f_back.f_code
    # Dump the message + the name of this function to the log.
    logging.exception("%s in %s:%i %s" % (
        caller.co_name, 
        caller.co_filename, 
        caller.co_firstlineno,
        message, 
    ))

import ctcsound
csound = ctcsound.Csound()
csound_is_performing = False
log_print("Global Csound instance: {} CSOUND *: 0x{:x}.".format(csound, int(csound.csound())))
    
import gi
gi.require_version('Gdk', '3.0')
from gi.repository import Gdk
from gi.repository import GObject
from gi.repository import GLib

# Read user settings.
settings = GLib.KeyFile.new()
home_directory = os.environ["HOME"]
playpen_ini_filepath = os.path.join(home_directory, "playpen.ini")
GLib.KeyFile.load_from_file(settings, playpen_ini_filepath, GLib.KeyFileFlags.NONE)
metadata_author = settings.get_value("metadata", "author")
metadata_publisher = settings.get_value("metadata", "publisher")
metadata_year = settings.get_value("metadata", "year")
metadata_notes = settings.get_value("metadata", "notes")
metadata_license=settings.get_value("metadata", "license")
csound_audio_output = settings.get_value("csound", "audio-output")
print("csound_audio_output: " + csound_audio_output)
soundfile_editor=settings.get_value("playpen", "soundfile-editor")
gnome_theme=settings.get_value("playpen", "gnome-theme")
editor_scheme = settings.get_value("playpen", "editor-scheme")

gi.require_version("Gtk", "3.0")
from gi.repository import Gtk 

# Override some global Gnome settings with playpen.ini values.
gnome_settings = Gtk.Settings.get_default()
gnome_settings.set_property("gtk-theme-name", gnome_theme)

piece_filepath = sys.argv[0]
piece_basename = os.path.splitext(piece_filepath)[0]
ui_filepath = piece_basename + ".ui"
ui_channels_filepath = ui_filepath + ".channels"
output_soundfile_filepath = piece_filepath + ".wav"
piece_archived_csd_filepath = piece_filepath + ".csd"
log_print("piece_filepath:              {}".format(piece_filepath))
log_print("ui_filepath:                 {}".format(ui_filepath))
log_print("ui_channels_filepath:        {}".format(ui_channels_filepath))
log_print("output_soundfile_filepath:   {}".format(output_soundfile_filepath))
log_print("piece_archived_csd_filepath: {}".format(piece_archived_csd_filepath))
widgets_for_channels = dict()
values_for_channels = dict()

def create_csd_text(options, license, orc, sco):
    string_file = io.StringIO()
    string_file.write("<CsoundSynthesizer>\n")
    string_file.write("<CsOptions>\n")
    string_file.write(options)
    string_file.write("\n")
    string_file.write("</CsOptions>\n")
    string_file.write("<CsLicense>\n")
    string_file.write(license)
    string_file.write("</CsLicense>\n")
    string_file.write("<CsInstruments>\n")
    string_file.write(orc)
    string_file.write("\n")
    string_file.write("; Preset channel values:\n\n")
    global values_for_channels
    for name, value in values_for_channels.items():
        if isinstance(value, str):
            line = '{} init "{}"\n'.format(name, value)
        else:
            line = "{} init {}\n".format(name, value)
        string_file.write(line)
    string_file.write("</CsInstruments>\n")
    string_file.write("<CsScore>\n")
    string_file.write(sco)
    string_file.write("\n")
    string_file.write("</CsScore>\n")
    string_file.write("</CsoundSynthesizer>\n")
    return string_file.getvalue()

def on_destroy(source):
    try:
        csound.stop()
        csound.cleanup()
        csound.reset()
        Gtk.main_quit()
    except:
        log_exception("Shutting down.")
    
def save_ui(button = None):
    try:
        global ui_channels_filepath
        global widgets_for_channels
        global values_for_channels
        log_print("ui_channels_filepath: {}".format(ui_channels_filepath))
        log_print("widgets_for_channels size: {}".format(len(widgets_for_channels)))
        with open(ui_channels_filepath, "w") as file:
            for channel, widget in widgets_for_channels.items():
                channel_value = get_control_value(widget)
                values_for_channels[channel] = channel_value
                log_print("channel: {} value: {}".format(widget.get_name(), channel_value))
                if isinstance(channel_value, str):
                    file.write('{} init "{}"\n'.format(widget.get_name(), channel_value))
                else:
                    file.write("{} init {}\n".format(widget.get_name(), channel_value))
    except:
        log_exception("Failed to save UI.")
    
def on_play_button_clicked(button):
    try:
        global csound_is_performing
        global piece_archived_csd_filepath
        csound_is_performing = False
        csd_text = create_csd_text("-+msg_color=0 -d -m195 -f -RWo" + csound_audio_output, "", orc, sco)
        with open(piece_archived_csd_filepath, "w", encoding="utf-8") as csd_file:
            csd_file.write(csd_text)
            log_print("Archived csd: {}.".format(piece_archived_csd_filepath))
        csound.stop()
        csound.cleanup()
        csound.reset()
        csound.compileCsdText(csd_text)
        csound.start()
        load_ui()
        log_print("Restoring {} channels...".format(len(values_for_channels)))
        for name, value in values_for_channels.items():
            log_print("initialize channel: {} value {} {}".format(name, value, type(value)))
            if isinstance(value, str):
                csound.setStringChannel(name, value)
            else:
                csound.setControlChannel(name, value)
        csound_is_performing = True
        if start_time_entry:
            start_time = float(start_time_entry.get_text())
            log_print("start_time: {}".format(start_time))
        else:
            start_time = 0
        kperiod_count = 0
        while csound.performKsmps() == 0:
            kperiod_count = kperiod_count + 1
            # Keep the UI responsive during performance.
            Gtk.main_iteration_do(False)
            if start_time > 0:
                csound.setScoreOffsetSeconds(start_time)
                start_time = 0
            if kperiod_count % 100:
                score_seconds = csound.scoreTime()
                if score_time_label:
                    score_time_label.set_text("{0:9.6f} / {1}".format(score_seconds, datetime.timedelta(score_seconds/86400.)))
    except:
        print(traceback.format_exc())
        
def post_process():
    try:
        global piece_filepath
        global output_soundfile_filepath
        cwd = os.getcwd()
        print('cwd:                    ' + cwd)
        author = metadata_author #'Michael Gogins'
        year = metadata_year #'2021'
        license = metadata_license #'ASCAP'
        publisher = metadata_publisher #'Irreducible Productions, ASCAP'
        notes = metadata_notes #'Electroacoustic Music'

        directory, basename = os.path.split(piece_filepath)
        rootname = os.path.splitext(basename)[0].split('.')[0]
        soundfile_name = output_soundfile_filepath
        title = rootname.replace("-", " ").replace("_", " ")
        label = '{} -- {}'.format(author, title).replace(" ", "_")
        master_filename = '{}.normalized.wav'.format(label)
        spectrogram_filename = '%s.png' % label
        cd_quality_filename = '%s.cd.wav' % label
        mp3_filename = '%s.mp3' % label
        mp4_filename = '%s.mp4' % label
        flac_filename = '%s.flac' % label
        print('Basename:               ' + basename)
        print('Original soundfile:     ' + soundfile_name)
        print('Author:                 ' + author)
        print('Title:                  ' + title)
        print('Year:                   ' + year)
        str_copyright          = 'Copyright %s by %s' % (year, author)
        print('Copyright:              ' + str_copyright)
        print('Licence:                ' + license)
        print('Publisher:              ' + publisher)
        print('Notes:                  ' + notes)
        print('Master filename:        ' + master_filename)
        print('Spectrogram filename:   ' + spectrogram_filename)
        print('CD quality filename:    ' + cd_quality_filename)
        print('MP3 filename:           ' + mp3_filename)
        print('MP4 filename:           ' + mp4_filename)
        print('FLAC filename:          ' + flac_filename)
        bext_description       = notes
        bext_originator        = author
        bext_orig_ref          = basename
        #bext_umid              = xxx
        #bext_orig_date         = xxx
        #bext_orig_time         = xxx
        #bext_coding_hist       = xxx
        #bext_time_ref          = xxx
        str_comment            = notes
        str_title              = title
        str_artist             = author
        str_date               = year
        str_license            = license
        sox_normalize_command = '''sox -S "%s" "%s" gain -n -3''' % (soundfile_name, master_filename + 'untagged.wav')
        print('sox_normalize command:  ' + sox_normalize_command)
        os.system(sox_normalize_command)
        tag_wav_command = '''sndfile-metadata-set "%s" --bext-description "%s" --bext-originator "%s" --bext-orig-ref "%s" --str-comment "%s" --str-title "%s" --str-copyright "%s" --str-artist  "%s" --str-date "%s" --str-license "%s" "%s"''' % (master_filename + 'untagged.wav', bext_description, bext_originator, bext_orig_ref, str_comment, str_title, str_copyright, str_artist, str_date, str_license, master_filename)
        print('tag_wav_command:        ' + tag_wav_command)
        os.system(tag_wav_command)
        sox_spectrogram_command = '''sox -S "%s" -n spectrogram -o "%s" -t"%s" -c"%s"''' % (master_filename, spectrogram_filename, label, str_copyright + ' (%s' % publisher)
        print('sox_spectrogram_command:' + sox_spectrogram_command)
        os.system(sox_spectrogram_command)
        sox_cd_command = '''sox -S "%s" -b 16 -r 44100 "%s"''' % (master_filename, cd_quality_filename + 'untagged.wav')
        print('sox_cd_command:         ' + sox_cd_command)
        os.system(sox_cd_command)
        tag_wav_command = '''sndfile-metadata-set "%s" --bext-description "%s" --bext-originator "%s" --bext-orig-ref "%s" --str-comment "%s" --str-title "%s" --str-copyright "%s" --str-artist  "%s" --str-date "%s" --str-license "%s" "%s"''' % (cd_quality_filename + 'untagged.wav', bext_description, bext_originator, bext_orig_ref, str_comment, str_title, str_copyright, str_artist, str_date, str_license, cd_quality_filename)
        print('tag_wav_command:        ' + tag_wav_command)
        os.system(tag_wav_command)
        mp3_command = '''lame --add-id3v2 --tt "%s" --ta "%s" --ty "%s" --tn "%s" --tg "%s"  "%s" "%s"''' % (title, "Michael Gogins", year, notes, "Electroacoustic", master_filename, mp3_filename)
        print('mp3_command:            ' + mp3_command)
        os.system(mp3_command)
        sox_flac_command = '''sox -S "%s" "%s"''' % (master_filename, flac_filename)
        print('sox_flac_command:       ' + sox_flac_command)
        os.system(sox_flac_command)
        mp4_command = '''%s -r 1 -i "%s" -i "%s" -codec:a aac -strict -2 -b:a 384k -c:v libx264 -b:v 500k "%s"''' % ('ffmpeg', os.path.join(cwd, spectrogram_filename), os.path.join(cwd, master_filename), os.path.join(cwd, mp4_filename))
        mp4_metadata =  '-metadata title="%s" ' % title
        mp4_metadata += '-metadata date="%s" ' % year
        mp4_metadata += '-metadata genre="%s" ' % notes
        mp4_metadata += '-metadata copyright="%s" ' % str_copyright
        mp4_metadata += '-metadata composer="%s" ' % author
        mp4_metadata += '-metadata artist="%s" ' % author
        mp4_metadata += '-metadata publisher="%s" ' % publisher
        mp4_command = '''"%s" -y -loop 1 -framerate 2 -i "%s" -i "%s" -c:v libx264 -preset medium -tune stillimage -crf 18 -codec:a aac -strict -2 -b:a 384k -r:a 48000 -shortest -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" %s "%s"''' % ('ffmpeg', os.path.join(cwd, spectrogram_filename), os.path.join(cwd, master_filename), mp4_metadata, os.path.join(cwd, mp4_filename))
        mp4_command = mp4_command.replace('\\', '/')
        print('mp4_command:            ' + mp4_command)
        os.system(mp4_command)
        os.system('del *wavuntagged.wav')
        os.system('{} {}'.format(soundfile_editor, master_filename))
        print("")
    except:
        print(traceback.format_exc())
        
def on_render_button_clicked(button):
    try:
        global piece_filepath
        global values_for_channels
        global csound_is_performing
        csound_is_performing = False
        csd_text = create_csd_text("-+msg_color=0 -d -m195 -f -RWo" + output_soundfile_filepath, "", orc, sco)
        csound.stop()
        csound.cleanup()
        csound.reset()
        csound.compileCsdText(csd_text)
        csound.start()
        load_ui()
        log_print("Restoring {} channels...".format(len(values_for_channels)))
        for name, value in values_for_channels.items():
            log_print("initialize channel: {} value {} {}".format(name, value, type(value)))
            if isinstance(value, str):
                csound.setStringChannel(name, value)
            else:
                csound.setControlChannel(name, value)
        csound_is_performing = True
        if start_time_entry:
            start_time = float(start_time_entry.get_text())
            log_print("start_time: {}".format(start_time))
        else:
            start_time = 0
        kperiod_count = 0
        while csound.performKsmps() == 0:
            kperiod_count = kperiod_count + 1
            # Keep the UI responsive during performance.
            Gtk.main_iteration_do(False)
            if start_time > 0:
                csound.setScoreOffsetSeconds(start_time)
                start_time = 0
            if kperiod_count % 100:
                score_seconds = csound.scoreTime()
                if score_time_label:
                    score_time_label.set_text("{0:9.6f} / {1}".format(score_seconds, datetime.timedelta(score_seconds/86400.)))
        csound.stop()
        csound.cleanup()
        csound.reset()
        post_process()
    except:
        print(traceback.format_exc())
        
def on_stop_button_clicked(button):
    try:
        global csound_is_performing
        csound_is_performing = False
        csound.stop()
        csound.cleanup()
        csound.reset()
        print("Csound has been stopped and reset.")
    except:
        print(traceback.format_exc())
        
def get_control_value(control):
    channel_value = 0
    if isinstance(control, Gtk.Switch):
        channel_value = control.get_state()
    elif isinstance(control, Gtk.ComboBox):
        channel_value = control.get_active_id()
    elif isinstance(control, Gtk.ToggleButton):
        channel_value = control.get_active()
    elif isinstance(control, Gtk.Scale):
        channel_value = control.get_value()
    #~ elif isinstance(control, Gtk.SpinButton):
        #~ channel_value = control.get_value()
    elif isinstance(control, Gtk.Editable):
        channel_value = control.get_text()
    #log_print("control: {} value: {}".format(control.get_name(), channel_value))
    return channel_value
    
def set_control_value(control, value):
    value = value.strip().replace('"', '')
    #log_print("control: {}{} value: {}".format(control.get_name(), type(control), value))
    if isinstance(control, Gtk.Switch):
        control.set_state(float(value))
    elif isinstance(control, Gtk.ComboBox):
        result = control.set_active_id(value)
    elif isinstance(control, Gtk.ToggleButton):
        control.set_active(float(value))
    elif isinstance(control, Gtk.Scale):
        control.set_value(float(value))
    #~ elif isinstance(control, Gtk.SpinButton):
        #~ channel_value = control.get_value()
    elif isinstance(control, Gtk.Editable):
        control.set_text(value)
         
# Please note, the order of conditions matters; some subclasses do 
# not handle superclass signals.

def on_control_change(control, data=-1 ,user_data=None):
    try:
        global values_for_channels
        global csound_is_performing
        global csound
        channel_name = control.get_name()
        channel_value = get_control_value(control)
        #log_print("channel: {} value: {}".format(channel_name, channel_value))
        # Prevent premature definition of control channels.
        if csound_is_performing == False:
            pass
        else:
            if isinstance(control, Gtk.ToggleButton):
                #log_print("ToggleButton:  setControlChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setControlChannel(channel_name, channel_value)
            elif isinstance(control, Gtk.ComboBox):
                channel_value = control.get_active_id()
                #log_print("Combo box:     SetStringChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setStringChannel(channel_name, channel_value)
            elif isinstance(control, Gtk.Button):
                channel_value = float(data)
                #log_print("Button:        setControlChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setControlChannel(channel_name, channel_value)
            elif isinstance(control, Gtk.MenuItem):
                channel_value = data
                #log_print("MenuItem:      setControlChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setControlChannel(channel_name, channel_value)
            elif isinstance(control, Gtk.Scale):
                #log_print("Scale:         setControlChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setControlChannel(channel_name, channel_value)
            #~ elif isinstance(control, Gtk.SpinButton):
                #~ channel_value = control.get_value()
                #~ csound.SetControlChannel(channel_name, channel_value)
            elif isinstance(control, Gtk.Editable):
                #channel_value = control.get_text()
                log_print("Editable:      SetStringChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setStringChannel(channel_name, channel_value)
        values_for_channels[channel_name] = channel_value
    except:
        print(traceback.format_exc())
        
'''
For only those widgets and those signals that are used here to control Csound 
performances using the Csound control channels, connect the on_control_changed 
signal to its callback. Also, associate the actual widget with its name and 
its current value.
'''
def connect_controls(container):
    global widgets_for_channels
    global values_for_channels
    # A rare GTK stupidity, all widgets should have get_children, none should 
    # have get_child.
    children = []
    try:
        children = container.get_children()
    except:
        try:
            child = container.get_child()
            children.append(child)
        except:
            pass
    for child in children:
        log_print("child: {} {}".format(child, child.get_name()))
        channel_name = child.get_name()
        # Valid channels start with gk, gi, or gS.
        if channel_name[:2] not in ["gk", "gi", "gS"]:
            pass # log_print("  {} is not a Csound control channel, skipping...".format(channel_name))
        else:
            channel_value = get_control_value(child)
            if isinstance(child, Gtk.ComboBox):
                child.connect("changed", on_control_change, 1.)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.Button):
                child.connect("pressed", on_control_change, 1.)
                child.connect("released", on_control_change, 0.)            
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.MenuItem):
                child.connect("select", on_control_change, 1.)  
                child.connect("deselect", on_control_change, 0.)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.Scale):
                handler_id = child.connect("value-changed", on_control_change)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.ScaleButton):
                child.connect("value-changed", on_control_change, -1.)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.Switch):
                child.connect("state-set", on_control_change, -1.)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.Editable):
                child.connect("activate", on_control_change, -1.)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.SpinButton):
                child.connect("value-changed", on_control_change, -1)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
        connect_controls(child)                

def load_ui(source=None):
    try:
        global ui_filepath
        global ui_channels_filepath
        global widgets_for_channels
        global values_for_channels
        log_print("Loading UI: {}".format(ui_filepath))
        if os.path.exists(ui_filepath) == True:
            with open(ui_filepath, "r") as file:
                ui_text = file.read()
            result = builder.add_from_string(ui_text)
            main_window = builder.get_object("main_window")
            log_print("main_window: {}".format(main_window))
            log_print("widgets_for_channels size: {}".format(len(widgets_for_channels)))
            if os.path.exists(ui_channels_filepath) == True:
                with open(ui_channels_filepath, "r", encoding="utf-8") as file:
                    for line in file:
                        channel, equals, value = line.split(maxsplit=2)
                        if channel in widgets_for_channels:
                            widget = widgets_for_channels[channel]
                            if widget:
                                set_control_value(widget, value)
            else:
                log_print("UI file not found, not defining controls.")
    except:
        log_print("Error: failed to load user-defined controls layout.")
        print(traceback.format_exc())
  
start_time_entry = None
score_time_label = None
builder = Gtk.Builder()
try:
    builder.add_from_file(ui_filepath)
    main_window = builder.get_object("main_window")
    main_window.connect("destroy", on_destroy)
    save_button = builder.get_object("save_button")
    save_button.connect("clicked", save_ui)
    restore_button = builder.get_object("restore_button")
    restore_button.connect("clicked", load_ui)
    play_button = builder.get_object("play_button")
    play_button.connect("clicked", on_play_button_clicked)
    render_button = builder.get_object("render_button")
    render_button.connect("clicked", on_render_button_clicked)
    stop_button = builder.get_object("stop_button")
    stop_button.connect("clicked", on_stop_button_clicked)
    start_time_entry = builder.get_object("start_time_entry")
    score_time_label = builder.get_object("score_time")
    level_slider = builder.get_object("gk_MasterOutput_level")
    connect_controls(main_window)
    main_window.show_all() 
    load_ui()
    Gtk.main()
except:
    if len(sys.argv) > 1 and sys.argv[1] == "--render":
        on_render_button_clicked(None)
    else:
        on_play_button_clicked(None)
    
