#pragma once

#ifdef AUDIO_ENABLE
#define STARTUP_SONG SONG(PLANCK_SOUND)
#endif

/* enable basic MIDI features:
   - MIDI notes can be sent when in Music mode is on
*/
#define MIDI_BASIC

// Setup double tapping to toggle TT layers.
#define TAPPING_TOGGLE 2

// https://docs.qmk.fm/tap_hold
// https://precondition.github.io/home-row-mods#tap-hold-configuration-settings
#define QUICK_TAP_TERM 0
#define TAPPING_TERM 200
#define HOLD_ON_OTHER_KEY_PRESS_PER_KEY
