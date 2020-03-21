#pragma once

#ifdef AUDIO_ENABLE
    #define STARTUP_SONG SONG(PLANCK_SOUND)

    // Using the DVORAK_SOUND below for the GAMING layer.
    #define DEFAULT_LAYER_SONGS { SONG(COLEMAK_SOUND), \
                                  SONG(QWERTY_SOUND),  \
                                  SONG(DVORAK_SOUND) \
                                }
#endif

/* enable basic MIDI features:
   - MIDI notes can be sent when in Music mode is on
*/
#define MIDI_BASIC

// Setup double tapping to toggle TT layers.
#define TAPPING_TOGGLE 2
