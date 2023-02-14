#pragma once

//#define USE_MATRIX_I2C

/* Select hand configuration */

#define MASTER_LEFT
// #define MASTER_RIGHT
// #define EE_HANDS

//#define TAPPING_FORCE_HOLD
//#define TAPPING_TERM 100

#ifdef RGBLIGHT_ENABLE
    #define RGBLIGHT_EFFECT_RAINBOW_MOOD
    #define RGBLIGHT_EFFECT_RAINBOW_SWIRL
    #define RGBLIGHT_EFFECT_SNAKE
    #define RGBLIGHT_EFFECT_KNIGHT
    #define RGBLIGHT_EFFECT_CHRISTMAS
    #define RGBLIGHT_EFFECT_STATIC_GRADIENT
    #define RGBLIGHT_EFFECT_TWINKLE
    #define RGBLIGHT_LIMIT_VAL 120
    #define RGBLIGHT_HUE_STEP 10
    #define RGBLIGHT_SAT_STEP 17
    #define RGBLIGHT_VAL_STEP 17
#endif

#define OLED_FONT_H "keyboards/crkbd/lib/glcdfont.c"

#ifdef RGB_MATRIX_ENABLE
#   define RGB_MATRIX_KEYPRESSES // reacts to keypresses
// #   define RGB_MATRIX_KEYRELEASES // reacts to keyreleases (instead of keypresses)
#   define RGB_DISABLE_WHEN_USB_SUSPENDED // turn off effects when suspended
#   define RGB_MATRIX_FRAMEBUFFER_EFFECTS
// #   define RGB_MATRIX_LED_PROCESS_LIMIT (RGB_MATRIX_LED_COUNT + 4) / 5 // limits the number of LEDs to process in an animation per task run (increases keyboard responsiveness)
// #   define RGB_MATRIX_LED_FLUSH_LIMIT 16 // limits in milliseconds how frequently an animation will update the LEDs. 16 (16ms) is equivalent to limiting to 60fps (increases keyboard responsiveness)
#    define RGB_MATRIX_MAXIMUM_BRIGHTNESS 150 // limits maximum brightness of LEDs to 150 out of 255. Higher may cause the controller to crash.
#    define RGB_MATRIX_HUE_STEP 8
#    define RGB_MATRIX_SAT_STEP 8
#    define RGB_MATRIX_VAL_STEP 8
#    define RGB_MATRIX_SPD_STEP 10

/* Enable the animations you want/need.  You may need to enable only a small number of these because       *
 * they take up a lot of space.  Enable and confirm that you can still successfully compile your firmware. */
// RGB Matrix Animation modes. Explicitly enabled
// For full list of effects, see:
// https://docs.qmk.fm/#/feature_rgb_matrix?id=rgb-matrix-effects
/* #    define ENABLE_RGB_MATRIX_ALPHAS_MODS */
/* #    define ENABLE_RGB_MATRIX_GRADIENT_UP_DOWN */
/* #    define ENABLE_RGB_MATRIX_GRADIENT_LEFT_RIGHT */
/* #    define ENABLE_RGB_MATRIX_BREATHING */
/* #    define ENABLE_RGB_MATRIX_BAND_SAT */
/* #    define ENABLE_RGB_MATRIX_BAND_VAL */
/* #    define ENABLE_RGB_MATRIX_BAND_PINWHEEL_SAT */
/* #    define ENABLE_RGB_MATRIX_BAND_PINWHEEL_VAL */
/* #    define ENABLE_RGB_MATRIX_BAND_SPIRAL_SAT */
/* #    define ENABLE_RGB_MATRIX_BAND_SPIRAL_VAL */
/* #    define ENABLE_RGB_MATRIX_CYCLE_ALL */
/* #    define ENABLE_RGB_MATRIX_CYCLE_LEFT_RIGHT */
/* #    define ENABLE_RGB_MATRIX_CYCLE_UP_DOWN */
/* #    define ENABLE_RGB_MATRIX_RAINBOW_MOVING_CHEVRON */
/* #    define ENABLE_RGB_MATRIX_CYCLE_OUT_IN */
/* #    define ENABLE_RGB_MATRIX_CYCLE_OUT_IN_DUAL */
/* #    define ENABLE_RGB_MATRIX_CYCLE_PINWHEEL */
/* #    define ENABLE_RGB_MATRIX_CYCLE_SPIRAL */
/* #    define ENABLE_RGB_MATRIX_DUAL_BEACON */
/* #    define ENABLE_RGB_MATRIX_RAINBOW_BEACON */
/* #    define ENABLE_RGB_MATRIX_RAINBOW_PINWHEELS */
/* #    define ENABLE_RGB_MATRIX_RAINDROPS */
/* #    define ENABLE_RGB_MATRIX_JELLYBEAN_RAINDROPS */
/* #    define ENABLE_RGB_MATRIX_HUE_BREATHING */
/* #    define ENABLE_RGB_MATRIX_HUE_PENDULUM */
/* #    define ENABLE_RGB_MATRIX_HUE_WAVE */
#    define ENABLE_RGB_MATRIX_PIXEL_RAIN
// enabled only if RGB_MATRIX_FRAMEBUFFER_EFFECTS is defined
#    define ENABLE_RGB_MATRIX_TYPING_HEATMAP
#    define ENABLE_RGB_MATRIX_DIGITAL_RAIN
// enabled only of RGB_MATRIX_KEYPRESSES or RGB_MATRIX_KEYRELEASES is defined
#    define ENABLE_RGB_MATRIX_SOLID_REACTIVE
#    define ENABLE_RGB_MATRIX_SOLID_REACTIVE_WIDE
#    define ENABLE_RGB_MATRIX_SOLID_REACTIVE_MULTIWIDE
#    define ENABLE_RGB_MATRIX_SOLID_REACTIVE_CROSS
#    define ENABLE_RGB_MATRIX_SOLID_REACTIVE_MULTICROSS
#    define ENABLE_RGB_MATRIX_SOLID_REACTIVE_NEXUS
#    define ENABLE_RGB_MATRIX_SOLID_REACTIVE_MULTINEXUS
#    define ENABLE_RGB_MATRIX_SPLASH
#    define ENABLE_RGB_MATRIX_MULTISPLASH
#    define ENABLE_RGB_MATRIX_SOLID_SPLASH
#    define ENABLE_RGB_MATRIX_SOLID_MULTISPLASH
#endif
