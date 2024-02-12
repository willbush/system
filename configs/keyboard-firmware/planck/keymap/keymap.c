#include QMK_KEYBOARD_H

#ifdef AUDIO_ENABLE
#    include "muse.h"
#endif

enum planck_layers { _COLEMAK, _QWERTY, _LOWER, _RAISE, _HYPER, _FN, _NUM, _ADJUST };

enum planck_keycodes { COLEMAK = SAFE_RANGE, QWERTY };

#define LOWER MO(_LOWER)
#define RAISE MO(_RAISE)
#define HYPER MO(_HYPER)
#define FN MO(_FN)
#define NUM TT(_NUM)

// Left Ctrl when held, Escape when tapped.
#define CTL_ESC LCTL_T(KC_ESC)
// Right Ctrl when held, quote when tapped.
#define CTL_QOT RCTL_T(KC_QUOT)
// Right shift when held, Enter when tapped.
#define SFT_ENT RSFT_T(KC_ENT)

// See the readme file at the root of this repository for easier to view tables
// and description of these layouts.
// clang-format off
const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  [_COLEMAK] = LAYOUT_planck_grid(
     KC_TAB, KC_Q,    KC_W,    KC_F,    KC_P,    KC_B,    KC_J,    KC_L,    KC_U,    KC_COMM, KC_SCLN, KC_BSPC,
    CTL_ESC, KC_A,    KC_R,    KC_S,    KC_T,    KC_G,    KC_M,    KC_N,    KC_E,    KC_I,    KC_O,    CTL_QOT,
    KC_LSFT, KC_Z,    KC_X,    KC_C,    KC_D,    KC_V,    KC_K,    KC_H,    KC_Y,    KC_DOT,  KC_SLSH, SFT_ENT,
    XXXXXXX, NUM,     XXXXXXX, KC_LGUI, LOWER,   HYPER,   KC_SPC,  RAISE,   FN,      XXXXXXX, XXXXXXX, XXXXXXX
  ),
  [_QWERTY] = LAYOUT_planck_grid(
     KC_TAB, KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_BSPC,
    CTL_ESC, KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, CTL_QOT,
    KC_LSFT, KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,    KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, SFT_ENT,
    XXXXXXX, NUM,     XXXXXXX, KC_LGUI, LOWER,   HYPER,   KC_SPC,  RAISE,   FN,      XXXXXXX, XXXXXXX, XXXXXXX
  ),
  [_LOWER] = LAYOUT_planck_grid(
    _______, KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    _______,
    KC_LCTL, KC_TILD, KC_LPRN, KC_EXLM, KC_EQL,  KC_RPRN, KC_LCBR, KC_PERC, KC_AMPR, KC_RCBR, KC_PIPE, KC_DEL,
    _______, XXXXXXX, KC_LT,   KC_GT,   XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
    XXXXXXX, XXXXXXX, XXXXXXX, KC_LALT, _______, _______, _______, _______, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX
  ),
  [_HYPER] = LAYOUT_planck_grid(
    _______, KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    _______,
    KC_LCTL, KC_GRV,  KC_LBRC, KC_PLUS, KC_MINS, KC_RBRC, KC_CIRC, KC_ASTR, KC_HASH, KC_DLR,  KC_BSLS, KC_DEL,
    _______, XXXXXXX, KC_LT,   KC_GT,   KC_UNDS, XXXXXXX, XXXXXXX, KC_AT,   XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
    XXXXXXX, XXXXXXX, XXXXXXX, KC_LALT, _______, _______, _______, _______, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX
  ),
  [_RAISE] = LAYOUT_planck_grid(
    _______, KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    _______,
    KC_LCTL, XXXXXXX, KC_HOME, KC_PGUP, KC_PGDN, KC_END,  KC_LEFT, KC_DOWN, KC_UP,   KC_RGHT, XXXXXXX, KC_DEL,
    _______, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
    XXXXXXX, XXXXXXX, XXXXXXX, KC_LALT, _______, _______, _______, _______, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX
  ),
  [_FN] = LAYOUT_planck_grid(
    XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, KC_F1,  KC_F2,    KC_F3,   KC_F4,   XXXXXXX, XXXXXXX,
    KC_LCTL, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, KC_F5,  KC_F6,    KC_F7,   KC_F8,   XXXXXXX, KC_DEL,
    _______, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, KC_F9,  KC_F10,   KC_F11,  KC_F12,  XXXXXXX, XXXXXXX,
    XXXXXXX, XXXXXXX, XXXXXXX, KC_LALT, _______, _______, _______, _______, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX
  ),
  [_NUM] = LAYOUT_planck_grid(
    XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, KC_7,    KC_8,    KC_9,   KC_MINS, _______,
    XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, KC_4,    KC_5,    KC_6,   KC_PLUS, XXXXXXX,
    XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, KC_1,    KC_2,    KC_3,   XXXXXXX, XXXXXXX,
    XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, _______, KC_0,    XXXXXXX, KC_DOT, XXXXXXX, XXXXXXX
  ),
  [_ADJUST] = LAYOUT_planck_grid(
    XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
    XXXXXXX, XXXXXXX, AU_NEXT, AU_ON,   AU_OFF,  XXXXXXX, XXXXXXX, COLEMAK, QWERTY,  XXXXXXX, XXXXXXX, XXXXXXX,
    XXXXXXX, AU_PREV, MU_NEXT, MU_ON,   MU_OFF,  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, DB_TOGG, XXXXXXX,
    XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, _______, _______, _______, _______, XXXXXXX, XXXXXXX, QK_BOOT, XXXXXXX
  )
};
// clang-format on

layer_state_t layer_state_set_user(layer_state_t state) {
    return update_tri_layer_state(state, _LOWER, _RAISE, _ADJUST);
}

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    switch (keycode) {
        case QWERTY:
            if (record->event.pressed) {
                set_single_persistent_default_layer(_QWERTY);
            }
            return false;
            break;
        case COLEMAK:
            if (record->event.pressed) {
                set_single_persistent_default_layer(_COLEMAK);
            }
            return false;
            break;
    }
    return true;
}

/* https://docs.qmk.fm/#/feature_audio?id=music-mask */
bool music_mask_user(uint16_t keycode) {
    switch (keycode) {
        case RAISE:
        case LOWER:
        case HYPER:
        case NUM:
        case FN:
            return false;
        default:
            return true;
    }
}
