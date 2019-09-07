#include QMK_KEYBOARD_H
#include "muse.h"

extern keymap_config_t keymap_config;

enum planck_layers {
  _COLEMAK,
  _QWERTY,
  _NUM,
  _LOWER,
  _RAISE,
  _HYPER,
  _ADJUST
};

enum planck_keycodes {
  COLEMAK = SAFE_RANGE,
  QWERTY,
};

#define LOWER MO(_LOWER)
#define RAISE MO(_RAISE)
#define HYPER MO(_HYPER)
#define NUM   TT(_NUM)

#define CTL_ESC LCTL_T(KC_ESC)
#define SFT_ENT RSFT_T(KC_ENT)

// See the readme file at the root of this repository for easier to view tables
// and description of these layouts.
const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  [_COLEMAK] = LAYOUT_planck_grid(
    KC_TAB,  KC_Q,    KC_W,    KC_F,    KC_P,    KC_B,    KC_J,    KC_L,    KC_U,    KC_Y,    KC_SCLN, KC_BSPC,
    CTL_ESC, KC_A,    KC_R,    KC_S,    KC_T,    KC_G,    KC_M,    KC_N,    KC_E,    KC_I,    KC_O,    KC_QUOT,
    KC_LSFT, KC_Z,    KC_X,    KC_C,    KC_D,    KC_V,    KC_K,    KC_H,    KC_COMM, KC_DOT,  KC_SLSH, SFT_ENT,
    KC_LCTL, NUM,     KC_LGUI, KC_LALT, LOWER,   HYPER,   KC_SPC,  RAISE,   KC_RALT, KC_RGUI, _______, KC_RCTL
  ),
  [_QWERTY] = LAYOUT_planck_grid(
    KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_BSPC,
    CTL_ESC, KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT,
    KC_LSFT, KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,    KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, SFT_ENT,
    KC_LCTL, NUM,     KC_LGUI, KC_LALT, LOWER,   HYPER,   KC_SPC,  RAISE,   KC_RALT, KC_RGUI, _______, KC_RCTL
  ),
  [_LOWER] = LAYOUT_planck_grid(
    _______, KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    _______,
    _______, KC_TILD, KC_LPRN, KC_EXLM, KC_EQL,  KC_RPRN, KC_LCBR, KC_PERC, KC_AMPR, KC_RCBR, KC_PIPE, KC_DEL,
    _______, _______, KC_LT,   KC_GT,   _______, _______, _______, _______, _______, _______, _______, _______,
    _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______
  ),
  [_HYPER] = LAYOUT_planck_grid(
    _______, KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    _______,
    _______, KC_GRV,  KC_LBRC, KC_PLUS, KC_MINS, KC_RBRC, KC_CIRC, KC_ASTR, KC_HASH, KC_DLR,  KC_BSLS, KC_DEL,
    _______, _______, KC_LT,   KC_GT,   KC_UNDS, _______, _______, KC_AT,   _______, _______, _______, _______,
    _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______
  ),
  [_RAISE] = LAYOUT_planck_grid(
    _______, KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    _______,
    _______, KC_F11,  KC_HOME, KC_PGUP, KC_PGDN, KC_END,  KC_LEFT, KC_DOWN, KC_UP,   KC_RGHT, KC_F12,  KC_DEL,
    _______, KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   KC_F6,   KC_F7,   KC_F8,   KC_F9,   KC_F10,  _______,
    _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______
  ),
  [_NUM] = LAYOUT_planck_grid(
    _______, _______, _______, _______, _______, _______, _______, KC_7,    KC_8,    KC_9,   KC_MINS, _______,
    _______, _______, _______, _______, _______, _______, _______, KC_4,    KC_5,    KC_6,   KC_PLUS, _______,
    _______, _______, _______, _______, _______, _______, _______, KC_1,    KC_2,    KC_3,   _______, _______,
    _______, _______, _______, _______, _______, _______, _______, KC_0,    XXXXXXX, KC_DOT, _______, _______
  ),
  [_ADJUST] = LAYOUT_planck_grid(
    _______, RESET,   DEBUG,   _______, _______, _______, _______, _______, _______, _______, _______, _______,
    _______, _______, MU_MOD,  AU_ON,   AU_OFF,  _______, _______, QWERTY,  COLEMAK, _______, _______, _______,
    _______, _______, _______, MU_ON,   MU_OFF,  _______, _______, _______, _______, _______, _______, _______,
    _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______
  )
};

uint32_t layer_state_set_user(uint32_t state) {
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
      return false;
    default:
      return true;
  }
}
