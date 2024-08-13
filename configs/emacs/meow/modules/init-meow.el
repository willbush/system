;;; -*- lexical-binding: t; -*-


(defconst my/meow-cheatsheet-physical-layout-planck
  "
┏━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┓
┃ TAB     │  <AD01> │  <AD02> │  <AD03> │  <AD04> │  <AD05> │  <AD06> │  <AD07> │  <AD08> │  <AD09> │  <AD10> │    BKSP ┃
┃         |         |         |         |         |         |         |         |         |         |         |         ┃
┃         ├┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┤         ┃
┃         │         │         │         │         │         │         │         │         │         │         │         ┃
┃         |         |         |         |         |         |         |         |         |         |         |         ┃
┠─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┨
┃ ESC / C │  <AC01> │  <AC02> │  <AC03> │  <AC04> │  <AC05> │  <AC06> │  <AC07> │  <AC08> │  <AC09> │  <AC10> │  <AC11> ┃
┃         |         |         |         |         |         |         |         |         |         |         |         ┃
┃         ├┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┃
┃         │         │         │         │         │         │         │         │         │         │         │         ┃
┃         |         |         |         |         |         |         |         |         |         |         |         ┃
┠─────────├─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┨
┃ SHIFT   │  <AB01> │  <AB02> │  <AB03> │  <AB04> │  <AB05> │  <AB06> │  <AB07> │  <AB08> │  <AB09> │  <AB10> │     RET ┃
┃         |         |         |         |         |         |         |         |         |         |         |         ┃
┃         ├┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┤         ┃
┃         │         │         │         │         │         │         │         │         │         │         │         ┃
┃         |         |         |         |         |         |         |         |         |         |         |         ┃
┗━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┛

")

(defconst my/meow-cheatsheet-layout-colemak-dhm
  '((<AD01> "q" "Q")
    (<AD02> "w" "W")
    (<AD03> "f" "F")
    (<AD04> "p" "P")
    (<AD05> "b" "B")
    (<AD06> "j" "J")
    (<AD07> "l" "L")
    (<AD08> "u" "U")
    (<AD09> "," "<")
    (<AD10> ";" ":")
    (<AC01> "a" "A")
    (<AC02> "r" "R")
    (<AC03> "s" "S")
    (<AC04> "t" "T")
    (<AC05> "g" "G")
    (<AC06> "m" "M")
    (<AC07> "n" "N")
    (<AC08> "e" "E")
    (<AC09> "i" "I")
    (<AC10> "o" "O")
    (<AC11> "'" "\"")
    (<AB01> "z" "Z")
    (<AB02> "x" "X")
    (<AB03> "c" "C")
    (<AB04> "d" "D")
    (<AB05> "v" "V")
    (<AB06> "k" "K")
    (<AB07> "h" "H")
    (<AB08> "y" "Y")
    (<AB09> "." ">")
    (<AB10> "/" "?")))

(use-package meow
  ;; after which key so keypad uses it instaed of its own implementation
  :after which-key
  :config

  (setq meow-keypad-leader-dispatch "C-c")
  (setq meow-cheatsheet-physical-layout my/meow-cheatsheet-physical-layout-planck)
  (setq meow-cheatsheet-layout my/meow-cheatsheet-layout-colemak-dhm)

  (meow-motion-overwrite-define-key
   ;; Use e to move up, n to move down.
   ;; Since special modes usually use n to move down, we only overwrite e here.
   '("e" . meow-prev)
   '("<escape>" . ignore))

  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   ;; To execute the originally e in MOTION state, use SPC e.
   '("e" . "H-e")
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("m" . meow-left)
   '("m" . meow-left-expand)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("h" . meow-mark-word)
   '("H" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))

  (meow-global-mode 1))


(provide 'init-meow)
