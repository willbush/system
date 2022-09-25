;;; -*- lexical-binding: t; -*-


;;
;;; Editing Variables

(setq electric-pair-pairs
  '(
    (?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\})
   ))

(electric-pair-mode t)

;; I don't want two spaces after my periods. This the affects behavior
;; of `fill-paragraph' (among other things).
(setq sentence-end-double-space nil)

;; Prevent indention inserting tabs by default.
(setq-default indent-tabs-mode nil
              tab-width 2)

;; fill-paragraph uses fill-column for the width at which to break lines
(setq-default fill-column 80)

;; Continue wrapped words at white-space, rather than in the middle of
;; a word.
(setq-default word-wrap t)

;; ...but don't do any wrapping by default. It's expensive. Enable
;; `visual-line-mode' if you want soft line-wrapping. `auto-fill-mode'
;; for hard line-wrapping.
(setq-default truncate-lines t)

;; If enabled (and `truncate-lines' was disabled), soft wrapping no
;; longer occurs when that window is less than
;; `truncate-partial-width-windows' characters wide. We don't need
;; this, and it's extra work for Emacs otherwise, so off it goes.
(setq truncate-partial-width-windows nil)

;; The POSIX standard defines a line is "a sequence of zero or more
;; non-newline characters followed by a terminating newline", so files
;; should end in a newline. Windows doesn't respect this (because it's
;; Windows), but we should, since programmers' tools tend to be POSIX
;; compliant.
(setq require-final-newline t)

;; Default to hard line-wrapping in text modes. Hard wrapping is more
;; performant, and Emacs makes it trivially easy to reflow text with
;; `fill-paragraph' and `evil-fill'.
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Cull duplicates in the kill ring to reduce bloat and make the kill
;; ring easier to peruse (with `counsel-yank-pop').
(setq kill-do-not-save-duplicates t)

;; Allow UTF or composed text from the clipboard, even in the terminal
;; or on non-X systems (like Windows or macOS), where only `STRING' is
;; used.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;
;;; Extra file extensions to support

(push '("/LICENSE\\'" . text-mode) auto-mode-alist)
(push '("\\.log\\'" . text-mode) auto-mode-alist)

;;
;;; Packages

;; Avoid performance issues in files with very long lines. Note that
;; global-so-long-mode may cause esup to error when it visits a .elc
;; file with long lines.
(use-package so-long
  :hook (after-init . global-so-long-mode))

;; Enables searching via * on a visual selection.
(use-package evil-visualstar
  :commands global-evil-visualstar-mode)

;; Enables inc/dec of numbers!
(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

;; See tutorials:
;; https://github.com/emacs-evil/evil-surround
;; https://github.com/tpope/vim-surround
(use-package evil-surround
  :commands global-evil-surround-mode)

;; Use % to jump to matching tags (no learning curve!)
(use-package evil-matchit
  :commands global-evil-matchit-mode)

;; see https://github.com/Dewdrops/evil-exchange
(use-package evil-exchange
  :commands evil-exchange-install)

(use-package evil-traces
  :after evil-ex
  :config
  (evil-traces-mode))

(use-package avy
  :commands (avy-goto-char avy-goto-char-timer))

(use-package expand-region
  :commands (er/expand-region er/contract-region)
  :init

  (defhydra hydra-expand-region ()
     "region"
     ("e" er/expand-region "expand")
     ("n" er/contract-region "contract"))

  ;; pressing r again after going into range mode (visual mode) will enter a
  ;; hydra for expand-region usage
  (general-def
    :keymaps 'evil-visual-state-map
    "r" 'hydra-expand-region/body))

(use-package evil-tutor
  :commands evil-tutor-start)

(use-package evil
  :demand t ;; demand due to general perf issues mentioned on `init-keys.el'
  :hook (after-init . evil-mode)
  :custom (evil-undo-system 'undo-redo)
  :init
  (setq-default evil-shift-width 2)

  (setq evil-ex-complete-emacs-commands nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-shiftround nil
        evil-want-C-d-scroll nil
        evil-want-C-u-scroll nil
        evil-want-C-i-jump nil)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Silence warning (:functions use-package property doesn't work for me for
  ;; some reason).
  (declare-function evil-line-move "evil-common")
  (declare-function evil-set-command-properties "evil-common")

  (evil-define-motion my/evil-next-line-10 (count)
    "Move the cursor COUNT * 10 lines down."
    :type line
    (let (line-move-visual)
      (evil-line-move (* 10 (or count 1)))))

  (evil-define-motion my/evil-previous-line-10 (count)
    "Move the cursor COUNT * 10 lines up."
    :type line
    (let (line-move-visual)
      (evil-line-move (- (* 10 (or count 1))))))

  ;; The following sets up my custom evil key bindings for Colemak-DHm keyboard
  ;; layout. For my in-depth thoughts on the new key mappings see the readme.
  ;;
  ;; Nuke the site from orbit. It's the only way to be sure.
  ;;
  ;; Unbind all keys I either don't want or want to rebind. However, the `i'
  ;; insert mode key gets special treatment because it needs to be swapped to
  ;; get the inner bindings in visual mode automatically setup correctly.
  (general-unbind 'normal
    "&" "C-r" "J" "R" "S" "\"" "g &" "g ," "g ;" "g F" "g f" "g i" "m"
    "g x" ;; Unbind explicitly despite `evil-exchange-install' stomping on this.
    "r" "s" "z O" "z a" "z c" "z m" "z o" "z r")
  (general-unbind 'motion
    "+" "-" "'" "," "/" "?" ";" "C-d" "C-u" "C-v" "C-w" "E" "F" "H" "L" "M" "N"
    "S-k" "V" "`" "e" "f" "g E" "g N" "g e" "g j" "g k" "g m" "g n" "g v"
    "h" "j" "k" "l" "n" "v" "z H" "z L" "z h" "z l"
    )

  ;; visual state R key is not that useful.
  (general-unbind '(normal visual) "R")

  ;; swap insert mode, which will handle remapping all the visual inner
  ;; bindings.
  (general-swap-key nil '(evil-normal-state-map evil-visual-state-map)
    "i" "l"
    "I" "L")

  (general-def
    :keymaps 'evil-normal-state-map
    "+" 'evil-numbers/inc-at-pt
    "-" 'evil-numbers/dec-at-pt
    "&" 'evil-use-register
    "C-i" 'evil-jump-forward
    "U" 'evil-redo
    "V" 'evil-replace-state
    "g '" 'goto-last-change-reverse
    "g S" 'evil-find-file-at-point-with-line
    "g \"" 'goto-last-change
    "g l" 'evil-insert-resume
    "g o" 'browse-url-at-point
    "g s" 'find-file-at-point
    "k" 'evil-set-marker
    "v" 'evil-replace)

  (general-def
    :keymaps 'evil-motion-state-map
    "'" 'evil-repeat-find-char
    ;; Far as I understand one of the side effects of using `evil-search-module
    ;; 'evil-search' is "/" and "?" in the motion map being set to
    ;; `evil-ex-search-forward' and `evil-ex-search-backward' respectively.
    ;; Despite using `evil-search' I was running into a weird issue in Windows
    ;; where it would be set to `evil-search-forward' which would break
    ;; `evil-ex-search-next' and `evil-ex-search-previous'. So I'm setting it
    ;; here explicitly.
    "/" 'swiper
    "?" 'swiper-backward
    "C-S-e" 'evil-scroll-up
    "C-S-n" 'evil-scroll-down
    "C-j" 'evil-join
    "C-r" 'evil-visual-block
    "E" 'my/evil-previous-line-10
    "F" 'evil-forward-WORD-end
    "H" 'evil-ex-search-previous
    "I" 'evil-end-of-line ;; `$' remains bound to this function
    "J" 'evil-goto-mark-line
    "M" 'evil-beginning-of-line ;; `0' remains bound to a similar function
    "N" 'my/evil-next-line-10
    "R" 'evil-visual-line
    "S" 'evil-find-char-backward
    "\"" 'evil-repeat-find-char-reverse
    "e" 'evil-previous-line
    "f" 'evil-forward-word-end
    "g F" 'evil-backward-WORD-end
    "g f" 'evil-backward-word-end
    "g r" 'evil-visual-restore
    "g e" 'evil-window-top
    "g m" 'evil-window-middle
    "g n" 'evil-window-bottom
    "h" 'evil-ex-search-next
    "i" 'evil-forward-char
    "j" 'evil-goto-mark
    "m" 'evil-backward-char
    "n" 'evil-next-line
    "r" 'evil-visual-char
    "s" 'evil-find-char
    "z I" 'evil-scroll-right
    "z M" 'evil-scroll-left
    "z i" 'evil-scroll-column-right
    "z m" 'evil-scroll-column-left)

  (global-evil-visualstar-mode)
  (global-evil-surround-mode 1)
  (global-evil-matchit-mode 1)
  ;; binds `evil-exchange' to "g x" and `evil-exchange-cancel' to "g X". Note
  ;; this will stomp on "g x" key binding which is `browse-url-at-point' by
  ;; default. I rebind this above.
  (evil-exchange-install)
  (evil-collection-init))

(provide 'init-editing)
