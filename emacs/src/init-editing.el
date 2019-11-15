;;; -*- lexical-binding: t; -*-

;; Enables searching via * on a visual selection.
(use-package evil-visualstar
  :commands global-evil-visualstar-mode)

;; Enables inc/dec of numbers!
(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

(use-package evil-surround
  :commands global-evil-surround-mode)

(use-package evil-matchit
  :commands global-evil-matchit-mode)

(use-package evil-exchange
  :commands evil-exchange-cx-install)

(use-package avy
  :commands (avy-goto-char avy-goto-char-timer))

(use-package expand-region
  :commands (er/expand-region er/contract-region)
  :init
  (defhydra hydra-expand-region ()
     "region"
     ("e" er/expand-region "expand")
     ("n" er/contract-region "contract")))

(use-package evil-tutor
  :commands evil-tutor-start)

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-search-module 'evil-search
        evil-ex-complete-emacs-commands nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-shiftround nil
        evil-want-C-d-scroll nil
        evil-want-C-u-scroll nil
        evil-want-C-i-jump nil)
  :config

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

  ;; The following sets up my custom evil key bindings for Colemak-DH keyboard
  ;; layout. For my in-depth thoughts on the new key mappings see the readme.
  ;;
  ;; Nuke the site from orbit. It's the only way to be sure.
  ;;
  ;; Unbind all keys I either don't want or want to rebind. However, the `i'
  ;; insert mode key gets special treatment because it needs to be swapped to
  ;; get the inner bindings in visual mode automatically setup correctly.
  (general-unbind 'normal
    "&" "C-r" "J" "R" "S" "\"" "g &" "g ," "g ;" "g F" "g f" "g i" "m"
    "r" "s" "z O" "z a" "z c" "z m" "z o" "z r")
  (general-unbind 'motion
    "'" "," "/" "?" ";" "C-d" "C-u" "C-v" "C-w" "E" "F" "H" "L" "M" "N"
    "S-k" "V" "`" "e" "f" "g E" "g N" "g e" "g j" "g k" "g n" "g v"
    "h" "j" "k" "l" "n" "v" "z H" "z L" "z h" "z l")

  ;; visual state R key is not that useful.
  (general-unbind '(normal visual) "R")

  ;; swap insert mode, which will handle remapping all the visual inner
  ;; bindings.
  (general-swap-key nil '(normal visual)
    "i" "l"
    "I" "L")

  (general-def
    :states 'normal
    "&" 'evil-use-register
    "U" 'undo-tree-redo
    "V" 'evil-replace-state
    "g '" 'goto-last-change-reverse
    "g S" 'evil-find-file-at-point-with-line
    "g \"" 'goto-last-change
    "g l" 'evil-insert-resume
    "g s" 'find-file-at-point
    "k" 'evil-set-marker
    "v" 'evil-replace)

  (general-def
    :states 'motion
    "'" 'evil-repeat-find-char
    ;; Far as I understand one of the side effects of using `evil-search-module
    ;; 'evil-search' is "/" and "?" in the motion map being set to
    ;; `evil-ex-search-forward' and `evil-ex-search-backward' respectively.
    ;; Despite using `evil-search' I was running into a weird issue in Windows
    ;; where it would be set to `evil-search-forward' which would break
    ;; `evil-ex-search-next' and `evil-ex-search-previous'. So I'm setting it
    ;; here explicitly.
    "/" 'evil-ex-search-forward
    "?" 'evil-ex-search-backward
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
    "M-e" 'evil-window-top
    "M-i" 'evil-window-middle
    "M-n" 'evil-window-bottom
    "N" 'my/evil-next-line-10
    "R" 'evil-visual-line
    "S" 'evil-find-char-backward
    "\"" 'evil-repeat-find-char-reverse
    "e" 'evil-previous-line
    "f" 'evil-forward-word-end
    "g F" 'evil-backward-WORD-end
    "g f" 'evil-backward-word-end
    "g r" 'evil-visual-restore
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
  (evil-exchange-cx-install)
  (evil-collection-init))

(provide 'init-editing)
