;;; -*- lexical-binding: t; -*-

;; For an in-depth discussion about why I'm rebinding Evil keys see the readme.
(defun my/enable-custom-evil-keys ()
  "Sets up my custom evil key bindings for Colemak-DH keyboard layout"
  (interactive)

  (setq evil-want-C-u-scroll nil
        evil-want-C-d-scroll nil)

  ;; Nuke the site from orbit. It's the only way to be sure.
  ;;
  ;; Unbind all keys before rebinding them except for the `i' insert mode key,
  ;; which needs to be swapped to get the inner bindings in visual mode
  ;; automatically setup correctly. Some of these keys I don't rebind.
  (general-unbind 'normal
    "&"
    "@"
    "C-r"
    "J"
    "R"
    "S"
    "\""
    "g &"
    "g ,"
    "g ;"
    "g F"
    "g f"
    "g i"
    "m"
    "r"
    "s"
    "z O"
    "z a"
    "z c"
    "z m"
    "z o"
    "z r")


  (general-unbind 'motion
    "'"
    ","
    ";"
    "C-d"
    "C-u"
    "C-v"
    "C-w"
    "E"
    "F"
    "H"
    "L"
    "M"
    "N"
    "V"
    "`"
    "e"
    "f"
    "g E"
    "g N"
    "g e"
    "g j"
    "g k"
    "g n"
    "g v"
    "h"
    "j"
    "k"
    "l"
    "n"
    "v"
    "z H"
    "z L"
    "z h"
    "z l")

  ;; visual state R key is not that useful.
  (general-unbind '(normal visual) "R")

  ;; swap insert mode, which will handle remapping all the visual inner
  ;; bindings.
  (general-swap-key nil '(normal visual) "i" "l")
  (general-swap-key nil '(normal visual) "I" "L")

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
    "," 'evil-execute-macro
    "C-j" 'evil-join
    "C-r" 'evil-visual-block
    "E" 'evil-scroll-up
    "F" 'evil-forward-WORD-end
    "H" 'evil-ex-search-previous
    "I" 'evil-end-of-line ;; `$' remains bound to this function
    "J" 'evil-goto-mark-line
    "M" 'evil-beginning-of-line ;; `0' remains bound to a similar function
    "M-e" 'evil-window-top
    "M-i" 'evil-window-middle
    "M-n" 'evil-window-bottom
    "N" 'evil-scroll-down
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
    "z m" 'evil-scroll-column-left))

;; evil-collection will give a warning if the following setting is not set
;; before loading evil and evil-collection. Note that evil-leader loads evil
;; see: https://github.com/emacs-evil/evil-collection/issues/215 Also even if
;; this is in the :init block it will still given the warning when lazy loading
;; evil.
(setq evil-want-keybinding nil)

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-search-module 'evil-search
        evil-ex-complete-emacs-commands nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-shift-round nil
        evil-want-C-u-scroll t)
  :config
  ;; unbind evil-lookup
  (eval-after-load "evil-maps"
    (define-key evil-motion-state-map "\S-k" nil))

  ;; make mnemonic alias for how I want to bind it
  (defalias 'my/evil-search-clear-highlight 'evil-ex-nohighlight)
  ;; evil global key bindings
  (global-set-key (kbd "C-S-h") 'evil-window-left)
  (global-set-key (kbd "C-S-l") 'evil-window-right)
  (global-set-key (kbd "C-S-j") 'evil-window-down)
  (global-set-key (kbd "C-S-k") 'evil-window-up))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config
  (setq evil-collection-mode-list
        '(calendar
          (package-menu package)
          (term term ansi-term multi-term)
          compile
          cus-theme
          custom
          deadgrep
          debug
          dired
          disk-usage
          ediff
          git-timemachine
          help
          info
          ivy
          man
          minibuffer
          (pdf pdf-view)
          woman))
  (evil-collection-init))

;; Enables searching via * on a visual selection.
(use-package evil-visualstar
  :after evil
  :init (global-evil-visualstar-mode))

;; Enables inc/dec of numbers!
(use-package evil-numbers
  :after evil
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :after evil
  :config (global-evil-matchit-mode 1))

(use-package evil-exchange
  :after evil
  :config (evil-exchange-cx-install))

(use-package avy :commands avy-goto-char)

(use-package expand-region
  :after hydra
  :commands (er/expand-region er/contract-region)
  :init
  (defhydra hydra-expand-region ()
     "region: "
     ("k" er/expand-region "expand")
     ("j" er/contract-region "contract")))

(use-package evil-tutor
  :commands evil-tutor-start)

(provide 'init-editing)
