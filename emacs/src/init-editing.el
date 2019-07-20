;;; -*- lexical-binding: t; -*-

;; For an in-depth discussion about why I'm rebinding Evil keys see the readme.
(defun my/enable-custom-evil-keys ()
  "Sets up my custom evil key bindings for Colemak-DH keyboard layout"
  (interactive)

  (setq evil-want-C-u-scroll nil
        evil-want-C-d-scroll nil)

  ;;These unbound keys likely have their functionality mapped onto another key
  (general-unbind '(normal visual motion)
    "C-u"
    "C-d"
    "C-r")

  (general-def
    :states 'normal
    "&" 'evil-use-register
    "U" 'undo-tree-redo
    "k" 'evil-set-marker)

  (general-def
    :states '(normal visual)
    "s" 'evil-insert
    "S" 'evil-insert-line)

  (general-def
    :states 'motion
    ;; These in Colemak-DH as what h, j, k, l keys are in Qwerty.
    "m" 'evil-backward-char
    "n" 'evil-next-line
    "e" 'evil-previous-line
    "i" 'evil-forward-char
    "M" 'evil-beginning-of-line ;; `0' remains bound to a similar function
    "N" 'evil-scroll-down
    "E" 'evil-scroll-up
    "I" 'evil-end-of-line ;; `$' remains bound to this function

    "M-e" 'evil-window-top
    "M-m" 'evil-window-middle
    "M-n" 'evil-window-bottom

    "h" 'evil-ex-search-next
    "H" 'evil-ex-search-previous
    "j" 'evil-goto-mark
    "l" 'evil-forward-word-end
    "L" 'evil-forward-WORD-end))

(defun my/enable-vanilla-evil-keys ()
  "Reset evil bindings back to default Qwerty."
  (interactive)

  (setq evil-want-C-u-scroll t
        evil-want-C-d-scroll t)

  (general-def
    :states 'normal
    "C-r" 'undo-tree-redo
    "\"" 'evil-use-register
    "m" 'evil-set-marker)

  (general-def
    :states '(normal visual)
    "i" 'evil-insert
    "I" 'evil-insert-line)

  (general-def
    :states 'motion
    "h" 'evil-backward-char
    "j" 'evil-next-line
    "k" 'evil-previous-line
    "l" 'evil-forward-char

    "C-u" 'evil-scroll-up
    "C-d" 'evil-scroll-down

    "H" 'evil-window-top
    "M" 'evil-window-middle
    "L" 'evil-window-bottom

    "n" 'evil-ex-search-next
    "N" 'evil-ex-search-previous

    "`" 'evil-goto-mark
    "e" 'evil-forward-word-end
    "E" 'evil-forward-WORD-end))

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
        evil-shift-round ni
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
