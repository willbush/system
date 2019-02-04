;;; -*- lexical-binding: t; -*-

(use-package evil
  :init
  (setq evil-search-module 'evil-search
        evil-ex-complete-emacs-commands nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-shift-round nil
        evil-want-C-u-scroll t
        ;; evil-collection will give a warning if the following setting is not set
        ;; before loading evil and evil-collection. Note that evil-leader loads evil
        ;; see: https://github.com/emacs-evil/evil-collection/issues/215
        evil-want-keybinding nil)
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
  (global-set-key (kbd "C-S-k") 'evil-window-up)

  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config
  (setq evil-collection-mode-list
        '(calendar
          compile
          deadgrep
          dired
          git-timemachine
          help
          info
          ivy
          man
          minibuffer
          (package-menu package)
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
  :commands (er/expand-region er/contract-region)
  :init
  (defhydra hydra-expand-region ()
     "region: "
     ("k" er/expand-region "expand")
     ("j" er/contract-region "contract")))

(provide 'init-editing)
