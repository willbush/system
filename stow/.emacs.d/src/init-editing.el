;;; -*- lexical-binding: t; -*-

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-search-module 'evil-search
        evil-ex-complete-emacs-commands nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-shift-round nil
        evil-want-C-u-scroll t
        ;; set to nil as required by evil-collection
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
  (global-set-key (kbd "C-S-k") 'evil-window-up))

;; Enables searching via * on a visual selection.
(use-package evil-visualstar
  :after evil
  :init (global-evil-visualstar-mode))

;; Enables inc/dec of numbers!
(use-package evil-numbers
  :after evil
  :config
  ;; Bind increment and decrement number at point.
  (global-set-key (kbd "M-=") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "M--") 'evil-numbers/dec-at-pt))

;; Bind zoom in and out
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

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

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config
  (setq evil-collection-mode-list
        '(calendar
          dired
          minibuffer
          woman
          man
          ivy
          deadgrep))
  (evil-collection-init))

(use-package avy :commands avy-goto-char)

(provide 'init-editing)
