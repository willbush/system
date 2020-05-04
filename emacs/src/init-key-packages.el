;;; -*- lexical-binding: t -*-

;; Packages used in bindings keys provided in a separate file for early loading.

(use-package general)
(use-package hydra)

(use-package which-key
  :defer 0.1
  :init
  ;; Silence warning (:defer causes byte compile warnings)
  (declare-function which-key-prefix-then-key-order "which-key")
  (declare-function which-key-mode "which-key")

  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (which-key-mode +1))

;; A collection of evil key bindings for various modes
(use-package evil-collection
  :commands evil-collection-init
  :preface
  ;; evil-collection will give a warning if the following setting is not set
  ;; before loading evil and evil-collection. Note that evil-leader loads evil
  ;; see: https://github.com/emacs-evil/evil-collection/issues/215 Also even if
  ;; this is in the :init block it will still given the warning when lazy loading
  ;; evil.
  (setq evil-want-keybinding nil)
  :custom (evil-collection-setup-minibuffer t)
  :init
  (setq evil-collection-mode-list
        '(calendar
          (package-menu package)
          (pdf pdf-view)
          (term term ansi-term multi-term)
          compile
          cus-theme
          custom
          debug
          disk-usage
          elfeed
          help
          info
          ivy
          minibuffer))

  (defun my/custom-evil-collection-bindings (_mode mode-keymaps &rest _rest)
    (evil-collection-swap-key 'normal mode-keymaps
      "m" "h" ;; left
      "n" "j" ;; down
      "e" "k" ;; up
      "i" "l" ;; right
      "r" "v" ;; range (old name visual)
      (kbd "C-n") (kbd "C-j")
      (kbd "C-e") (kbd "C-k")))

  ;; called after evil-collection makes its keybindings
  ;; https://github.com/emacs-evil/evil-collection#key-translation
  (add-hook 'evil-collection-setup-hook #'my/custom-evil-collection-bindings))

(provide 'init-key-packages)
