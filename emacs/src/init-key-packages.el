;;; -*- lexical-binding: t -*-

;; Packages used in bindings keys provided in a separate file for early loading.

(use-package general)
(use-package hydra)

(use-package which-key
  :defer 0.1
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (which-key-mode +1))

(defun my/custom-evil-collection-bindings (mode mode-keymaps &rest _rest)
  (cond ((eq mode 'dired)
         ;; dired key bindings
         (general-def
           :states 'normal
           :keymaps 'dired-mode-map
           ;; remove evil mode shadows
           "i" nil ;; was 'dired-toggle-read-only
           "m" nil ;; was 'dired-mark
           "j" nil ;; was 'dired-next-line
           "^" nil ;; was 'dired-up-directory
           "r" nil ;; was 'dired-do-redisplay
           "R" nil ;; was 'dired-do-rename
           ;; rebind things better to my custom evil keys
           "l" 'dired-toggle-read-only
           "k" 'dired-mark
           "n" 'dired-next-line
           "e" 'dired-previous-line
           "C-e" 'dired-up-directory
           "v" 'dired-do-rename))
        ;; default case make some blind key swaps for my custom evil keys.
        ;;
        ;; Note this does not work for `ediff-mode' because evil-collection
        ;; doesn't apply key bindings until after `ediff-startup-hook'.
        (t (evil-collection-swap-key 'normal mode-keymaps
             "m" "h" ;; left
             "n" "j" ;; down
             "e" "k" ;; up
             "i" "l" ;; right
             "r" "v" ;; range (old name visual)
             (kbd "C-n") (kbd "C-j")
             (kbd "C-e") (kbd "C-k")))))

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
          deadgrep
          debug
          dired
          disk-usage
          elfeed
          help
          info
          ivy
          man
          minibuffer
          woman))

  ;; called after evil-collection makes its keybindings
  ;; https://github.com/emacs-evil/evil-collection#key-translation
  (add-hook 'evil-collection-setup-hook #'my/custom-evil-collection-bindings))

(provide 'init-key-packages)
