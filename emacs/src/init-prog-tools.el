;;; -*- lexical-binding: t; -*-

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package magit
  :commands
  (magit-clone
   magit-gitignore-globally
   magit-init
   magit-status
   magit-dispatch)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit
  :after magit
  :config

  (general-unbind
    :states '(normal visual)
    :keymaps 'magit-mode-map
    "C-w")

  ;; swap basic up/down keys for Colemak-DH
  (general-swap-key '(normal visual) 'magit-mode-map
    "C-j" "C-n"
    "C-k" "C-e"
    "g j" "g n"
    "g k" "g e"
    "j" "n"
    "k" "e"))

(use-package git-timemachine
  ;; mode key bindings provided by evil-collection
  :commands git-timemachine)

(use-package flycheck
  :hook (haskell-mode . flycheck-mode))

(add-hook 'prog-mode-hook
          '(lambda ()
             (setq show-trailing-whitespace t)))

(use-package attrap :commands attrap-attrap)

(provide 'init-prog-tools)
