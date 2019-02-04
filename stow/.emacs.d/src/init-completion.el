;;; -*- lexical-binding: t; -*-

(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

(use-package counsel
  :after ivy
  :bind (("C-c C-r" . ivy-resume))
  :config
  ;; make mnemonic alias for how I want to bind it
  (defalias 'my/counsel-rg-directory 'counsel-rg)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (setq counsel-git-cmd "rg --files"
        counsel-grep-base-command
          "rg --column --line-number --no-heading --smart-case --no-ignore --hidden --follow --color never %s %s"
        counsel-rg-base-command
          "rg --column --line-number --no-heading --smart-case --no-ignore --hidden --follow --color never %s ."))

;; Used by Ivy to sort commands by frequency.
(use-package smex
  :hook (after-init . smex-initialize)
  :config
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

;; increasing recentf max items for better ivy-switch-buffer completion
(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 1000
        recentf-max-saved-items 1000))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.3))

(use-package projectile
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))

(use-package hydra)

(provide 'init-completion)
