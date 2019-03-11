;;; -*- lexical-binding: t; -*-

(use-package ivy
  :pin melpa-stable
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

(use-package counsel
  :pin melpa-stable
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
  :pin melpa-stable
  :hook (after-init . smex-initialize)
  :config
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

;; increasing recentf max items for better ivy-switch-buffer completion
(use-package recentf
  :pin melpa-stable
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 1000
        recentf-max-saved-items 1000))

(use-package company
  :pin melpa-stable
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2))

(use-package projectile
  :pin melpa-stable
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :pin melpa-stable
  :after projectile
  :config
  (counsel-projectile-mode))

(use-package hydra
  :pin melpa-stable)

(use-package fd-dired :commands fd-dired
  :pin melpa-stable)

(provide 'init-completion)
