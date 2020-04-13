;;; -*- lexical-binding: t; -*-

(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

(use-package all-the-icons-ivy
  :after ivy
  :config
  (all-the-icons-ivy-setup))

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


;; increasing recentf max items for better ivy-switch-buffer completion
(use-package recentf
  :functions recentf-mode
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 1000
        recentf-max-saved-items 1000))

(use-package company
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-show-numbers t))

(use-package company-tabnine
  :config
  (add-to-list 'company-backends #'company-tabnine))

(use-package counsel-projectile
  ;; When this loads, projectile will also load. It would be nice if I could use
  ;; `:commands projectile-command-map'. However, commands expects a function
  ;; and not a key map. `:keymap' exists for this case, but I can't figure out
  ;; how to bind it they way I want and where I want (not here). So just load
  ;; after init.
  :hook (after-init . counsel-projectile-mode)
  :config

  ;; This can also be accomplished by invoking
  ;; `counsel-projectile-switch-project' then `M-o D', but I want to make it
  ;; easier.
  (defun my/counsel-projectile-switch-project-dired ()
    "Switches to a projectile project's root in dired mode."
    (interactive)
    (counsel-projectile-switch-project "D"))

  ;; rebind `evil-goto-definition' for Haskell mode.
  (general-def
    :keymaps 'projectile-command-map
    "P" 'my/counsel-projectile-switch-project-dired
    ;; Was bound to P. Rebind it.
    "Z" 'projectile-test-project))

(use-package projectile
  :commands projectile-mode
  :config
  (projectile-mode +1)
  ;; The default uses Emacs Lisp in Windows, which way too slow for large
  ;; projects.
  (setq projectile-indexing-method 'alien))

(use-package hydra)

(use-package fd-dired :commands fd-dired)

(provide 'init-completion)
