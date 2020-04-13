;;; -*- lexical-binding: t; -*-

;; increasing recentf max items for better ivy-switch-buffer completion
(use-package recentf
  :functions recentf-mode
  :init
  (setq recentf-max-menu-items 1000
        recentf-max-saved-items 1000)
  :config
  (recentf-mode 1))

(use-package hydra)

(use-package ivy
  :defer 0.1
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  (ivy-mode 1))

(use-package all-the-icons-ivy
  :after ivy
  :config
  (all-the-icons-ivy-setup))

(use-package counsel
  :after ivy
  :config
  (setq counsel-git-cmd "rg --files"
        counsel-grep-base-command
          "rg --column --line-number --no-heading --smart-case --no-ignore --hidden --follow --color never %s %s"
        counsel-rg-base-command
        "rg --column --line-number --no-heading --smart-case --no-ignore --hidden --follow --color never %s .")

  (counsel-mode 1))

(use-package company
  :defer 0.1
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-show-numbers t)

  (global-company-mode 1))

(use-package company-tabnine
  :after company
  :config
  (add-to-list 'company-backends #'company-tabnine))

(use-package counsel-projectile
  :defer 0.1
  :config
  ;; This can also be accomplished by invoking
  ;; `counsel-projectile-switch-project' then `M-o D', but I want to make it
  ;; easier.
  (defun my/counsel-projectile-switch-project-dired ()
    "Switches to a projectile project's root in dired mode."
    (interactive)
    (counsel-projectile-switch-project "D"))

  (general-def
    :keymaps 'projectile-command-map
    "P" 'my/counsel-projectile-switch-project-dired
    ;; Was bound to P. Rebind it.
    "Z" 'projectile-test-project)

  ;; `counsel-projectile-mode' enables `projectile-mode'
  (counsel-projectile-mode 1))

(use-package projectile
  :commands projectile-mode
  :config
  ;; The default uses Emacs Lisp in Windows, which way too slow for large
  ;; projects.
  (setq projectile-indexing-method 'alien
        projectile-completion-system 'ivy))

(use-package fd-dired :commands fd-dired)

(provide 'init-completion)
