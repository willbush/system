;;; -*- lexical-binding: t; -*-

;; Keep track of recently opened files
(use-package recentf
  :hook (after-init . recentf-mode)
  :ensure nil ;; is included in Emacs.
  :config
  (defun my/recent-file-truename (file)
    (if (or (file-remote-p file nil t)
            (not (file-remote-p file)))
        (file-truename file)
      file))

  (setq recentf-filename-handlers
        '(substring-no-properties ;; strip out lingering text properties
          my/recent-file-truename ;; resolve symlinks of local files
          abbreviate-file-name)   ;; replace $HOME with ~
        recentf-save-file (expand-file-name "recentf" user-emacs-directory)
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 1000)

  ;; Exclude these from recentf
  (dolist
      (ex '("\\.gpg$"
            "^/scp:"
            "^/ssh:"
            "^/su:"
            "^/sudo:"))
    (add-to-list 'recentf-exclude ex))

  (add-hook 'dired-mode-hook
            (lambda ()
              (recentf-add-file default-directory)))

  (add-hook 'kill-emacs-hook #'recentf-cleanup))

(use-package ivy
  :defer 0.1
  :config
  ;; Silence warning (:defer causes byte compile warnings)
  (declare-function ivy-mode "ivy")

  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  (ivy-mode 1))

(use-package all-the-icons-ivy
  :after ivy
  :config
  ;; Silence warning (ivy's :defer causes byte compile warnings)
  (declare-function all-the-icons-ivy-setup "all-the-icons-ivy-setup")
  (all-the-icons-ivy-setup))

(use-package counsel
  :after ivy
  :config
  ;; Silence warning (ivy's :defer causes byte compile warnings)
  (declare-function counsel-mode "counsel")

  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable)

  (setq counsel-git-cmd "rg --files"
        counsel-grep-base-command
          "rg --column --line-number --no-heading --smart-case --no-ignore --hidden --follow --color never %s %s"
        counsel-rg-base-command
        "rg --column --line-number --no-heading --smart-case --no-ignore --hidden --follow --color never %s .")

  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package company
  :defer 0.1
  :config
  ;; Silence warning (:defer causes byte compile warnings)
  (declare-function global-company-mode "company")

  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-show-numbers t)

  (global-company-mode 1))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package copilot
  :hook ((prog-mode text-mode) . copilot-mode)
  :bind ("C-<tab>" . copilot-accept-completion)
  :config
  (setq copilot-node-executable
        (exec-path-from-shell-copy-env "NODEJS_16_X")))

(use-package counsel-projectile
  :defer 0.1
  :config
  ;; Silence warning (:defer causes byte compile warnings)
  (declare-function counsel-projectile-switch-project "counsel-projectile")
  (declare-function counsel-projectile-mode "counsel-projectile")

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
        projectile-completion-system 'ivy)

  ;; Make these indexing methods safe as file-local variables
  (dolist
      (method '((projectile-indexing-method . alien)
                (projectile-indexing-method . hybrid)
                (projectile-indexing-method . native)))
    (add-to-list 'safe-local-variable-values method)))

(use-package fd-dired :commands fd-dired)

(provide 'init-completion)
