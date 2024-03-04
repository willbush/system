;;; -*- lexical-binding: t; -*-

;; The `vertico' package applies a vertical layout to the minibuffer. It also
;; pops up the minibuffer eagerly so we can see the available options without
;; further interactions.
(use-package vertico
  :config
  (setq vertico-cycle t)
  (vertico-mode 1))


;; The `marginalia' package provides helpful annotations next to completion
;; candidates in the minibuffer.
(use-package marginalia
  :config
  (marginalia-mode))


;; The `orderless' package lets the minibuffer use an out-of-order pattern
;; matching algorithm.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


(use-package consult)

(use-package consult-project-extra
  :commands (consult-project-extra-find consult-project-extra-find-other-window))


(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :config

  (global-corfu-mode)

  (general-def
   :keymaps 'corfu-map
   "C-h" 'corfu-info-documentation)

  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode))


(use-package embark
  :commands (embark-act embark-dwim embark-bindings)
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package all-the-icons-completion
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))


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


(use-package copilot
  :hook ((prog-mode text-mode) . copilot-mode)
  :bind ("C-<tab>" . copilot-accept-completion)
  :config

  (setq copilot-indent-offset-warning-disable t)

  (defun my/disable-copilot-for-gpg-p ()
    "Return t if the current buffer is visiting a .gpg file."
    (and (stringp buffer-file-name)
         (string-match "\\.gpg$" buffer-file-name)))

  (add-to-list 'copilot-disable-predicates #'my/disable-copilot-for-gpg-p))

(provide 'init-completion)
