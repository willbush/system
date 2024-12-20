;;; -*- lexical-binding: t; -*-

;; The `vertico' package applies a vertical layout to the minibuffer. It also
;; pops up the minibuffer eagerly so we can see the available options without
;; further interactions.
(use-package vertico
  :config
  (setq vertico-cycle t)
  (vertico-mode 1))


(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


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


(use-package cape
  :commands (cape-dabbrev
             cape-elisp-block
             cape-file)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'. The order of the functions matters, the
  ;; first function returning a result wins. Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))


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

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Keep track of recently opened files
(use-package recentf
  :hook (after-init . recentf-mode)
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

(use-package smartparens
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config))


(provide 'init-completion)
