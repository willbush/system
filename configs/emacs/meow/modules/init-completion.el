;;; -*- lexical-binding: t; -*-

;; The `vertico' package applies a vertical layout to the minibuffer. It also
;; pops up the minibuffer eagerly so we can see the available options without
;; further interactions.
(use-package vertico
  :hook (emacs-startup . vertico-mode)
  :config
  (setq vertico-cycle t))


(use-package vertico-directory
  :after vertico
  :init
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))


;; The `marginalia' package provides helpful annotations next to completion
;; candidates in the minibuffer.
(use-package marginalia
  :hook (emacs-startup . vertico-mode))


(use-package all-the-icons-completion
  :after marginalia
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))


;; The `orderless' package lets the minibuffer use an out-of-order pattern
;; matching algorithm.
(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


(use-package consult :after vertico)


(use-package consult-project-extra
  ;; TODO bind keys
  :commands (consult-project-extra-find consult-project-extra-find-other-window))


(use-package corfu
  :after orderless
  :config
  (setq corfu-cycle t)
  (setq corfu-auto t)

  (global-corfu-mode)

  ;; (general-def
  ;;  :keymaps 'corfu-map
  ;;  "C-h" 'corfu-info-documentation)

  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode))


(use-package embark
  ;; PERF: takes ~35 ms to load according to hyperfine
  :defer 2 ;; load if not loaded already after n seconds
  ;; TODO bind keys
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
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; Keep track of recently opened files
(use-package recentf
  :hook (emacs-startup . recentf-mode)
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


;; TODO This is kinda more of a pain than a help.
;; IS there anything better? https://github.com/Exafunction/codeium.el perhaps
;; (use-package copilot
;;   ;; PERF: takes ~92 ms to load according to hyperfine
;;   :defer 3 ;; load if not loaded already after n seconds
;;   :hook ((prog-mode text-mode) . copilot-mode)
;;   ;; TODO BIND
;;   ;; :bind ("C-<tab>" . copilot-accept-completion)
;;   :config

;;   (setq copilot-indent-offset-warning-disable t)

;;   (add-to-list 'warning-suppress-log-types '(copilot copilot-exceeds-max-char))

;;   (defun my/disable-copilot-for-gpg-p ()
;;     "Return t if the current buffer is visiting a .gpg file."
;;     (and (stringp buffer-file-name)
;;          (string-match "\\.gpg$" buffer-file-name)))

;;   (add-to-list 'copilot-disable-predicates #'my/disable-copilot-for-gpg-p))


(provide 'init-completion)
