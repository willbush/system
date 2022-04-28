;;; -*- lexical-binding: t; -*-

;; A dependency that dashboard has.
(use-package page-break-lines)

(use-package dashboard
  :init
  ;; causes emacs daemon to lock up when attempting to start
  ;; see: https://github.com/emacs-dashboard/emacs-dashboard/issues/334
  ;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config

  (let ((art "~/code/external/nixos-artwork/logo/nix-snowflake.svg"))
    (setq dashboard-startup-banner (if (file-exists-p art) art 3)))

  (setq dashboard-banner-logo-title nil
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents  . 5)
                          (projects . 5)))
  ;; This is the default icon, but it doesn't always show up when running Emacs
  ;; as a daemon. So I set it explicitly here to fix the issue.
  (setq dashboard-footer-icon
    #("" 0 1
      (rear-nonsticky t display
                      (raise -0.06)
                      font-lock-face #1=(:family "file-icons" :height 1.32 :inherit font-lock-keyword-face)
                      face #1#)))
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)
