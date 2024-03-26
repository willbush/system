;;; -*- lexical-binding: t; -*-

;; A dependency that dashboard has.
(use-package page-break-lines)


(use-package dashboard
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config

  (let ((art "~/code/external/nixos-artwork/logo/nix-snowflake.svg"))
    (setq dashboard-startup-banner (if (file-exists-p art) art 3)))

  (setq dashboard-banner-logo-title nil
        dashboard-projects-backend 'project-el
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-items '((recents  . 5)
                          (projects . 5)
                          (bookmarks . 5)))


  ;; This is the default icon, but it doesn't always show up when running Emacs
  ;; as a daemon. So I set it explicitly here to fix the issue.
  (setq dashboard-footer-icon
    #("" 0 1
      (rear-nonsticky t display
                      (raise -0.06)
                      font-lock-face #1=(:family "file-icons" :height 1.32 :inherit font-lock-keyword-face)
                      face #1#)))

  (dashboard-setup-startup-hook)

  (general-unbind
    :keymaps 'dashboard-mode-map
    "C-n" "C-p" "SPC" "-" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
    "<" ">" "g" "j" "k" "p" "q" "r" "{" "}" "DEL" "S-SPC"
    "<backspace>" "<backtab>" "<delete>" "<mouse-1>")

  (general-def
    :states 'normal
    :keymaps 'dashboard-mode-map
    "g m" 'dashboard-jump-to-bookmarks
    "g p" 'dashboard-jump-to-projects
    "g r" 'dashboard-jump-to-recents))

(provide 'init-dashboard)
