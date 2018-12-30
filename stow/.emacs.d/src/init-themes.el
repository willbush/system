;;; -*- lexical-binding: t; -*-

(use-package doom-themes
  :config
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; doom modeline requires M-x all-the-icons-install-fonts
(use-package doom-modeline
  :hook (after-init . doom-modeline-init))

(provide 'init-themes)
