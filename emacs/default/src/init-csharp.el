;;; -*- lexical-binding: t; -*-

(use-package csharp-mode
  :mode "\\.cs\\'"
  :init
  (add-hook 'csharp-mode-hook
            (lambda ()
              (setq indent-tabs-mode t
                    c-syntactic-indentation t
                    fill-column 100
                    truncate-lines t
                    tab-width 4
                    evil-shift-width 4
                    c-basic-offset 4)
              (c-set-offset 'substatement-open 0)))
  :config
  ;; Just going to hardcode this path for now. The auto install doesn't work for some reason in NixOS.
  (setq lsp-csharp-server-path "/etc/profiles/per-user/will/bin/omnisharp"))

(provide 'init-csharp)
