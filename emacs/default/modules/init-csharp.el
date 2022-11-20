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
              (c-set-offset 'substatement-open 0))))

(provide 'init-csharp)
