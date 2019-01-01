;;; -*- lexical-binding: t; -*-

(use-package csharp-mode
  :mode "\\.cs\\'")

(use-package omnisharp
  :hook ((csharp-mode . omnisharp-mode)
         (before-save . omnisharp-code-format-entire-file))
  :config
  (setq omnisharp-debug t)
  (add-hook 'omnisharp-mode-hook
            (lambda()
              (add-to-list (make-local-variable 'company-backends) '(company-omnisharp)))))

(provide 'init-csharp)
