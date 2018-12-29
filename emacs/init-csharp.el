;;; -*- lexical-binding: t; -*-

(use-package csharp-mode
  :mode "\\.cs\\'"
  :after which-key
  :config
  (which-key-declare-prefixes-for-mode 'csharp-mode
    "SPC m" "mode"
    "SPC mr" "refactor"
    "SPC mn" "navigate")
  (evil-leader/set-key-for-mode 'csharp-mode
    "me" 'omnisharp-solution-errors
    "mo" 'omnisharp-show-overloads-at-point
    "mi" 'omnisharp-find-implementations
    "mg" 'omnisharp-go-to-definition
    "mG" 'omnisharp-go-to-definition-other-window
    "ms" 'omnisharp-stop-server
    "mc" 'omnisharp-check-alive-status
    "mR" 'omnisharp-reload-solution
    "mrr" 'omnisharp-rename
    "mra" 'omnisharp-run-code-action-refactoring
    "mnr" 'omnisharp-navigate-to-region
    "mnf" 'omnisharp-navigate-to-solution-file
    "mnm" 'omnisharp-navigate-to-solution-member
    "mt" 'omnisharp-unit-test-buffer
    "mu" 'omnisharp-fix-usings))

(use-package omnisharp
  :hook ((csharp-mode . omnisharp-mode)
         (before-save . omnisharp-code-format-entire-file))
  :config
  (setq omnisharp-debug t)
  (add-hook 'omnisharp-mode-hook
            (lambda()
              (add-to-list (make-local-variable 'company-backends) '(company-omnisharp)))))

(provide 'init-csharp)
