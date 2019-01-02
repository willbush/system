;;; -*- lexical-binding: t; -*-

(use-package csharp-mode
  :mode "\\.cs\\'"
  :config

  (general-def
    :prefix ","
    :states 'normal
    :keymaps 'csharp-mode-map
    "e" 'omnisharp-solution-errors
    "o" 'omnisharp-show-overloads-at-point
    "i" 'omnisharp-find-implementations
    "g" 'omnisharp-go-to-definition
    "G" 'omnisharp-go-to-definition-other-window
    "s" 'omnisharp-stop-server
    "c" 'omnisharp-check-alive-status
    "R" 'omnisharp-reload-solution
    "r" '(:ignore t :which-key "refactor")
    "n" '(:ignore t :which-key "navigate")
    "t" 'omnisharp-unit-test-buffer
    "u" 'omnisharp-fix-usings)

  (general-def
    :prefix ", r"
    :states 'normal
    :keymaps 'csharp-mode-map
    "r" 'omnisharp-rename
    "a" 'omnisharp-run-code-action-refactoring)

  (general-def
    :prefix ", n"
    :states 'normal
    :keymaps 'csharp-mode-map
    "r" 'omnisharp-navigate-to-region
    "f" 'omnisharp-navigate-to-solution-file
    "m" 'omnisharp-navigate-to-solution-member)
  )

(use-package omnisharp
  :hook ((csharp-mode . omnisharp-mode)
         (before-save . omnisharp-code-format-entire-file))
  :config
  (setq omnisharp-debug t)
  (add-hook 'omnisharp-mode-hook
            (lambda()
              (add-to-list (make-local-variable 'company-backends) '(company-omnisharp))))
  )

(provide 'init-csharp)
