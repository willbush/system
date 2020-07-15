;;; -*- lexical-binding: t; -*-

(use-package csharp-mode
  :mode "\\.cs\\'"
  :init
  (add-hook 'csharp-mode-hook
            '(lambda ()
               (setq indent-tabs-mode t
                     c-syntactic-indentation t
                     fill-column 100
                     truncate-lines t
                     tab-width 4
                     evil-shift-width 4
                     c-basic-offset 4)
               (c-set-offset 'substatement-open 0)))

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
         ;; (before-save . omnisharp-code-format-entire-file)
         )
  :config
  (setq omnisharp-debug t)

  (eval-after-load
    'company
    '(add-to-list 'company-backends 'company-omnisharp)))

(provide 'init-csharp)
