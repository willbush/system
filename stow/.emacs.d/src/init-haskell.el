;;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq-local flycheck-check-syntax-automatically
                          '(save mode-enabled))))
  :config
  ;; Configure haskell-mode to use cabal new-style builds
  (setq haskell-process-type 'cabal-new-repl)

  ;; Configure haskell-mode (haskell-cabal) to use Nix
  ;; this depends on nix-sandbox
  (setq haskell-process-wrapper-function
        (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))

  (general-def
    :prefix ","
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "c" '(:ignore t :which-key "check")
    "d" '(:ignore t :which-key "dante")
    "s" 'hasky-stack-execute
    "i" 'dante-info
    "p" 'hasky-stack-package-action
    "r" '(:ignore t :which-key "refactor")
    "t" 'hasky-stack-test
    "." 'dante-type-at)

  (general-def
    :prefix ", d"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "e" 'dante-eval-block
    "d" 'dante-diagnose
    "r" 'dante-restart)

  (general-def
    :prefix ", r"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    ;; "B" 'hlint-refactor-refactor-buffer
    ;; "p" 'hlint-refactor-refactor-at-point
    "b" 'hindent-reformat-buffer
    "r" 'hindent-reformat-region
    "f" 'attrap-attrap
    "s" 'haskell-mode-stylish-buffer)

  (general-def
    :prefix ", c"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "l" 'flycheck-list-errors
    "b" 'flycheck-buffer
    "c" 'flycheck-clear
    "n" 'flycheck-next-error
    "p" 'flycheck-previous-error))

(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'dante-mode-hook
            '(lambda () (flycheck-add-next-checker 'haskell-dante
                                                   '(warning . haskell-hlint)))))

(use-package hindent
  :hook (haskell-mode-hook . hindent-mode))

(use-package hasky-stack
  :commands (hasky-stack-execute
             hasky-stack-package-action
             hasky-stack-new))

(provide 'init-haskell)
