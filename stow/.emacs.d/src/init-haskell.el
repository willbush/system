;;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq-local flycheck-check-syntax-automatically '(save mode-enabled))))
  :config
  (general-def
    :prefix "SPC m"
    :states 'normal
    :keymaps 'haskell-mode-map
    "e" 'hasky-stack-execute
    "p" 'hasky-stack-package-action
    "i" 'dante-info
    "r" '(:ignore t :which-key "refactor"))

  (general-def
    :prefix "SPC m r"
    :states 'normal
    :keymaps 'haskell-mode-map
    ;; "B" 'hlint-refactor-refactor-buffer
    ;; "p" 'hlint-refactor-refactor-at-point
    "b" 'hindent-reformat-buffer
    "r" 'hindent-reformat-region)
  )

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
