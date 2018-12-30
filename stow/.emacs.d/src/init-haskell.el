;;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq-local flycheck-check-syntax-automatically '(save mode-enabled)))))

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

(provide 'init-haskell)
