;;; -*- lexical-binding: t; -*-

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :defer t
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit :after magit)

(use-package git-timemachine
  ;; mode key bindings provided by evil-collection
  :commands (git-timemachine))

(use-package flycheck
  :hook (haskell-mode . flycheck-mode))

(add-hook 'prog-mode-hook
          '(lambda ()
             (setq show-trailing-whitespace t)))

(provide 'init-prog-tools)
