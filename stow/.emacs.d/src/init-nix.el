;;; -*- lexical-binding: t; -*-

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (add-hook 'nix-mode-hook
              (lambda ()
                (add-to-list (make-local-variable 'company-backends)
                             '(company-nixos-options)))))

(use-package nix-sandbox
  :commands (nix-shell-command
             nix-shell
             nix-compile
             nix-find-sandbox
             nix-current-sandbox
             nix-executable-find))

(use-package company-nixos-options
  :commands 'company-nixos-options)

(provide 'init-nix)
