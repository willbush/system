;;; -*- lexical-binding: t; -*-

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (add-to-list 'company-backends 'company-nixos-options))

(use-package nix-sandbox
  :commands (nix-shell-command
             nix-shell
             nix-compile
             nix-find-sandbox
             nix-current-sandbox
             nix-executable-find))

(use-package company-nixos-options
  :commands company-nixos-options)

(provide 'init-nix)
