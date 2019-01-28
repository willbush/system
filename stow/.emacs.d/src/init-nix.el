;;; -*- lexical-binding: t; -*-

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nix-sandbox
  :commands (nix-shell-command
             nix-shell
             nix-compile
             nix-find-sandbox
             nix-current-sandbox
             nix-executable-find))

(provide 'init-nix)
