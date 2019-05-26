;;; -*- lexical-binding: t; -*-

(use-package nix-mode :mode "\\.nix\\'")

(use-package nix-sandbox
  :commands (nix-shell-command
             nix-shell
             nix-compile
             nix-find-sandbox
             nix-current-sandbox
             nix-executable-find
             nix-find-sandbox))

(use-package nix-update
  :commands nix-update-fetch
  :init
  (general-def
    :prefix ","
    :states '(normal visual)
    :keymaps 'nix-mode-map
    "u" 'nix-update-fetch))

(provide 'init-nix)
