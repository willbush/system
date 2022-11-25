;;; -*- lexical-binding: t; -*-

;;
;;; A file for other language packages / settings that are too simple to be in
;;; their own file.

(use-package tree-sitter
  :defer t
  :hook (after-init . global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :defer t
  :after tree-sitter)

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode))

(use-package cc-mode)

(use-package dart-mode :mode "\\.dart\\'")

(use-package haskell-mode :mode "\\.hs\\'")

(use-package racket-mode :mode "\\.rkt\\'")

(use-package terraform-mode :mode "\\.tf\\'")

(use-package mips-mode :mode "\\.mips$")

(use-package dockerfile-mode :mode "Dockerfile\\'")

(use-package typescript-mode
  ;; doesn't support tsx files yet:
  ;; https://github.com/emacs-typescript/typescript.el/issues/4
  :mode "\\.ts\\'"
  :config
  (setq typescript-ident-level 2))

;; CSS / JavaScript (build in modes) indention level
(setq-default css-indent-offset 2)
(setq-default js-indent-level 2)

(use-package powershell
  :mode ("\\.ps[dm]?1\\'" . powershell-mode)
  :commands (powershell-mode powershell))

(use-package vimrc-mode
  :mode (("\\.vim\\(rc\\)?\\'" . vimrc-mode)
         ("\\vimrc\\'" . vimrc-mode)))

;;
;;; Nix

(use-package nix-sandbox
  :commands (nix-shell-command
             nix-shell
             nix-compile
             nix-find-sandbox
             nix-current-sandbox
             nix-executable-find
             nix-find-sandbox))

(use-package nix-update :commands nix-update-fetch)

(use-package nixpkgs-fmt
  :commands nixpkgs-fmt
  :hook (nix-mode . nixpkgs-fmt-on-save-mode))

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (general-def
    :prefix ","
    :states '(normal visual)
    :keymaps 'nix-mode-map
    "u" 'nix-update-fetch
    "f" '(nixpkgs-fmt :wk "format file")))

(use-package prolog-mode :mode "\\.pl\\'")

(use-package meson-mode
  :mode "meson//.build\\'"
  :hook (meson-mode . company-mode))

(provide 'init-languages)
