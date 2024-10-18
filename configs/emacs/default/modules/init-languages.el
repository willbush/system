;;; -*- lexical-binding: t; -*-

;;
;;; A file for other language packages / settings that are too simple to be in
;;; their own file.

(use-package project
  :init
  ;; project.el project root marker for when in a project that's in a
  ;; sub-directory of a git repo.
  (setq project-vc-extra-root-markers '(".envrc")))


(use-package treesit-auto
  :demand t
  :config
  ;; Note `treesit-auto-install-all' can be called to install every available,
  ;; maintained grammar.
  (setq treesit-auto-install t)
  (global-treesit-auto-mode))

(use-package rust-mode
  :mode ("\\.rs$" . rust-mode)
  :init
  (add-hook 'rust-mode-hook
            (lambda ()
              (setq-local fill-column 100)))
  (setq rust-ts-mode-hook rust-mode-hook))


(use-package cc-mode)
(use-package dart-mode :mode "\\.dart\\'")
(use-package dockerfile-mode :mode "Dockerfile\\'")
(use-package haskell-mode :mode "\\.hs\\'")
(use-package just-mode :mode ("\\.just'" "justfile\\'"))
(use-package protobuf-mode :mode "\\.proto$")
(use-package terraform-mode :mode "\\.tf\\'")


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

(use-package nix-mode :mode "\\.nix\\'")

(provide 'init-languages)
