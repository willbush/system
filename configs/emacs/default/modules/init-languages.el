;;; -*- lexical-binding: t; -*-

;;
;;; A file for other language packages / settings that are too simple to be in
;;; their own file.

(use-package project
  :ensure nil ;; included in Emacs.
  :init
  ;; project.el project root marker for when in a project that's in a
  ;; sub-directory of a git repo.
  (setq project-vc-extra-root-markers '("Cargo.toml")))


(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)

  ;; `treesit-auto-install-all' needs to be called to install every available,
  ;; maintained grammar. This depends on `gcc' being installed.

  (global-treesit-auto-mode))

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (add-hook 'rustic-mode-hook
            (lambda ()
              (setq fill-column 100)))
  (setq rustic-lsp-client 'eglot))

(use-package cc-mode)

(use-package dart-mode :mode "\\.dart\\'")

(use-package haskell-mode :mode "\\.hs\\'")

(use-package terraform-mode :mode "\\.tf\\'")

(use-package protobuf-mode :mode "\\.proto$")

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

(use-package csharp-mode
  :ensure nil ;; included in Emacs.
  :init

  (defun csharp-mode-setup ()
    (setq indent-tabs-mode t
          c-syntactic-indentation t
          fill-column 100
          truncate-lines t
          tab-width 4
          evil-shift-width 4
          c-basic-offset 4)

    (c-set-offset 'substatement-open 0))

  (add-hook 'csharp-mode-hook 'csharp-mode-setup)
  (add-hook 'csharp-ts-mode-hook 'csharp-mode-setup)

  (add-hook 'web-mode-hook
            (lambda ()
              (when (or (string-match "\\.csproj\\'" (buffer-file-name))
                        (string-match "\\.xaml\\'" (buffer-file-name))
                        (string-match "\\.props\\'" (buffer-file-name)))
                (progn
                  (setq indent-tabs-mode t)
                  (web-mode-use-tabs))))))

;;
;;; Nix

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
    "f" '(nixpkgs-fmt :wk "format file")))

(provide 'init-languages)
