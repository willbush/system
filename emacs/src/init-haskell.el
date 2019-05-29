;;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq-local flycheck-check-syntax-automatically
                          '(save mode-enabled))))
  ;; auto inserts `module Foo where' and a comment block for a new Foo.hs file.
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

  ;; This scan mode is needed to give support to imenu and anything that depends on imenu.
  ;; It enables the use of which-func-mode and speedbar
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  ;; which func mode is just a nicety that shows the current function the point
  ;; is in on the mode line.
  (add-hook 'haskell-mode-hook 'which-func-mode)

  :config

  ;; speedbar depends on imenu and by extension haskell-decl-scan-mode.
  (eval-after-load "speedbar"
    '(speedbar-add-supported-extension ".hs"))

  ;; Configure haskell-mode to use cabal new-style builds
  (setq haskell-process-type 'cabal-new-repl)

  ;; Setup the build command for nix + cabal setups.
  ;; This doesn't have much of a use when using ghcid.
  (setq haskell-compile-cabal-build-command
        "nix-shell --run 'cabal new-build --ghc-option=-ferror-spans'")

  ;; Use `hasktags' to regenerate `etags' on save.
  (setq haskell-tags-on-save t)
  ;; Configure haskell-mode (haskell-cabal) to use Nix
  ;; this depends on nix-sandbox
  (setq haskell-process-wrapper-function
        (lambda (args)
          (apply 'nix-shell-command (nix-current-sandbox) args)))

  ;; rebind `evil-goto-definition' for Haskell mode.
  (general-def
    :prefix "g"
    :states 'normal
    :keymaps 'haskell-mode-map

    ;; Haskell mode provides `haskell-mode-tag-find' which appears to wrap
    ;; `xref-find-definitions', but I can't get `haskell-mode-tag-find' to work.
    ;; In addition, `haskell-mode-jump-to-def-or-tag' is available and uses GHCi
    ;; by default and falls back on tags. This works if I have a GHCi process
    ;; loaded, but the fall back doesn't work.
    "d" 'xref-find-definitions)

  (general-def
    :prefix ","
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "b" 'haskell-compile ;; `haskell-compile-cabal-build-command' has to be setup correctly
    "c" '(:ignore t :which-key "check")
    "d" '(:ignore t :which-key "dante")
    "i" 'dante-info
    "r" '(:ignore t :which-key "refactor")
    "v" '(:ignore t :which-key "visit")
    "." 'dante-type-at)

  (general-def
    :prefix ", d"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "e" 'dante-eval-block
    "d" 'dante-diagnose
    "r" 'dante-restart)

  (general-def
    :prefix ", r"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "p" 'hlint-refactor-refactor-at-point
    "b" 'hindent-reformat-buffer
    "r" 'hindent-reformat-region
    "f" 'attrap-attrap
    "s" 'haskell-mode-stylish-buffer)

  (general-def
    :prefix ", c"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "l" 'flycheck-list-errors
    "b" 'flycheck-buffer
    "c" 'flycheck-clear
    "n" 'flycheck-next-error
    "p" 'flycheck-previous-error)

  (general-def
    :prefix ", v"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "c" 'haskell-cabal-visit-file
    "o" 'my/haskell-cabal-visit-other-file))

(use-package dante
  :after haskell-mode
  :commands dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'dante-mode-hook
            '(lambda () (flycheck-add-next-checker 'haskell-dante
                                                   '(warning . haskell-hlint)))))

(use-package hindent
  :hook (haskell-mode-hook . hindent-mode)
  :init
  (when (file-executable-p "~/.nix-profile/bin/brittany")
    (setq hindent-style nil)
    (setq hindent-process-path "~/.nix-profile/bin/brittany")))

(use-package hlint-refactor
  :commands (hlint-refactor-refactor-at-point))

(defun my/haskell-cabal-visit-other-file ()
    "Opens the cabal file in another window."
    (interactive) (haskell-cabal-visit-file t))

(provide 'init-haskell)
