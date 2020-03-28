;;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook
            '(lambda ()
               (progn
                 ;; For some reason haskell-mode doesn't respect global
                 ;; tab-width. It uses 8 by default.
                 (setq-local tab-width 2)

                 (set (make-local-variable 'company-backends)
                      ;; Adding `company-capf' to the end of the list because
                      ;; something else is adding it to the front of the list if
                      ;; I don't.

                      ;; Note that `company-lsp' is also something to consider
                      ;; using instead of `company-capf'. However, since it
                      ;; hasn't seen any activity in a year on their Github
                      ;; repository, I'm reluctant to use it.
                      '((company-capf :with company-tabnine) company-tabnine company-capf)))))
  :config
  ;; Use cabal new-style builds.
  (setq haskell-process-type 'cabal-new-repl)

  ;; Use `hasktags' to regenerate `etags' on save. Using `hasktags' is a good
  ;; fall back to `lsp-mode' and it's fast.
  (setq haskell-tags-on-save t)

  ;; Auto inserts `module Foo where' and a comment block for a new Foo.hs file.
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

  ;; This scan mode is needed to give support to imenu and anything that depends on imenu.
  ;; It enables the use of which-func-mode and speedbar.
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

  ;; `which-function-mode' is just a nicety that shows the current function the
  ;; point is in on the mode line.
  (add-hook 'haskell-mode-hook 'which-function-mode)

  ;; rebind `evil-goto-definition' for Haskell mode.
  (general-def
    :states 'normal
    :keymaps 'haskell-mode-map
    "gd" 'xref-find-definitions)

  (general-def
    :prefix ","
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "." 'lsp-describe-thing-at-point
    "F" '(:ignore t :which-key "workspace folders")
    "c" '(:ignore t :which-key "check")
    "d" '(:ignore t :which-key "document")
    "f" '(:ignore t :which-key "find")
    "p" '(:ignore t :which-key "peek")
    "r" '(:ignore t :which-key "refactor")
    "s" '(:ignore t :which-key "lsp session")
    "t" '(:ignore t :which-key "toggle"))

  (general-def
    :prefix ", F"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "a" 'lsp-workspace-folders-add
    "b" 'lsp-workspace-blacklist-remove
    "r" 'lsp-workspace-folders-remove)

  (general-def
    :prefix ", d"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "g" 'lsp-ui-doc-glance)

  (general-def
    :prefix ", c"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "L" 'flycheck-list-errors
    "b" 'flycheck-buffer
    "c" 'flycheck-clear
    "l" 'lsp-ui-flycheck-list
    "n" 'flycheck-next-error
    "p" 'flycheck-previous-error)

  (general-def
    :prefix ", f"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "d" 'lsp-find-definition
    "i" 'lsp-find-implementation
    "r" 'lsp-find-references
    "t" 'lsp-find-type-definition)

  (general-def
    :prefix ", p"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "i" 'lsp-ui-peek-find-implementation
    "r" 'lsp-ui-peek-find-references
    "d" 'lsp-ui-peek-find-definitions)

  (general-def
    :prefix ", r"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "B" 'lsp-format-buffer ;; Works in HIE, but not Ghcide yet.
    "R" 'lsp-rename
    "a" 'lsp-execute-code-action
    "b" 'format-all-buffer ;; uses Brittany without lsp
    "f" 'lsp-format-region ;; Works in HIE, but not Ghcide yet.
    "o" 'lsp-organize-imports
    "p" 'hlint-refactor-refactor-at-point ;; Ghcide doesn't support code actions for hlint yet.
    "r" 'my/brittany-format-region)

  (general-def
    :prefix ", s"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "D" 'lsp-disconnect
    "d" 'lsp-describe-session
    "q" 'lsp-workspace-shutdown
    "r" 'lsp-workspace-restart
    "s" 'lsp)

  (general-def
    :prefix ", t"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "D" 'lsp-signature-toggle-full-docs
    "F" 'lsp-toggle-on-type-formatting
    "S" 'lsp-ui-sideline-mode
    "a" 'lsp-toggle-signature-auto-activate
    "d" 'lsp-ui-doc-mode
    "h" 'lsp-toggle-symbol-highlight
    "m" 'lsp-ui-imenu
    "s" 'lsp-ui-sideline-toggle-symbols-info
    "t" 'lsp-toggle-trace-io
    "u" 'lsp-ui-mode))

(use-package lsp-haskell
  :after (haskell-mode lsp-mode)
  :config
  ;; Silence warnings
  (declare-function lsp-haskell--get-root "lsp-haskell")

  (setq lsp-haskell-process-path-hie "ghcide"
        lsp-haskell-process-args-hie '())

  (setq lsp-haskell-process-wrapper-function
        (lambda (argv)
             (append
             (append (list "nix-shell" "-I" "." "--command" )
                     (list (mapconcat 'identity argv " ")))
             (list (concat (lsp-haskell--get-root) "shell.nix"))))))

(use-package hlint-refactor
  :commands (hlint-refactor-refactor-at-point))

(defun my/brittany-format-region (start end)
  "Uses Brittany to format a region of Haskell code."
  (interactive "r")
  (let ((cmd "brittany")
        (out-buffer "*Brittany Command Output*")
        (err-buffer "*Brittany Command Error*"))
    (shell-command-on-region start end cmd out-buffer t err-buffer t nil)))

(provide 'init-haskell)
