;;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook (haskell-mode . lsp-deferred)
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (progn
                ;; For some reason haskell-mode doesn't respect global
                ;; tab-width. It uses 8 by default.
                (setq-local tab-width 2)

                (set (make-local-variable 'company-backends)
                     ;; Adding `company-capf' to the end of the list because
                     ;; something else is adding it to the front of the list if
                     ;; I don't.
                     '((company-capf :with company-tabnine) company-tabnine company-capf)))))
  :config

  ;; I'm not sure what, but something calls a function provided by `haskell-doc.el'
  ;; and doesn't properly require it before doing so. requiring haskell-doc here
  ;; this fixes the void function error.
  (require 'haskell-doc)

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

  ;; fix the haskell-indentation-mode-map for evil-mode. For example, it would
  ;; insert a comma in normal mode when haskell-indentation-mode was enabled.
  (general-unbind 'haskell-indentation-mode-map
    ")" "," ";" "<backtab>" "RET" "]" "}")

  (general-def
    :states '(insert)
    :keymaps 'haskell-indentation-mode-map
      ")" 'haskell-indentation-common-electric-command
      "," 'haskell-indentation-common-electric-command
      ";" 'haskell-indentation-common-electric-command
      "<backtab>" 'haskell-indentation-indent-backwards
      "RET" 'haskell-indentation-newline-and-indent
      "]" 'haskell-indentation-common-electric-command
      "}" 'haskell-indentation-common-electric-command)

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
    "F" '(:ignore t :wk "workspace folders")
    "c" '(:ignore t :wk "check")
    "d" '(:ignore t :wk "document")
    "f" '(:ignore t :wk "find")
    "p" '(:ignore t :wk "peek")
    "r" '(:ignore t :wk "refactor")
    "s" '(:ignore t :wk "lsp session")
    "t" '(:ignore t :wk "toggle"))

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
    "n" 'lsp-ui-find-next-reference
    "p" 'lsp-ui-find-prev-reference
    "r" 'lsp-find-references
    "t" 'lsp-find-type-definition)

  (general-def
    :prefix ", p"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "d" 'lsp-ui-peek-find-definitions
    "i" 'lsp-ui-peek-find-implementation
    "r" 'lsp-ui-peek-find-references)

  (general-def
    :prefix ", r"
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "B" 'lsp-format-buffer
    "R" 'lsp-rename
    "a" 'lsp-execute-code-action
    "b" 'format-all-buffer
    "f" 'lsp-format-region
    "o" 'lsp-organize-imports
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

  ;; Comment/uncomment this line to see interactions between lsp client/server.
  (setq lsp-log-io t)

  ;; the following does not work: (lsp-haskell-set-formatter :brittany)
  ;; see https://github.com/emacs-lsp/lsp-haskell/issues/75
  (defun lsp-haskell-set-formatter-brittany ()
    "Use brittany."
    (interactive)
    (lsp-haskell-set-formatter "brittany")
    (lsp-haskell--set-configuration))

  (defun lsp-haskell-set-formatter-floskell ()
    "Use floskell."
    (interactive)
    (lsp-haskell-set-formatter "floskell")
    (lsp-haskell--set-configuration))

  (defun lsp-haskell-set-formatter-ormolu ()
    "Use ormolu."
    (interactive)
    (lsp-haskell-set-formatter "ormolu")
    (lsp-haskell--set-configuration))

  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper"
        lsp-haskell-process-args-hie '()))

;; HLS provides format providers, but I'm going to keep this around until I'm
;; sure they're dependable.
;;;###autoload
(defun my/brittany-format-region (start end)
  "Uses Brittany to format a region of Haskell code."
  (interactive "r")
  (let ((cmd "brittany")
        (out-buffer "*Brittany Command Output*")
        (err-buffer "*Brittany Command Error*"))
    (shell-command-on-region start end cmd out-buffer t err-buffer t nil)))

(provide 'init-haskell)
