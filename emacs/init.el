;; -*- lexical-binding: t -*-

;; Keep a ref to the actual file-name-handler
(defvar file-name-handler-alist-actual file-name-handler-alist)

;; Set the file-name-handler to nil (because regexing is cpu intensive)
(setq file-name-handler-alist nil)

;; Basically disable gc during initialization
(setq gc-cons-threshold 500000000
      gc-cons-percentage 0.6)

;; Reset gc threshold and file-name-handler-alist after initialization
;; WARNING window-setup-hook is not called if emacs is ran with batch-mode
(add-hook 'window-setup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1)
    (setq file-name-handler-alist file-name-handler-alist-actual)))

(add-to-list 'load-path (expand-file-name "src/" user-emacs-directory))

;; order matters in the initialization process.
(setq files-to-load
      (list
       ;; setup package management before everything
       "init-package"
       ;; Put key binding package high on the list so other files can also bind
       ;; keys. The key binding package allows bindings keys before the
       ;; functions they're bound to are defined.
       "init-keys"
       "init-settings"
       "init-editing"
       "init-completion"
       "init-org"
       "init-nix"
       "init-haskell"
       "init-csharp"
       "init-markdown"
       "init-scripting"
       "init-prog-tools"
       "init-win-buffer-tools"
       "init-misc-tools"
       "init-visuals"
       "funcs"))

(mapc 'load files-to-load)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file)
  ;; Create an empty custom file if it's not present.
  (write-region "" nil custom-file))
