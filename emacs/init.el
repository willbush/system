;; -*- lexical-binding: t -*-

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later using a
;; hook and controlling after that with `gcmh-mode'.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Keep a ref to the actual file-name-handler
(defvar default-file-name-handler-alist file-name-handler-alist)

;; Set the file-name-handler to nil (because regexing is cpu intensive)
(setq file-name-handler-alist nil)

;; Reset file-name-handler-alist after initialization
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1
          file-name-handler-alist default-file-name-handler-alist)))

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
       ;; This should also be high on the list because hydra is initialized here
       ;; and packages below use hydras
       "init-completion"
       ;; The order after this point shouldn't matter except perhaps funcs.
       ;; which comes last. The functions file utilizes functions from packages
       ;; that should have auto loads created before hand to avoid warnings.
       "init-settings"
       "init-editing"
       "init-prog-tools"
       "init-org"
       "init-nix"
       "init-haskell"
       "init-rust"
       "init-csharp"
       "init-markdown"
       "init-scripting"
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
