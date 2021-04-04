;;; -*- lexical-binding: t -*-

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
    (setq gc-cons-threshold MY/GC-CONS-THRESHOLD
          gc-cons-percentage 0.1
          file-name-handler-alist default-file-name-handler-alist)))

;; order matters in the initialization process.
(mapc 'load
      (list
       "init-settings"
       ;; Put key binding packages high on the list so other files can also bind
       ;; keys and define hydras.
       "init-key-packages"
       "init-ui"
       "init-completion"
       "init-csharp"
       "init-dired"
       "init-editing"
       "init-haskell"
       "init-languages"
       "init-misc-tools"
       "init-org"
       "init-prog-tools"
       "init-win-buffer-tools"
       "funcs"
       ;; This should come after evil due to performance issues see:
       ;; https://github.com/noctuid/general.el/issues/180
       "init-keys"))
