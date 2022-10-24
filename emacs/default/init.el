;;; -*- lexical-binding: t -*-

;; Coping how Doom Emacs sets their GC threshold.
(defconst GC-CONS-THRESHOLD (* 16 1024 1024)) ; 16 MiB

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later using a
;; hook and controlling after that with `gcmh-mode'.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Keep a ref to the actual file-name-handler
(defvar default-file-name-handler-alist file-name-handler-alist)

;; Set the file-name-handler to nil (because regexing is cpu intensive)
(setq file-name-handler-alist nil)

(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

(eval-when-compile
  (require 'use-package))

;; order matters in the initialization process.
(require 'init-dashboard)
(require 'init-settings)
;; Put key binding packages high on the list so other files can also bind
;; keys and define hydras.
(require 'init-key-packages)
(require 'init-ui)
(require 'init-completion)
(require 'init-csharp)
(require 'init-dired)
(require 'init-editing)

;; These two must come before other languages.
(require 'init-misc-tools)
(require 'init-prog-tools)

(require 'init-haskell)
(require 'init-languages)
(require 'init-markup-languages)
(require 'init-win-buffer-tools)
(require 'init-erc)
(require 'funcs)
;; This should come after evil due to performance issues see:
;; https://github.com/noctuid/general.el/issues/180
(require 'init-keys)

;; Reset file-name-handler-alist after initialization
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Doom currently uses 16 MiB and Spacemacs is using 100 MB. I'm going to try 64
            ;; MiB to see how it goes.
            (setq gc-cons-threshold GC-CONS-THRESHOLD
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)))
