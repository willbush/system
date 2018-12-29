;; -*- mode: emacs-lisp; lexical-binding: t -*-

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

(add-to-list 'load-path "~/system/emacs/")

(require 'init-package)
(require 'init-settings)
(require 'init-keys)
(require 'init-editing)
(require 'init-completion)
(require 'init-org)
(require 'init-nix)
(require 'init-haskell)
(require 'init-csharp)
(require 'init-markdown)
(require 'init-prog-tools)
(require 'init-win-buffer-tools)
(require 'init-misc-tools)
(require 'init-themes)

(load "custom")
