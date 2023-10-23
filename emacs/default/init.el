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

(when (string-equal (getenv "EMACS_RUN_BENCHMARK_INIT") "1")
  (use-package benchmark-init
    :config
    ;; Disable collection of benchmark data after init is done and show results.
    (add-hook 'after-init-hook
              (lambda ()
                (progn
                  (benchmark-init/deactivate)
                  (require 'benchmark-init-modes)
                  (benchmark-init/show-durations-tree)
                  (benchmark-init/show-durations-tabulated))))))

;; order matters in the initialization process.
(require 'init-defaults)
(require 'init-dashboard)
(require 'init-key-packages)
(require 'init-ui)
(require 'init-csharp)
(require 'init-dired)
(require 'init-editing)
(require 'init-misc-tools)
(require 'init-completion)
(require 'init-prog-tools)
(require 'init-languages)
(require 'init-markup-languages)
(require 'init-win-buffer-tools)
(require 'init-erc)
(require 'init-funcs)
;; This should come after evil due to performance issues see:
;; https://github.com/noctuid/general.el/issues/180
(require 'init-keys)

;; Reset file-name-handler-alist after initialization
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold GC-CONS-THRESHOLD
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)))
