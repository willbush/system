;;; -*- lexical-binding: t -*-

(defconst GC-CONS-THRESHOLD (* 32 1024 1024)) ; 32 MiB

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later using a
;; hook and controlling after that with `gcmh-mode'.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

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

(require 'init-settings)
(require 'init-meow)


;;
;;; Packages

(use-package which-key
  :ensure nil ;; included in Emacs.
  :hook (emacs-startup . which-key-mode)
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-idle-delay 0.5))

(require 'init-meow)
;; (require 'init-completion)

;; TODO
;; (require 'init-dired)
;; (require 'init-editing)
;; (require 'init-misc-tools)
;; (require 'init-completion)
;; (require 'init-prog-tools)
;; (require 'init-languages)
;; (require 'init-markup-languages)
;; (require 'init-org)
;; (require 'init-erc)
;; (require 'init-funcs)
;; (require 'init-keys)


;;
;;; Packages

(use-package doom-themes
  :hook (emacs-startup . load-doom-theme)
  :config
  (defun load-doom-theme ()
    (load-theme 'doom-winter-is-coming-dark-blue t)))


(use-package moody
  :hook (emacs-startup . my/start-modeline)
  :config
  (defun my/start-modeline ()
    (moody-replace-mode-line-front-space)
    (moody-replace-mode-line-buffer-identification)
    (moody-replace-vc-mode)))


;; Highlight matching parentheses
(use-package paren
  :ensure nil
  :hook (prog-mode . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package pulsar
  :hook
  (after-init . pulsar-global-mode)
  (next-error . pulsar-pulse-line)
  :config
  (setq pulsar-pulse t
        pulsar-delay 0.05
        pulsar-iterations 7
        pulsar-face 'pulsar-magenta
        pulsar-highlight-face 'pulsar-yellow)

  (setq pulse-functions
        '(
          mode-line-other-buffer
          next-multiframe-window))

  (dolist (f pulse-functions)
    (add-to-list 'pulsar-pulse-functions f)))


;;
;;; Dired Settings

;; dired attempts to guess the default target for copy/rename etc.
(setq-default dired-dwim-target t)

;; ‘always’ means to copy recursively without asking.
(setq-default dired-recursive-copies 'always)

;; better dired sorting defaults
;; https://www.emacswiki.org/emacs/DiredSorting
(setq dired-listing-switches "-lahv  --group-directories-first")

;; Reset file-name-handler-alist after initialization
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold GC-CONS-THRESHOLD
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)))
