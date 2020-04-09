;;; -*- lexical-binding: t; -*-

;; I don't want two spaces after my periods. This the affects behavior of
;; `fill-paragraph' (among other things).
(setq sentence-end-double-space nil)

;; switches (yes or no) prompts to (y or n)
(defalias 'yes-or-no-p 'y-or-n-p)

;; When Emacs tags get regenerated I want it to reload them without prompting me
;; y or n.
(setq-default tags-revert-without-query 1)

;; prevent indention inserting tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq evil-shift-width 2)

;; truncate lines by default
(setq-default truncate-lines t)

;; fill-paragraph uses fill-column for the width at which to break lines
(setq-default fill-column 80)

;; set default line endings
(setq-default buffer-file-coding-system 'utf-8-unix)

(setq inhibit-splash-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      help-window-select t)

;; dired settings
;; dired attempts to guess the default target for copy/rename etc.
(setq-default dired-dwim-target t)

;; ‘always’ means to copy recursively without asking.
(setq-default dired-recursive-copies 'always)

;; Put all backups in one directory (Emacs auto makes this directory as needed)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))

(defconst my/auto-save-dir "~/.emacs.d/backups/auto-saves/")

;; Put all auto-save files into one directory (will get an error on auto-save if
;; this directory doesn't exist)
(unless (file-exists-p my/auto-save-dir)
  (make-directory my/auto-save-dir t))

(setq auto-save-file-name-transforms
  `((".*" ,my/auto-save-dir t)))

;; backup / file related settings
(setq make-backup-files t
      ;; enable file backup even if the file is under source control
      vc-make-backup-files t
      ;; Always use copying to create backup files.
      backup-by-copying t
      ;; newest versions to keep when a new numbered backup is made
      kept-new-versions 10
      ;; oldest versions to keep when a new numbered backup is made
      kept-old-versions 10
      ;; use version numbers in backup files
      version-control t
      ;; delete excess backup files silently
      delete-old-versions t
      ;; file and directory deletions will move to trash instead of outright
      ;; deletion.
      delete-by-moving-to-trash t
      auto-save-default t
      ;; number of seconds idle time before auto-save (default: 30)
      auto-save-timeout 20
      ;; number of keystrokes between auto-saves (default: 300)
      auto-save-interval 200)

;; scrolling settings
(setq
  scroll-conservatively 1000
  scroll-margin 4
  scroll-step 1
  mouse-wheel-scroll-amount '(6 ((shift) . 1))
  mouse-wheel-progressive-speed nil
  fast-but-imprecise-scrolling t
  jit-lock-defer-time 0)

;; keeps a faint highlight on the line of the point. Note I found there is a
;; cost to this being on where every vertical movement causes emacs takes up
;; ~15% CPU.
(global-hl-line-mode 1)

;; Avoid performance issues in files with very long lines. Note that
;; global-so-long-mode may cause esup to error when it visits a .elc file with
;; long lines.
(use-package so-long
  :hook (after-init . global-so-long-mode))

;; follows symlinks without prompt when set to t
(setq vc-follow-symlinks t)

;; fixes many performance issue in Windows
(setq inhibit-compacting-font-caches t)

;; allow narrow to region
(put 'narrow-to-region 'disabled nil)

;; tramp:
(setq password-cache-expiry nil)

(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink")
  (setq tramp-default-method "ssh"))

;; CSS / JavaScript indention level
(setq-default css-indent-offset 2)
(setq-default js-indent-level 2)

;; Adopt Doom's sneaky garbage collection strategy of waiting until idle time to
;; collect; staving off the collector while the user is working.
(use-package gcmh
  :hook (after-init . gcmh-mode)
  :commands gcmh-idle-garbage-collect
  :config
  (setq gcmh-idle-delay 10
        gcmh-high-cons-threshold 16777216)
  (add-function :after after-focus-change-function #'gcmh-idle-garbage-collect))

(provide 'init-settings)
