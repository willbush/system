;;; -*- lexical-binding: t; -*-

;; disable GUI elements.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; show line and column number on mode line
(line-number-mode 1)
(column-number-mode 1)

;; switches (yes or no) prompts to (y or n)
(defalias 'yes-or-no-p 'y-or-n-p)

;; When Emacs tags get regenerated I want it to reload them without prompting me
;; y or n.
(setq-default tags-revert-without-query 1)

;; prevent indention inserting tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; fill-paragraph uses fill-column for the width at which to break lines
(setq-default fill-column 80)

;; set default line endings
(setq-default buffer-file-coding-system 'utf-8-unix)

(setq inhibit-splash-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      help-window-select t)

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

;; Disk space is cheap so lets make a back up on each save (ignores files over
;; 5MB by default and remote files).
(use-package backup-each-save
  :commands backup-each-save
  :init (add-hook 'after-save-hook 'backup-each-save)
  :config
  ;; Save all per-save backups in $TMPDIR/emacs$UID/. I have NixOS setup to
  ;; delete /tmp on boot and Windows 10 needs to be configured to automatically
  ;; clean its temp directory.
  (setq backup-each-save-mirror-location
        (expand-file-name
         (format "emacs%d-per-save-backups/" (user-uid)) temporary-file-directory)))

;; enable prettify symbols when using GUI emacs
(when window-system (global-prettify-symbols-mode t))

;; changes default behavior of scrolling to bottom of screen.  Normally,
;; scrolling past the bottom causes it to scroll down enough so that the point
;; is re-centered. The documentation on the variable states values above 100
;; behave the same, but I could not observe any difference for values greater
;; than or equal to 1. Also of note from testing I found that the constant
;; redrawing of the screen to make the scrolling smooth utilizes ~20-30%
;; CPU. However, this hardly affects me with how I move around.
(setq scroll-conservatively 666)

;; keeps a faint highlight on the line of the point. Note I found there is a
;; cost to this being on where every vertical movement causes emacs takes up
;; ~15% CPU.
(global-hl-line-mode 1)

;; follows symlinks without prompt when set to t
(setq vc-follow-symlinks t)

;; fixes performance issue with doom-modeline in Windows
(setq inhibit-compacting-font-caches t)

(setq electric-pair-pairs
  '(
    (?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\})
   ))
(electric-pair-mode t)

(setq default-tab-width 2)
(setq evil-shift-width 2)

;; allow narrow to region
(put 'narrow-to-region 'disabled nil)

;; terminal:
(defvar my-shell "/run/current-system/sw/bin/zsh")
(defadvice ansi-term (before force-bash)
(interactive (list my-shell)))
(ad-activate 'ansi-term)

(provide 'init-settings)
