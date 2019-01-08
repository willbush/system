;;; -*- lexical-binding: t; -*-

;; removes gui elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; show line and column number on mode line
(line-number-mode 1)
(column-number-mode 1)

;; switches (yes or no) prompts to (y or n)
(defalias 'yes-or-no-p 'y-or-n-p)

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
      make-backup-files nil
      auto-save-default nil
      help-window-select t)

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

;; keeps a faint highlight on the line of the point. I sort of like this
;; feature, but I found that every vertical movement causes emacs takes up ~15%
;; CPU.
(global-hl-line-mode -1)

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
