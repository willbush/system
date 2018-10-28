;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; similar ot how doom does it increase GC threshold during init
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; reset GC after startup
(add-hook 'emacs-startup-hook
	  (lambda () (setq gc-cons-threshold 16777216
		      gc-cons-percentage 0.1)))

(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; ensure everything is installed
(setq use-package-always-ensure t)

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-search-module 'evil-search
	evil-ex-complete-emacs-commands nil
	evil-vsplit-window-right t
	evil-split-window-below t
	evil-shift-round nil
	evil-want-C-u-scroll t)
  :config
  ;; unbind evil-lookup
  (eval-after-load "evil-maps"
    (define-key evil-motion-state-map "\S-k" nil))

  ;; make mnemonic alias for how I want to bind it
  (defalias 'my/evil-search-clear-highlight 'evil-ex-nohighlight)
  ;; evil global key bindings
  (global-set-key (kbd "C-S-h") 'evil-window-left)
  (global-set-key (kbd "C-S-l") 'evil-window-right)
  (global-set-key (kbd "C-S-j") 'evil-window-down)
  (global-set-key (kbd "C-S-k") 'evil-window-up))


;; Enables searching via * on a visual selection.
(use-package evil-visualstar
  :after evil
  :init (global-evil-visualstar-mode))

(use-package which-key
  :hook (after-init . which-key-mode))

(use-package avy :commands avy-goto-char)

(defun my/open-shell ()
  "Opens my prefered shell for the current operating system."
  (interactive)
  (if (eq system-type 'windows-nt)
      (call-interactively 'eshell)
    (call-interactively 'ansi-term)))

(defun my/close-all-buffers ()
  "close all buffers"
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun my/switch-to-dashboard ()
  "Switch to *dashboard* (creates if needed)"
  (interactive)
  (let ((buffer "*dashboard*"))
    (when (not (get-buffer buffer))
      (dashboard-insert-startupify-lists))
    (switch-to-buffer buffer)
    (dashboard-refresh-buffer)))

(defun my/switch-to-messages ()
  "Switch to *Messages* buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun my/switch-to-scratch ()
  "Switch to *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun my/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (kill-buffer)))

(defun my/toggle-maximize-window ()
  "Toggle between maximizing the window and restoring previous window setup."
  (interactive)
  (if (and (= 1 (length (window-list)))
	   (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

(use-package evil-leader
  ;; after which key so I can keep its prefix declariations next to
  ;; evil-leader declarations in this config block
  :after which-key
  :init (global-evil-leader-mode)
  :config

  (evil-leader/set-leader "<SPC>")

  (evil-leader/set-key
    "<SPC>" 'counsel-M-x
    "'" 'my/open-shell)

  (which-key-declare-prefixes "SPC b" "buffer")
  (evil-leader/set-key
    "TAB" 'mode-line-other-buffer
    "bb" 'ivy-switch-buffer
    "bs" 'save-buffer
    "bd" 'my/kill-this-buffer
    "bk" 'kill-buffer ;; requests buffer to kill
    "bp" 'previous-buffer
    "bn" 'next-buffer
    "bh" 'my/switch-to-dashboard
    "bm" 'my/switch-to-messages
    "bs" 'my/switch-to-scratch
    "bq" 'my/close-all-buffers)

  (which-key-declare-prefixes "SPC c" "comment")
  (evil-leader/set-key
    "cr" 'comment-or-uncomment-region
    "cl" 'comment-line)

  (which-key-declare-prefixes "SPC f" "file")
  (evil-leader/set-key
    "ff" 'counsel-find-file
    "fs" 'save-buffer)

  (which-key-declare-prefixes "SPC F" "frame")
  (evil-leader/set-key
    "Fd" 'delete-frame
    "FD" 'delete-other-frames
    "Fo" 'other-frame
    "Fn" 'make-frame)

  (which-key-declare-prefixes "SPC g" "git")
  (evil-leader/set-key
    "gs" 'magit-status)

  (which-key-declare-prefixes "SPC h" "help")
  (which-key-declare-prefixes "SPC hd" "describe")
  (evil-leader/set-key
    "hi" 'info
    "hl" 'counsel-find-library
    "hn"  'view-emacs-news
    "hdb" 'counsel-descbinds
    "hdc" 'describe-char
    "hdf" 'counsel-describe-function
    "hdk" 'describe-key
    "hdm" 'describe-mode
    "hdp" 'describe-package
    "hdv" 'counsel-describe-variable
    "hdt" 'describe-theme
    "hds" 'counsel-info-lookup-symbol
    "hPs" 'profiler-start
    "hPk" 'profiler-stop
    "hPr" 'profiler-report
    "hPw" 'profiler-report-write-profile)

  (which-key-declare-prefixes "SPC j" "jump")
  (evil-leader/set-key
    "jj" 'avy-goto-char
    "jf" 'find-function
    "jv" 'find-variable)

  (which-key-declare-prefixes "SPC n" "narrow")
  (evil-leader/set-key
    "nr" 'narrow-to-region
    "np" 'narrow-to-page
    "nf" 'narrow-to-defun
    "nw" 'widen)

  (which-key-declare-prefixes "SPC q" "quit")
  (evil-leader/set-key
    "qq" 'save-buffers-kill-terminal)

  (which-key-declare-prefixes "SPC s" "search")
  (evil-leader/set-key
    "ss" 'swiper
    "sc" 'my/evil-search-clear-highlight
    "sd" 'my/counsel-rg-directory)

  (which-key-declare-prefixes "SPC t" "toggle")
  (evil-leader/set-key
    "tt" 'display-time-mode
    "tl" 'toggle-truncate-lines
    "tf" 'auto-fill-mode
    "tn" 'linum-mode)

  (which-key-declare-prefixes "SPC w" "window")
  (evil-leader/set-key
    "wd" 'delete-window
    "wh" 'evil-window-left
    "wl" 'evil-window-right
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wH" 'evil-window-move-far-left
    "wL" 'evil-window-move-far-right
    "wJ" 'evil-window-move-far-down
    "wK" 'evil-window-move-far-up
    "w/" 'split-window-horizontally
    "w-" 'split-window-vertically
    "wo" 'delete-other-windows
    "wm" 'my/toggle-maximize-window
    "wx" 'kill-buffer-and-window
    "wba" 'balance-windows-area
    "wbb" 'balance-windows)
)

;; Enables inc/dec of numbers!
(use-package evil-numbers
  :after evil
  :config
  ;; Bind increment and decrement number at point.
  (global-set-key (kbd "M-=") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "M--") 'evil-numbers/dec-at-pt))

;; Bind zoom in and out
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :after evil
  :config (global-evil-matchit-mode 1))

(use-package evil-exchange
  :after evil
  :config (evil-exchange-cx-install))

;; IVY
(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

(use-package counsel
  :after ivy
  :bind (("C-c C-r" . ivy-resume))
  :config
  ;; make mnemonic alias for how I want to bind it
  (defalias 'my/counsel-rg-directory 'counsel-rg)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (setq counsel-git-cmd "rg --files"
	counsel-grep-base-command
	  "rg --column --line-number --no-heading --smart-case --no-ignore --hidden --follow --color never %s %s"
	counsel-rg-base-command
	  "rg --column --line-number --no-heading --smart-case --no-ignore --hidden --follow --color never %s ."))

(use-package swiper :bind (("C-s" . swiper)))

;; Used by Ivy to sort commands by frequency.
(use-package smex
  :hook (after-init . smex-initialize)
  :config
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))


;; MINOR SETTINGS:

;; removes gui elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; show line and column number on mode line
(line-number-mode 1)
(column-number-mode 1)

;; switches (yes or no) prompts to (y or n)
(defalias 'yes-or-no-p 'y-or-n-p)

;; prevent indention for inserting tabs
(setq-default indent-tabs-mode nil)

(setq inhibit-splash-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      make-backup-files nil
      auto-save-default nil
      default-fill-column 80)

;; enable prettify symbols when using GUI emacs
(when window-system (global-prettify-symbols-mode t))

;; changes default behavior of scrolling to bottom of screen.
;; Normally, scrolling past the bottom casues it to scroll down enough
;; so that the point is recentered. The documentation on the variable
;; states values above 100 behave the same, but I could not observe
;; any difference for values greater than or equal to 1.
(setq scroll-conservatively 666)

;; follows symlinks without prompt when set to t
(setq vc-follow-symlinks t)

(setq electric-pair-pairs
  '(
    (?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\})
   ))
(electric-pair-mode t)

(setq default-tab-width 2)
(setq evil-shift-width 2)

;; TERMINAL:
;; keeps a faint highlight on the line of the point.
(global-hl-line-mode t)
(defvar my-shell "/run/current-system/sw/bin/zsh")
(defadvice ansi-term (before force-bash)
(interactive (list my-shell)))
(ad-activate 'ansi-term)

;; ORG
(setq org-src-window-setup 'current-window)
(setq org-log-done 'time)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(use-package doom-themes
  :config
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; doom modeline requires M-x all-the-icons-install-fonts
(use-package doom-modeline
  :hook (after-init . doom-modeline-init))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dashboard
  :hook (after-init . dashboard-setup-startup-hook)
  :config
  (setq dashboard-banner-logo-title (format "init time: %s" (emacs-init-time)))
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 5))))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.3))

(use-package magit
  :defer t
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

;; for some reason using :requires (evil magit) prevents it from initializing
(use-package evil-magit :after magit)

(use-package csharp-mode
  :mode("\\.cs\\'" . csharp-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "simp" :family "Hack")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (doom-vibrant)))
 '(custom-safe-themes
   (quote
    ("1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" default)))
 '(package-selected-packages
   (quote
    (evil-magit magit smex which-key use-package rainbow-delimiters evil-visualstar evil-surround evil-numbers evil-matchit evil-leader evil-exchange doom-themes doom-modeline dashboard counsel company avy))))

(put 'narrow-to-region 'disabled nil)
