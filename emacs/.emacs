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
	evil-want-C-u-scroll t))

;; Enables searching via * on a visual selection.
(use-package evil-visualstar
  :after evil
  :init (global-evil-visualstar-mode))

(use-package which-key
  :hook (after-init . which-key-mode))

(use-package avy :commands avy-goto-char)

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun switch-to-dashboard ()
  (interactive)
  (switch-to-buffer "*dashboard*"))

(defun switch-to-messages ()
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(use-package evil-leader
  ;; after which key so I can keep its prefix declariations next to
  ;; evil-leader declarations in this config block
  :after which-key
  :init (global-evil-leader-mode)
  :config

  (evil-leader/set-leader "<SPC>")

  (evil-leader/set-key
    "<SPC>" 'execute-extended-command ;; M-x
    "j" 'avy-goto-char
    "'" 'ansi-term)

  (which-key-declare-prefixes "SPC f" "file")
  (evil-leader/set-key
    "ff" 'find-file
    "fs" 'save-buffer)

  (which-key-declare-prefixes "SPC b" "buffer")
  (evil-leader/set-key
    "TAB" 'mode-line-other-buffer
    "bb" 'ivy-switch-buffer
    "bs" 'save-buffer
    "bd" 'evil-delete-buffer
    "bk" 'kill-buffer
    "bp" 'previous-buffer
    "bn" 'next-buffer
    "bh" 'switch-to-dashboard
    "bm" 'switch-to-messages
    "bs" 'switch-to-scratch
    "bq" 'close-all-buffers)

  (which-key-declare-prefixes "SPC w" "window")
  (evil-leader/set-key
    "wd" 'delete-window
    "wh" 'evil-window-left
    "wl" 'evil-window-right
    "w/" 'split-window-horizontally
    "w-" 'split-window-vertically
    "wo" 'delete-other-windows
    "wx" 'kill-buffer-and-window)

  (which-key-declare-prefixes "SPC c" "comment")
  (evil-leader/set-key
    "cr" 'comment-or-uncomment-region
    "cl" 'comment-line)

  (which-key-declare-prefixes "SPC q" "quit")
  (evil-leader/set-key
    "qq" 'save-buffers-kill-terminal))

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
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-display-style 'fancy)
  (define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur))

(use-package counsel
  :after ivy
  :config
  (setq counsel-git-cmd "rg --files")
  (setq counsel-grep-base-command
	"rg --column --line-number --no-heading --smart-case --no-ignore --hidden --follow --color never %s %s")
  (setq counsel-rg-base-command
	"rg --column --line-number --no-heading --smart-case --no-ignore --hidden --follow --color never %s ."))

(use-package swiper :bind (("C-s" . swiper)))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (doom-vibrant)))
 '(custom-safe-themes
   (quote
    ("1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" default)))
 '(package-selected-packages
   (quote
    (company dashboard doom-modeline rainbow-delimiters evil-exchange evil-matchit evil-surround evil-numbers evil-visualstar evil-leader avy smex ido-vertical-mode doom-themes use-package which-key evil-visual-mark-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "simp" :family "Hack")))))
