;; increase GC threshold in doom style
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

(add-hook 'after-init-hook
	  (lambda () (message "init time: %s" (emacs-init-time))))

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package evil
  :ensure t
  :defer .1 ;; load evil after startup
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package avy :ensure t :commands avy-goto-char)

(use-package evil-leader
  :ensure t
  :init (global-evil-leader-mode))

(evil-leader/set-leader "<SPC>")

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(evil-leader/set-key
  "<SPC>" 'smex ;; M-x
  "j" 'avy-goto-char)

(which-key-declare-prefixes "SPC f" "file")
(evil-leader/set-key
  "ff" 'find-file)

(which-key-declare-prefixes "SPC b" "buffer")
(evil-leader/set-key
  "bb" 'ido-switch-buffer
  "bs" 'save-buffer
  "bd" 'evil-delete-buffer
  "bk" 'kill-buffer
  "bh" 'previous-buffer
  "bl" 'next-buffer
  "bq" 'close-all-buffers)

(which-key-declare-prefixes "SPC w" "window")
(evil-leader/set-key
  "wd" 'delete-window
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "w/" 'split-window-horizontally
  "w-" 'split-window-vertically
  "wo" 'delete-other-windows)

(which-key-declare-prefixes "SPC c" "comment")
(evil-leader/set-key
  "cr" 'comment-or-uncomment-region
  "cl" 'comment-line)

(which-key-declare-prefixes "SPC q" "quit")
(evil-leader/set-key
  "qq" 'save-buffers-kill-terminal)

;; Enables searching via * on a visual selection.
(use-package evil-visualstar
  :after evil
  :ensure t
  :init (global-evil-visualstar-mode))

;; Enables inc/dec of numbers!
(use-package evil-numbers :after evil :ensure t)

(global-set-key (kbd "C-c =") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :after evil
  :ensure t
  :config (global-evil-matchit-mode 1))

(use-package evil-exchange
  :after evil
  :ensure t
  :config (evil-exchange-cx-install))

;; MINOR SETTINGS:

;; removes gui elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

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

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; IDO

(setq ido-enable-flex-matching t
      ido-case-fold t
      ido-everywhere t
      ido-create-new-buffer 'always
      ido-enable-tramp-completion t)

(ido-mode 1)

(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode 1)
	(setq ido-vertical-define-keys 'C-n-and-C-p-only))

;; enables IDO when using M-x
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex))

(use-package doom-themes :ensure t)

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
    (evil-exchange evil-matchit evil-surround evil-numbers evil-visualstar evil-leader avy smex ido-vertical-mode doom-themes use-package which-key evil-visual-mark-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "simp" :family "Hack")))))
