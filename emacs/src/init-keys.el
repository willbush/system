;;; -*- lexical-binding: t; -*-

(use-package which-key
  :config (which-key-mode 1))

(use-package general)

;; Global Bindings
(general-def
  ;; zoom in and out
  "C-+" 'text-scale-increase
  "C--" 'text-scale-decrease
  ;; increment and decrement number at point.
  "M-+" 'evil-numbers/inc-at-pt
  "M--" 'evil-numbers/dec-at-pt)

;; pressing r again after going into range mode (visual mode) will enter a hydra
;; for expand-region usage
(general-def
  :keymaps 'evil-visual-state-map
  "r" 'hydra-expand-region/body)

(general-def
  :prefix "SPC"
  :states '(normal visual)
  :keymaps 'override
  "!" 'shell-command
  "'" 'my/open-shell
  "?" 'counsel-descbinds
  "SPC" 'counsel-M-x
  "TAB" 'mode-line-other-buffer
  ;; Not using 'b' as a prefix because it's too hard to reach on Colemak-DHm and
  ;; too widely used.
  "F" '(:ignore t :which-key "frame")
  "S" '(:ignore t :which-key "spell-checking")
  "a" '(:ignore t :which-key "apps")
  "b" '(:ignore t :which-key "buffer")
  "c" '(:ignore t :which-key "comment")
  "f" '(:ignore t :which-key "file")
  "g" '(:ignore t :which-key "go")
  "h" '(:ignore t :which-key "help")
  "m" '(:ignore t :which-key "magit")
  "n" '(:ignore t :which-key "narrow")
  "o" '(:ignore t :which-key "org")
  "p" 'projectile-command-map
  "q" '(:ignore t :which-key "quit")
  "r" '(:ignore t :which-key "rapid")
  "s" '(:ignore t :which-key "search")
  "t" '(:ignore t :which-key "toggle")
  "w" '(:ignore t :which-key "window")
  "x" '(:ignore t :which-key "text manipulation"))

(general-def
  :prefix "SPC a"
  :states '(normal visual)
  :keymaps 'override
  "D" 'deer-jump-other-window
  "E" 'esup
  "d" 'deer ;; minimal ranger dired
  "e" '(:ignore t :which-key "direnv")
  "f" 'elfeed
  "p" '(:ignore t :which-key "profiler")
  "r" 'ranger
  "s" 'speedbar
  "u" 'disk-usage
  "w" 'wttrin)

(general-def
  :prefix "SPC a e"
  :states '(normal visual)
  :keymaps 'override
  "a" 'direnv-allow
  "m" 'direnv-mode
  "U" 'direnv-update-directory-environment
  "u" 'direnv-update-environment)

(general-def
  :prefix "SPC a p"
  :states '(normal visual)
  :keymaps 'override
  "k" 'profiler-stop
  "r" 'profiler-report
  "s" 'profiler-start
  "w" 'profiler-report-write-profile)

(general-def
  :prefix "SPC b"
  :states '(normal visual)
  :keymaps 'override
  "D" 'my/kill-all-buffers
  "H" 'my/kill-all-buffers-then-switch-to-dashboard
  "I" 'ibuffer
  "b" 'ivy-switch-buffer
  "d" 'my/kill-this-buffer
  "h" 'my/switch-to-dashboard
  "i" 'counsel-ibuffer
  "k" 'kill-buffer ;; requests buffer to kill
  "m" 'my/switch-to-messages
  "r" 'my/revert-buffer-no-confirm
  "s" 'my/switch-to-scratch)

(general-def
  :prefix "SPC c"
  :states '(normal visual)
  :keymaps 'override
  "l" 'comment-line
  "r" 'comment-or-uncomment-region)

(general-def
  :prefix "SPC f"
  :states '(normal visual)
  :keymaps 'override
  "d" 'fd-dired
  "f" 'counsel-find-file
  "s" 'save-buffer
  "y" 'my/yank-and-show-buffer-full-path
  "z" 'counsel-fzf)

(general-def
  :prefix "SPC F"
  :states '(normal visual)
  :keymaps 'override
  "D" 'delete-other-frames
  "d" 'delete-frame
  "i" 'iconify-frame
  "m" 'toggle-frame-maximized
  "n" 'make-frame
  "o" 'other-frame)

(general-def
  :prefix "SPC g"
  :states '(normal visual)
  :keymaps 'override
  "f" 'find-function
  "g" 'avy-goto-char-timer
  "v" 'find-variable)

(general-def
  :prefix "SPC m"
  :states '(normal visual)
  :keymaps 'override
  "G" 'hydra-git-gutter/body
  "I" 'magit-init
  "c" 'magit-clone
  "d" 'magit-dispatch
  "g" 'counsel-git-grep
  "i" 'magit-gitignore-globally
  "l" 'counsel-git-log
  "m" 'magit-status
  "t" 'git-timemachine)

(general-def
  :prefix "SPC n"
  :states '(normal visual)
  :keymaps 'override
  "f" 'narrow-to-defun
  "p" 'narrow-to-page
  "r" 'narrow-to-region
  "w" 'widen)

(general-def
  :prefix "SPC o"
  :states '(normal visual)
  :keymaps 'override
  "S" 'org-store-link
  "a" 'org-agenda
  "c" 'org-capture
  "i" 'org-indent-mode
  "r" 'org-refile
  "s" 'org-save-all-org-buffers
  "t" 'org-toggle-link-display)

(general-def
  :prefix "SPC h"
  :states '(normal visual)
  :keymaps 'override
  "I" 'info-display-manual
  "d" '(:ignore t :which-key "describe")
  "i" 'info
  "l" 'counsel-find-library
  "n"  'view-emacs-news
  "t" 'evil-tutor-start
  "w"  'woman)

(general-def
  :prefix "SPC h d"
  :states '(normal visual)
  :keymaps 'override
  "B" 'evil-collection-describe-bindings
  "K" 'my/describe-keymap
  "b" 'counsel-descbinds
  "c" 'describe-char
  "f" 'counsel-describe-function
  "g" 'general-describe-keybindings
  "k" 'describe-key
  "m" 'describe-mode
  "p" 'describe-package
  "s" 'counsel-info-lookup-symbol
  "t" 'describe-theme
  "u" 'counsel-unicode-char
  "v" 'counsel-describe-variable)

(general-def
  :prefix "SPC q"
  :states '(normal visual)
  :keymaps 'override
  "Q" 'save-buffers-kill-emacs
  "q" 'save-buffers-kill-terminal)

;; Rapid keys emphasize easy access due to high usage over mnemonics.
(general-def
  :prefix "SPC r"
  :states '(normal visual)
  :keymaps 'override
  "s" 'save-buffer)

(general-def
  :prefix "SPC s"
  :states '(normal visual)
  :keymaps 'override
  "D" 'my/counsel-rg-directory
  "c" 'evil-ex-nohighlight ;; mnemonic is search clear
  "d" 'deadgrep
  "s" 'swiper-multi
  "z" 'counsel-fzf)

(general-def
  :prefix "SPC S"
  :states '(normal visual)
  :keymaps 'override
  "b" 'flyspell-buffer
  "c" 'flyspell-correct-wrapper
  "n" 'flyspell-correct-next
  "p" 'flyspell-correct-previous
  "r" 'flyspell-region)

(general-def
  :prefix "SPC t"
  :states '(normal visual)
  :keymaps 'override
  "c" 'fci-mode
  "f" 'auto-fill-mode
  "g" 'my/toggle-golden-ratio
  "l" 'toggle-truncate-lines
  "m" 'counsel-major ;; switches major mode
  "n" 'display-line-numbers-mode
  "s" 'flyspell-mode
  "t" 'display-time-mode
  "v" 'my/toggle-adaptive-visual-fill-column)

(general-def
  :prefix "SPC w"
  :states '(normal visual)
  :keymaps 'override
  "-" 'split-window-vertically
  "/" 'split-window-horizontally
  "B" 'balance-windows-area
  "E" 'evil-window-move-very-top
  "I" 'evil-window-move-far-right
  "M" 'evil-window-move-far-left
  "N" 'evil-window-move-very-bottom
  "b" 'balance-windows
  "d" 'delete-window
  "e" 'evil-window-up
  "g" 'golden-ratio
  "i" 'evil-window-right
  "k" 'kill-buffer-and-window
  "m" 'evil-window-left
  "n" 'evil-window-down
  "o" 'delete-other-windows
  "x" 'my/toggle-maximize-window)

(general-def
  :prefix "SPC x"
  :states '(normal visual)
  :keymaps 'override
  "C" 'my/sort-lines-by-column-reverse
  "L" 'my/sort-lines-reverse
  "c" 'my/sort-lines-by-column
  "d" 'define-word-at-point
  "l" 'my/sort-lines
  "u" 'my/uniquify-lines)

;; evil-collection will give a warning if the following setting is not set
;; before loading evil and evil-collection. Note that evil-leader loads evil
;; see: https://github.com/emacs-evil/evil-collection/issues/215 Also even if
;; this is in the :init block it will still given the warning when lazy loading
;; evil.
(setq evil-want-keybinding nil)

(defun my/custom-evil-collection-bindings (mode mode-keymaps &rest _rest)
  (cond ((eq mode 'dired)
         ;; dired key bindings
         (general-def
           :states 'normal
           :keymaps 'dired-mode-map
           ;; remove evil mode shadows
           "i" nil ;; was 'dired-toggle-read-only
           "m" nil ;; was 'dired-mark
           "j" nil ;; was 'dired-next-line
           "^" nil ;; was 'dired-up-directory
           "r" nil ;; was 'dired-do-redisplay
           "R" nil ;; was 'dired-do-rename
           ;; rebind things better to my custom evil keys
           "l" 'dired-toggle-read-only
           "k" 'dired-mark
           "n" 'dired-next-line
           "e" 'dired-previous-line
           "C-e" 'dired-up-directory
           "v" 'dired-do-rename))
        ;; default case make some blind key swaps for my custom evil keys.
        ;;
        ;; Note this does not work for `ediff-mode' because evil-collection
        ;; doesn't apply key bindings until after `ediff-startup-hook'.
        (t (evil-collection-swap-key 'normal mode-keymaps
             "m" "h" ;; left
             "n" "j" ;; down
             "e" "k" ;; up
             "i" "l" ;; right
             "r" "v" ;; range (old name visual)
             (kbd "C-n") (kbd "C-j")
             (kbd "C-e") (kbd "C-k")))))

;; A collection of evil key bindings for various modes
(use-package evil-collection
  :commands evil-collection-init
  :custom (evil-collection-setup-minibuffer t)
  :init
  (setq evil-collection-mode-list
        '(calendar
          (package-menu package)
          (pdf pdf-view)
          (term term ansi-term multi-term)
          compile
          cus-theme
          custom
          deadgrep
          debug
          dired
          disk-usage
          elfeed
          help
          info
          ivy
          man
          minibuffer
          woman))

  ;; called after evil-collection makes its keybindings
  ;; https://github.com/emacs-evil/evil-collection#key-translation
  (add-hook 'evil-collection-setup-hook #'my/custom-evil-collection-bindings))

(provide 'init-keys)
