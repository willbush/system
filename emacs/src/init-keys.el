;;; -*- lexical-binding: t; -*-

(use-package which-key
  :config (which-key-mode 1))

(use-package general)

;; Global Bindings
(general-def
  "C-S-u" 'universal-argument
  ;; zoom in and out
  "C-=" 'text-scale-increase
  "C--" 'text-scale-decrease
  ;; increment and decrement number at point.
  "M-=" 'evil-numbers/inc-at-pt
  "M--" 'evil-numbers/dec-at-pt)

;; pressing v again after going into visual mode will enter
;; a hydra for expand-region usage
(general-def
  :states 'visual
  "v" 'hydra-expand-region/body)

(general-def
  :prefix "SPC"
  :states '(normal visual)
  :keymaps 'override
  "SPC" 'counsel-M-x
  "!" 'shell-command
  "'" 'my/open-shell
  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  "5" 'winum-select-window-5
  "6" 'winum-select-window-6
  "7" 'winum-select-window-7
  "8" 'winum-select-window-8
  "9" 'winum-select-window-9
  "?" 'counsel-descbinds
  "TAB" 'mode-line-other-buffer
  "a" '(:ignore t :which-key "apps")
  "b" '(:ignore t :which-key "buffer")
  "c" '(:ignore t :which-key "comment")
  "f" '(:ignore t :which-key "file")
  "F" '(:ignore t :which-key "frame")
  "g" '(:ignore t :which-key "git")
  "h" '(:ignore t :which-key "help")
  "j" '(:ignore t :which-key "jump")
  "n" '(:ignore t :which-key "narrow")
  "p" 'projectile-command-map
  "q" '(:ignore t :which-key "quit")
  "s" '(:ignore t :which-key "search")
  "S" '(:ignore t :which-key "spell-checking")
  "t" '(:ignore t :which-key "toggle")
  "w" '(:ignore t :which-key "window")
  "x" '(:ignore t :which-key "text manipulation"))

(general-def
  :prefix "SPC a"
  :states '(normal visual)
  :keymaps 'override
  "d" 'deer ;; minimal ranger dired
  "D" 'deer-jump-other-window
  "p" '(:ignore t :which-key "profiler")
  "r" 'ranger
  "s" 'speedbar
  "u" 'disk-usage
  "w" 'wttrin)

(general-def
  :prefix "SPC a p"
  :states '(normal visual)
  :keymaps 'override
  "s" 'profiler-start
  "k" 'profiler-stop
  "r" 'profiler-report
  "w" 'profiler-report-write-profile)

(general-def
  :prefix "SPC b"
  :states '(normal visual)
  :keymaps 'override
  "b" 'ivy-switch-buffer
  "d" 'my/kill-this-buffer
  "D" 'my/kill-all-buffers
  "k" 'kill-buffer ;; requests buffer to kill
  "p" 'previous-buffer
  "n" 'next-buffer
  "h" 'my/switch-to-dashboard
  "m" 'my/switch-to-messages
  "s" 'my/switch-to-scratch)

(general-def
  :prefix "SPC c"
  :states '(normal visual)
  :keymaps 'override
  "r" 'comment-or-uncomment-region
  "l" 'comment-line
  "p" 'poporg-dwim)

(general-def
  :prefix "SPC f"
  :states '(normal visual)
  :keymaps 'override
  "d" 'fd-dired
  "f" 'counsel-find-file
  "s" 'save-buffer
  "z" 'counsel-fzf)

(general-def
  :prefix "SPC F"
  :states '(normal visual)
  :keymaps 'override
  "d" 'delete-frame
  "D" 'delete-other-frames
  "i" 'iconify-frame
  "o" 'other-frame
  "n" 'make-frame
  "m" 'toggle-frame-maximized)

(general-def
  :prefix "SPC g"
  :states '(normal visual)
  :keymaps 'override
  "c" 'magit-clone
  "i" 'magit-gitignore-globally
  "I" 'magit-init
  "s" 'magit-status
  "m" 'magit-dispatch-popup
  "t" 'git-timemachine)

(general-def
  :prefix "SPC h"
  :states '(normal visual)
  :keymaps 'override
  "i" 'info
  "I" 'info-display-manual
  "l" 'counsel-find-library
  "n"  'view-emacs-news
  "w"  'woman
  "d" '(:ignore t :which-key "describe")
  "t" 'evil-tutor-start)

(general-def
  :prefix "SPC h d"
  :states '(normal visual)
  :keymaps 'override
  "b" 'counsel-descbinds
  "c" 'describe-char
  "f" 'counsel-describe-function
  "k" 'describe-key
  "m" 'describe-mode
  "p" 'describe-package
  "v" 'counsel-describe-variable
  "t" 'describe-theme
  "s" 'counsel-info-lookup-symbol)

(general-def
  :prefix "SPC j"
  :states '(normal visual)
  :keymaps 'override
  "j" 'avy-goto-char-timer
  "f" 'find-function
  "v" 'find-variable)

(general-def
  :prefix "SPC n"
  :states '(normal visual)
  :keymaps 'override
  "r" 'narrow-to-region
  "p" 'narrow-to-page
  "f" 'narrow-to-defun
  "w" 'widen)

(general-def
  :prefix "SPC q"
  :states '(normal visual)
  :keymaps 'override
  "q" 'save-buffers-kill-terminal
  "Q" 'save-buffers-kill-emacs)

(general-def
  :prefix "SPC s"
  :states '(normal visual)
  :keymaps 'override
  "s" 'swiper
  "c" 'my/evil-search-clear-highlight
  "d" 'deadgrep
  "D" 'my/counsel-rg-directory
  "z" 'counsel-fzf)

(general-def
  :prefix "SPC S"
  :states '(normal visual)
  :keymaps 'override
  "b" 'flyspell-buffer
  "c" 'flyspell-correct-at-point
  "n" 'evil-next-flyspell-error
  "p" 'evil-prev-flyspell-error)

(general-def
  :prefix "SPC t"
  :states '(normal visual)
  :keymaps 'override
  "c" 'fci-mode
  "f" 'auto-fill-mode
  "g" 'my/toggle-golden-ratio
  "l" 'toggle-truncate-lines
  "n" 'linum-mode
  "s" 'flyspell-mode
  "t" 'display-time-mode
  "v" 'visual-fill-column-mode)

(general-def
  :prefix "SPC w"
  :states '(normal visual)
  :keymaps 'override
  "b" 'balance-windows
  "B" 'balance-windows-area
  "d" 'delete-window
  "g" 'golden-ratio
  "h" 'evil-window-left
  "l" 'evil-window-right
  "j" 'evil-window-down
  "k" 'evil-window-up
  "H" 'evil-window-move-far-left
  "L" 'evil-window-move-far-right
  "J" 'evil-window-move-far-down
  "K" 'evil-window-move-far-up
  "/" 'split-window-horizontally
  "-" 'split-window-vertically
  "o" 'delete-other-windows
  "m" 'my/toggle-maximize-window
  "x" 'kill-buffer-and-window)

(general-def
  :prefix "SPC x"
  :states '(normal visual)
  :keymaps 'override
  "d" 'define-word-at-point
  "l" 'my/sort-lines
  "L" 'my/sort-lines-reverse
  "c" 'my/sort-lines-by-column
  "C" 'my/sort-lines-by-column-reverse
  "u" 'my/uniquify-lines)

(provide 'init-keys)
