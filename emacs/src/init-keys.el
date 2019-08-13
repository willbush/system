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
  :states 'visual
  "r" 'hydra-expand-region/body)

(general-def
  :prefix "SPC"
  :states '(normal visual)
  :keymaps 'override
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
  "SPC" 'counsel-M-x
  "TAB" 'mode-line-other-buffer
  ;; Not using 'b' as a prefix because it's too hard to reach on Colemak-DH and
  ;; too widely used.
  "." '(:ignore t :which-key "buffer")
  "F" '(:ignore t :which-key "frame")
  "S" '(:ignore t :which-key "spell-checking")
  "a" '(:ignore t :which-key "apps")
  "c" '(:ignore t :which-key "comment")
  "f" '(:ignore t :which-key "file")
  "g" '(:ignore t :which-key "go")
  "h" '(:ignore t :which-key "help")
  "m" '(:ignore t :which-key "magit")
  "n" '(:ignore t :which-key "narrow")
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
  "d" 'deer ;; minimal ranger dired
  "f" 'elfeed
  "p" '(:ignore t :which-key "profiler")
  "r" 'ranger
  "s" 'speedbar
  "u" 'disk-usage
  "w" 'wttrin)

(general-def
  :prefix "SPC a p"
  :states '(normal visual)
  :keymaps 'override
  "k" 'profiler-stop
  "r" 'profiler-report
  "s" 'profiler-start
  "w" 'profiler-report-write-profile)

(general-def
  :prefix "SPC ."
  :states '(normal visual)
  :keymaps 'override
  "." 'ivy-switch-buffer
  "D" 'my/kill-all-buffers
  "d" 'my/kill-this-buffer
  "h" 'my/switch-to-dashboard
  "k" 'kill-buffer ;; requests buffer to kill
  "m" 'my/switch-to-messages
  "s" 'my/switch-to-scratch)

(general-def
  :prefix "SPC n"
  :states '(normal visual)
  :keymaps 'override
  "f" 'narrow-to-defun
  "p" 'narrow-to-page
  "r" 'narrow-to-region
  "w" 'widen)

(general-def
  :prefix "SPC c"
  :states '(normal visual)
  :keymaps 'override
  "l" 'comment-line
  "p" 'poporg-dwim
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
  "I" 'magit-init
  "c" 'magit-clone
  "d" 'magit-dispatch
  "i" 'magit-gitignore-globally
  "m" 'magit-status
  "t" 'git-timemachine)

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
  "k" 'describe-key
  "m" 'describe-mode
  "p" 'describe-package
  "s" 'counsel-info-lookup-symbol
  "t" 'describe-theme
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
  "s" 'swiper
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
  "n" 'display-line-numbers-mode
  "s" 'flyspell-mode
  "t" 'display-time-mode
  "v" 'visual-fill-column-mode)

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

(provide 'init-keys)
