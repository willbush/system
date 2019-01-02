;;; -*- lexical-binding: t; -*-

(defun my/open-shell ()
  "Opens my prefered shell for the current operating system."
  (interactive)
  (if (eq system-type 'windows-nt)
      (call-interactively 'eshell)
    (call-interactively 'ansi-term)))

(use-package which-key
  :config (which-key-mode 1))

(use-package general)

;; Global Bindings
(general-def
  ;; zoom in and out
  "C-=" 'text-scale-increase
  "C--" 'text-scale-decrease
  ;; increment and decrement number at point.
  "M-=" 'evil-numbers/inc-at-pt
  "M--" 'evil-numbers/dec-at-pt)

(general-def
  :prefix "SPC"
  :keymaps 'normal
  "SPC" 'counsel-M-x
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
  "b" '(:ignore t :which-key "buffer")
  "c" '(:ignore t :which-key "comment")
  "f" '(:ignore t :which-key "file")
  "F" '(:ignore t :which-key "frame")
  "g" '(:ignore t :which-key "git")
  "h" '(:ignore t :which-key "help")
  "j" '(:ignore t :which-key "jump")
  "m" '(:ignore t :which-key "mode")
  "n" '(:ignore t :which-key "narrow")
  "q" '(:ignore t :which-key "quit")
  "s" '(:ignore t :which-key "search")
  "S" '(:ignore t :which-key "spell-checking")
  "t" '(:ignore t :which-key "toggle")
  "w" '(:ignore t :which-key "window"))

(general-def
  :prefix "SPC b"
  :keymaps 'normal
  "b" 'ivy-switch-buffer
  "s" 'save-buffer
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
  :keymaps 'normal
  "r" 'comment-or-uncomment-region
  "l" 'comment-line
  "p" 'poporg-dwim)

(general-def
  :prefix "SPC f"
  :keymaps 'normal
  "f" 'counsel-find-file
  "s" 'save-buffer)

(general-def
  :prefix "SPC F"
  :keymaps 'normal
  "d" 'delete-frame
  "D" 'delete-other-frames
  "i" 'iconify-frame
  "o" 'other-frame
  "n" 'make-frame
  "m" 'toggle-frame-maximized)

(general-def
  :prefix "SPC g"
  :keymaps 'normal
  "s" 'magit-status)

(general-def
  :prefix "SPC h"
  :keymaps 'normal
  "i" 'info
  "l" 'counsel-find-library
  "n"  'view-emacs-news
  "w"  'woman
  "d" '(:ignore t :which-key "describe")
  "P" '(:ignore t :which-key "profiler"))

(general-def
  :prefix "SPC h d"
  :keymaps 'normal
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
  :prefix "SPC h P"
  :keymaps 'normal
  "s" 'profiler-start
  "k" 'profiler-stop
  "r" 'profiler-report
  "w" 'profiler-report-write-profile)

(general-def
  :prefix "SPC j"
  :keymaps 'normal
  "j" 'avy-goto-char
  "f" 'find-function
  "v" 'find-variable)

(general-def
  :prefix "SPC n"
  :keymaps 'normal
  "r" 'narrow-to-region
  "p" 'narrow-to-page
  "f" 'narrow-to-defun
  "w" 'widen)

(general-def
  :prefix "SPC q"
  :keymaps 'normal
  "q" 'save-buffers-kill-terminal
  "Q" 'save-buffers-kill-emacs)

(general-def
  :prefix "SPC s"
  :keymaps 'normal
  "s" 'swiper
  "c" 'my/evil-search-clear-highlight
  "d" 'deadgrep
  "D" 'my/counsel-rg-directory
  "z" 'counsel-fzf)

(general-def
  :prefix "SPC S"
  :keymaps 'normal
  "b" 'flyspell-buffer
  "c" 'flyspell-correct-at-point
  "n" 'evil-next-flyspell-error
  "p" 'evil-prev-flyspell-error)

(general-def
  :prefix "SPC t"
  :keymaps 'normal
  "t" 'display-time-mode
  "l" 'toggle-truncate-lines
  "f" 'auto-fill-mode
  "n" 'linum-mode
  "g" 'my/toggle-golden-ratio
  "s" 'flyspell-mode)

(general-def
  :prefix "SPC w"
  :keymaps 'normal
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

;; C# mode
(general-def
  :prefix "SPC m"
  :states 'normal
  :keymaps 'csharp-mode-map
  "e" 'omnisharp-solution-errors
  "o" 'omnisharp-show-overloads-at-point
  "i" 'omnisharp-find-implementations
  "g" 'omnisharp-go-to-definition
  "G" 'omnisharp-go-to-definition-other-window
  "s" 'omnisharp-stop-server
  "c" 'omnisharp-check-alive-status
  "R" 'omnisharp-reload-solution
  "r" '(:ignore t :which-key "refactor")
  "n" '(:ignore t :which-key "navigate")
  "t" 'omnisharp-unit-test-buffer
  "u" 'omnisharp-fix-usings)

(general-def
  :prefix "SPC m r"
  :states 'normal
  :keymaps 'csharp-mode-map
  "r" 'omnisharp-rename
  "a" 'omnisharp-run-code-action-refactoring)

(general-def
  :prefix "SPC m n"
  :states 'normal
  :keymaps 'csharp-mode-map
  "r" 'omnisharp-navigate-to-region
  "f" 'omnisharp-navigate-to-solution-file
  "m" 'omnisharp-navigate-to-solution-member)

;; haskell mode
(general-def
  :prefix "SPC m"
  :states 'normal
  :keymaps 'haskell-mode-map
  "e" 'hasky-stack-execute
  "p" 'hasky-stack-package-action
  "i" 'dante-info
  "r" '(:ignore t :which-key "refactor"))

(general-def
  :prefix "SPC m r"
  :states 'normal
  :keymaps 'haskell-mode-map
  ;; "B" 'hlint-refactor-refactor-buffer
  ;; "p" 'hlint-refactor-refactor-at-point
  "b" 'hindent-reformat-buffer
  "r" 'hindent-reformat-region)

(provide 'init-keys)
