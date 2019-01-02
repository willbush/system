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
  :keymaps '(normal visual)
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
  "n" '(:ignore t :which-key "narrow")
  "q" '(:ignore t :which-key "quit")
  "s" '(:ignore t :which-key "search")
  "S" '(:ignore t :which-key "spell-checking")
  "t" '(:ignore t :which-key "toggle")
  "w" '(:ignore t :which-key "window"))

(general-def
  :prefix "SPC b"
  :keymaps '(normal visual)
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
  :keymaps '(normal visual)
  "r" 'comment-or-uncomment-region
  "l" 'comment-line
  "p" 'poporg-dwim)

(general-def
  :prefix "SPC f"
  :keymaps '(normal visual)
  "f" 'counsel-find-file
  "s" 'save-buffer)

(general-def
  :prefix "SPC F"
  :keymaps '(normal visual)
  "d" 'delete-frame
  "D" 'delete-other-frames
  "i" 'iconify-frame
  "o" 'other-frame
  "n" 'make-frame
  "m" 'toggle-frame-maximized)

(general-def
  :prefix "SPC g"
  :keymaps '(normal visual)
  "s" 'magit-status)

(general-def
  :prefix "SPC h"
  :keymaps '(normal visual)
  "i" 'info
  "l" 'counsel-find-library
  "n"  'view-emacs-news
  "w"  'woman
  "d" '(:ignore t :which-key "describe")
  "P" '(:ignore t :which-key "profiler"))

(general-def
  :prefix "SPC h d"
  :keymaps '(normal visual)
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
  :keymaps '(normal visual)
  "s" 'profiler-start
  "k" 'profiler-stop
  "r" 'profiler-report
  "w" 'profiler-report-write-profile)

(general-def
  :prefix "SPC j"
  :keymaps '(normal visual)
  "j" 'avy-goto-char
  "f" 'find-function
  "v" 'find-variable)

(general-def
  :prefix "SPC n"
  :keymaps '(normal visual)
  "r" 'narrow-to-region
  "p" 'narrow-to-page
  "f" 'narrow-to-defun
  "w" 'widen)

(general-def
  :prefix "SPC q"
  :keymaps '(normal visual)
  "q" 'save-buffers-kill-terminal
  "Q" 'save-buffers-kill-emacs)

(general-def
  :prefix "SPC s"
  :keymaps '(normal visual)
  "s" 'swiper
  "c" 'my/evil-search-clear-highlight
  "d" 'deadgrep
  "D" 'my/counsel-rg-directory
  "z" 'counsel-fzf)

(general-def
  :prefix "SPC S"
  :keymaps '(normal visual)
  "b" 'flyspell-buffer
  "c" 'flyspell-correct-at-point
  "n" 'evil-next-flyspell-error
  "p" 'evil-prev-flyspell-error)

(general-def
  :prefix "SPC t"
  :keymaps '(normal visual)
  "t" 'display-time-mode
  "l" 'toggle-truncate-lines
  "f" 'auto-fill-mode
  "n" 'linum-mode
  "g" 'my/toggle-golden-ratio
  "s" 'flyspell-mode)

(general-def
  :prefix "SPC w"
  :keymaps '(normal visual)
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

(provide 'init-keys)
