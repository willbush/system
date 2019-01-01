;;; -*- lexical-binding: t; -*-

(use-package which-key
  :config (which-key-mode 1))

(use-package evil-leader
  ;; after which key so I can keep its prefix declarations next to
  ;; evil-leader declarations in this config block.
  :after which-key
  :init
  ;; evil-collection will give a warning if the following setting is not set
  ;; before loading evil-leader, evil, and evil-collection.
  ;; see: https://github.com/emacs-evil/evil-collection/issues/215
  (setq evil-want-keybinding nil)
  (global-evil-leader-mode)
  :config

  (evil-leader/set-leader "<SPC>")

  (evil-leader/set-key
    "<SPC>" 'counsel-M-x
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
    "?" 'counsel-descbinds)

  (which-key-declare-prefixes "SPC b" "buffer")
  (evil-leader/set-key
    "TAB" 'mode-line-other-buffer
    "bb" 'ivy-switch-buffer
    "bs" 'save-buffer
    "bd" 'my/kill-this-buffer
    "bD" 'my/kill-all-buffers
    "bk" 'kill-buffer ;; requests buffer to kill
    "bp" 'previous-buffer
    "bn" 'next-buffer
    "bh" 'my/switch-to-dashboard
    "bm" 'my/switch-to-messages
    "bs" 'my/switch-to-scratch)

  (which-key-declare-prefixes "SPC c" "comment")
  (evil-leader/set-key
    "cr" 'comment-or-uncomment-region
    "cl" 'comment-line
    "cp" 'poporg-dwim)

  (which-key-declare-prefixes "SPC f" "file")
  (evil-leader/set-key
    "ff" 'counsel-find-file
    "fs" 'save-buffer)

  (which-key-declare-prefixes "SPC F" "frame")
  (evil-leader/set-key
    "Fd" 'delete-frame
    "FD" 'delete-other-frames
    "Fi" 'iconify-frame
    "Fo" 'other-frame
    "Fn" 'make-frame
    "Fm" 'toggle-frame-maximized
    "Ff" 'toggle-frame-fullscreen)

  (which-key-declare-prefixes "SPC g" "git")
  (evil-leader/set-key
    "gs" 'magit-status)

  (which-key-declare-prefixes "SPC h" "help")
  (which-key-declare-prefixes "SPC hd" "describe")
  (evil-leader/set-key
    "hi" 'info
    "hl" 'counsel-find-library
    "hn"  'view-emacs-news
    "hw"  'woman
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
    "qq" 'save-buffers-kill-terminal
    "qQ" 'save-buffers-kill-emacs)

  (which-key-declare-prefixes "SPC s" "search")
  (evil-leader/set-key
    "ss" 'swiper
    "sc" 'my/evil-search-clear-highlight
    "sd" 'deadgrep
    "sD" 'my/counsel-rg-directory
    "sz" 'counsel-fzf)

  (which-key-declare-prefixes "SPC S" "spell-checking")
  (evil-leader/set-key
    "Sb" 'flyspell-buffer
    "Sc" 'flyspell-correct-at-point
    "Sn" 'evil-next-flyspell-error
    "Sp" 'evil-prev-flyspell-error)

  (which-key-declare-prefixes "SPC t" "toggle")
  (evil-leader/set-key
    "tt" 'display-time-mode
    "tl" 'toggle-truncate-lines
    "tf" 'auto-fill-mode
    "tn" 'linum-mode
    "tg" 'my/toggle-golden-ratio
    "ts" 'flyspell-mode)

  (which-key-declare-prefixes "SPC w" "window")
  (evil-leader/set-key
    "wb" 'balance-windows
    "wB" 'balance-windows-area
    "wd" 'delete-window
    "wg" 'golden-ratio
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
    "wx" 'kill-buffer-and-window)

  (which-key-declare-prefixes-for-mode 'csharp-mode
    "SPC m" "mode"
    "SPC mr" "refactor"
    "SPC mn" "navigate")

  (evil-leader/set-key-for-mode 'csharp-mode
    "me" 'omnisharp-solution-errors
    "mo" 'omnisharp-show-overloads-at-point
    "mi" 'omnisharp-find-implementations
    "mg" 'omnisharp-go-to-definition
    "mG" 'omnisharp-go-to-definition-other-window
    "ms" 'omnisharp-stop-server
    "mc" 'omnisharp-check-alive-status
    "mR" 'omnisharp-reload-solution
    "mrr" 'omnisharp-rename
    "mra" 'omnisharp-run-code-action-refactoring
    "mnr" 'omnisharp-navigate-to-region
    "mnf" 'omnisharp-navigate-to-solution-file
    "mnm" 'omnisharp-navigate-to-solution-member
    "mt" 'omnisharp-unit-test-buffer
    "mu" 'omnisharp-fix-usings)

  (which-key-declare-prefixes-for-mode 'haskell-mode
    "SPC m" "mode"
    "SPC mr" "refactor"
    "SPC mn" "navigate")

  (evil-leader/set-key-for-mode 'haskell-mode
     "e" 'hasky-stack-execute
     "p" 'hasky-stack-package-action
     "rb" 'hindent-reformat-buffer
     "rB" 'hlint-refactor-refactor-buffer
     "rr" 'hindent-reformat-region
     "rp" 'hlint-refactor-refactor-at-point)
  )

(defun my/open-shell ()
  "Opens my prefered shell for the current operating system."
  (interactive)
  (if (eq system-type 'windows-nt)
      (call-interactively 'eshell)
    (call-interactively 'ansi-term)))

(provide 'init-keys)
