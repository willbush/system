;;; -*- lexical-binding: t; -*-

(use-package ibuffer :bind ("C-x C-b" . ibuffer))

(use-package winum :hook (after-init . winum-mode))

(use-package golden-ratio
  :defer t
  :config
  ;; extra golden ratio commands
  (dolist (cs '(avy-pop-mark
                evil-avy-goto-word-or-subword-1
                evil-avy-goto-line
                evil-window-delete
                evil-window-split
                evil-window-vsplit
                evil-window-left
                evil-window-right
                evil-window-up
                evil-window-down
                evil-window-bottom-right
                evil-window-top-left
                evil-window-mru
                evil-window-next
                evil-window-prev
                evil-window-new
                evil-window-vnew
                evil-window-rotate-upwards
                evil-window-rotate-downwards
                evil-window-move-very-top
                evil-window-move-far-left
                evil-window-move-far-right
                evil-window-move-very-bottom
                next-multiframe-window
                previous-multiframe-window
                winum-select-window-0-or-10
                winum-select-window-1
                winum-select-window-2
                winum-select-window-3
                winum-select-window-4
                winum-select-window-5
                winum-select-window-6
                winum-select-window-7
                winum-select-window-8
                winum-select-window-9
                windmove-left
                windmove-right
                windmove-up
                windmove-down
                quit-window))

    (add-to-list 'golden-ratio-extra-commands cs))

  ;; modes for golden-ratio to exclude
  (dolist (ms '("bs-mode"
                "calc-mode"
                "ediff-mode"
                "eshell-mode"
                "dired-mode"
                "gud-mode"
                "gdb-locals-mode"
                "gdb-registers-mode"
                "gdb-breakpoints-mode"
                "gdb-threads-mode"
                "gdb-frames-mode"
                "gdb-inferior-io-mode"
                "gdb-disassembly-mode"
                "gdb-memory-mode"
                "speedbar-mode"))

    (add-to-list 'golden-ratio-exclude-modes ms)))

(use-package dashboard
  :hook (after-init . dashboard-setup-startup-hook)
  :config
  (setq dashboard-banner-logo-title (format "init time: %s" (emacs-init-time)))
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 5))))

(use-package visual-fill-column
  :config
  (add-hook 'visual-fill-column-mode-hook #'visual-line-mode))

(provide 'init-win-buffer-tools)
