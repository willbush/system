;;; -*- lexical-binding: t; -*-

(use-package edwina
  :hook (after-init . edwina-mode)
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))

  (general-def
    :states '(normal visual emacs)
    :keymaps 'override
    ;; Xmonad style keybindings
    "M-," 'edwina-inc-nmaster
    "M-." 'edwina-dec-nmaster
    ;; I can't bind this to M-S-RET unfortunately.
    "M-c" 'edwina-clone-window
    "M-C" 'edwina-delete-window
    "M-N" 'edwina-swap-next-window
    "M-E" 'edwina-swap-previous-window
    "M-n" 'edwina-select-next-window
    "M-e" 'edwina-select-previous-window
    "M-i" 'edwina-inc-mfact
    "M-m" 'edwina-dec-mfact
    ;; I can't bind this to M-S-SPC unfortunately.
    "M-a" 'edwina-arrange
    "M-RET" 'edwina-zoom))

(use-package golden-ratio
  :commands (golden-ratio golden-ratio-mode)
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

(use-package adaptive-wrap
  :commands adaptive-wrap-prefix-mode)

(use-package visual-fill-column
  :commands visual-fill-column-mode)

(provide 'init-win-buffer-tools)
