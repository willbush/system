;;; -*- lexical-binding: t; -*-

(use-package edwina
  :hook (after-init . edwina-mode)
  :config

  ;; Fixes zoom so that the master window is selected after zooming from a
  ;; non-master window. see: https://github.com/ajgrf/edwina/issues/7
  (defun my/edwina-zoom ()
    "Zoom/cycle the selected window to/from master area."
    (interactive)
    (if (eq (selected-window) (frame-first-window))
        (edwina-swap-next-window)
      (let ((pane (edwina-pane (selected-window))))
        (edwina-delete-window)
        (edwina-arrange (cons pane (edwina-pane-list)))
        ;; switch to master window
        (select-window (car (edwina--window-list))))))

  (general-def
    :states '(normal visual emacs)
    :keymaps 'override
    ;; Xmonad style keybindings
    "M-," 'edwina-inc-nmaster
    "M-." 'edwina-dec-nmaster
    "M-S-RET" 'edwina-clone-window
    "<M-S-return>" 'edwina-clone-window
    "M-C" 'edwina-delete-window
    "M-N" 'edwina-swap-next-window
    "M-E" 'edwina-swap-previous-window
    "M-n" 'edwina-select-next-window
    "M-e" 'edwina-select-previous-window
    "M-i" 'edwina-inc-mfact
    "M-m" 'edwina-dec-mfact
    "M-RET" 'my/edwina-zoom))

(use-package eyebrowse
  :hook (after-init . eyebrowse-mode)
  :config
  (setq eyebrowse-wrap-around t
        eyebrowse-new-workspace t
        eyebrowse-default-workspace-slot 0)

  ;; When these keys are easy to get reach all the other functions provided by
  ;; eyebrowse become way less useful. Therefore, this is all I feel I need to
  ;; bind.
  (general-def
    :states '(normal visual emacs)
    :keymaps 'override
    ;; I'm binding these like I do in Xmonad
    "M-q" 'eyebrowse-switch-to-window-config-0
    "M-w" 'eyebrowse-switch-to-window-config-1
    "M-f" 'eyebrowse-switch-to-window-config-2
    "M-p" 'eyebrowse-switch-to-window-config-3
    "M-b" 'eyebrowse-switch-to-window-config-4
    "M-a" 'eyebrowse-switch-to-window-config-5
    "M-r" 'eyebrowse-switch-to-window-config-6
    "M-s" 'eyebrowse-switch-to-window-config-7
    "M-t" 'eyebrowse-switch-to-window-config-8
    "M-g" 'eyebrowse-switch-to-window-config-9))

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
