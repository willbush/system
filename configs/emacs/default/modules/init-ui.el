;;; -*- lexical-binding: t; -*-
;; Also see `early-init-file' because it sets UI elements early.

;; Set `doom-themes' early to prevent non-stylized UI flash.
(use-package doom-themes
  :config
  (load-theme 'doom-Iosvkem t))


;; Set `doom-modeline' early to prevent non-stylized UI flash.
(use-package doom-modeline
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-root)

  (doom-modeline-mode)
  (size-indication-mode)
  (column-number-mode))


;; Highlight matching parentheses
(use-package paren
  :hook (prog-mode . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package pulsar
  :hook
  (after-init . pulsar-global-mode)
  (next-error . pulsar-pulse-line)
  :config
  (setq pulsar-pulse t
        pulsar-delay 0.05
        pulsar-iterations 7
        pulsar-face 'pulsar-magenta
        pulsar-highlight-face 'pulsar-yellow)

  (setq pulse-functions
        '(
          evil-avy-goto-char-timer
          evil-goto-first-line
          evil-goto-line
          evil-mouse-drag-region
          evil-scroll-down
          evil-scroll-up
          evil-window-bottom
          evil-window-down
          evil-window-left
          evil-window-middle
          evil-window-right
          evil-window-top
          evil-window-up
          mode-line-other-buffer
          next-multiframe-window))

  (dolist (f pulse-functions)
    (add-to-list 'pulsar-pulse-functions f)))


(provide 'init-ui)
