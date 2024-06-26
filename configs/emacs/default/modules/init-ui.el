;;; -*- lexical-binding: t; -*-
;; Also see `early-init-file' because it sets UI elements early.

;; Set `doom-themes' early to prevent non-stylized UI flash.
(use-package doom-themes
  :config
  (load-theme 'doom-badger t))


;; Set `doom-modeline' early to prevent non-stylized UI flash.
(use-package doom-modeline
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-root)

  (doom-modeline-mode)
  (size-indication-mode)
  (column-number-mode))


;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session, where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))


;; Highlight matching parentheses
(use-package paren
  :ensure nil
  :hook (prog-mode . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package fill-column-indicator
  :commands fci-mode)


(use-package nav-flash
  :commands nav-flash-show
  :preface

  (defun my/maybe-nav-flash (&rest _)
    "Performs the `nav-flash-show' in current window when in the correct context."
    (unless (or (minibufferp) (derived-mode-p 'term-mode))
      (nav-flash-show)
      ;; only show in the current window
      (overlay-put compilation-highlight-overlay 'window (selected-window))))

  :hook ((bookmark-after-jump
          imenu-after-jump
          evil-jumps-post-jump
          xref-after-jump
          org-follow-link
          xref-after-return) . my/maybe-nav-flash)

  :init

  ;; This used to be done using the focus-in-hook, but it was deprecated.
  (add-function :after after-focus-change-function #'my/maybe-nav-flash)

  ;; Seems I could just add advice to set-buffer to remove some of these, but it
  ;; didn't work for me.
  (advice-add #'next-buffer :after #'my/maybe-nav-flash)
  (advice-add #'previous-buffer :after #'my/maybe-nav-flash)
  (advice-add #'switch-to-buffer :after #'my/maybe-nav-flash)
  (advice-add #'other-window :after #'my/maybe-nav-flash)
  (advice-add #'recenter :after #'my/maybe-nav-flash)

  ;; `avy'
  (advice-add #'evil-avy-goto-char-timer :after #'my/maybe-nav-flash)

  ;; `evil'
  (advice-add #'evil-scroll-up :after #'my/maybe-nav-flash)
  (advice-add #'evil-scroll-down :after #'my/maybe-nav-flash)

  ;; This covers gg and G along with jumping to specific line numbers.
  (advice-add #'evil-goto-line :after #'my/maybe-nav-flash)

  (advice-add #'evil-window-top :after #'my/maybe-nav-flash)
  (advice-add #'evil-window-middle :after #'my/maybe-nav-flash)
  (advice-add #'evil-window-bottom :after #'my/maybe-nav-flash)

  ;; At first seems we could just hook into `window-configuration-change-hook'
  ;; instead of all these, but it causes it to flash on a different line than
  ;; the cursor for some unknown reason.
  (advice-add #'evil-window-move-far-left :after #'my/maybe-nav-flash)
  (advice-add #'evil-window-move-far-right :after #'my/maybe-nav-flash)
  (advice-add #'evil-window-move-very-top :after #'my/maybe-nav-flash)
  (advice-add #'evil-window-move-very-bottom :after #'my/maybe-nav-flash)

  ;; `evil-window-right' etc. uses `windmove'
  (advice-add #'windmove-do-window-select :after #'my/maybe-nav-flash))

(provide 'init-ui)
