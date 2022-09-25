;;; -*- lexical-binding: t; -*-
;; Also see `early-init-file' because it sets UI elements early.

;; Less visual noise
(setq inhibit-startup-message t
      inhibit-default-init t
      ;; Avoid pulling in many packages by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode'.
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      ring-bell-function 'ignore)

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
          counsel-grep-post-action
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
  (advice-add #'ivy-switch-buffer :after #'my/maybe-nav-flash)
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

  ;; edwina window movement
  (advice-add #'edwina-select-next-window :after #'my/maybe-nav-flash)
  (advice-add #'edwina-select-previous-window :after #'my/maybe-nav-flash)

  ;; `evil-window-right' etc. uses `windmove'
  (advice-add #'windmove-do-window-select :after #'my/maybe-nav-flash))

(provide 'init-ui)
