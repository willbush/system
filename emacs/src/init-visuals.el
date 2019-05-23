;;; -*- lexical-binding: t; -*-

;; In Linux I set these using .Xresources file because it saves about half a
;; second on startup time compared to setting these here.
(when (eq system-type 'windows-nt)
  ;; disable GUI elements.
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; set default font
  (set-face-attribute 'default nil
                      :family "Hack"
                      :height 110
                      :weight 'normal
                      :width 'normal))

;; show line and column number on mode line
(line-number-mode 1)
(column-number-mode 1)

(setq electric-pair-pairs
  '(
    (?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\})
   ))
(electric-pair-mode t)

(use-package doom-themes
  :config
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (load-theme 'doom-tomorrow-night t))

;; doom modeline requires M-x all-the-icons-install-fonts
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-root))

;; Highlight matching parentheses
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
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
          focus-in ;; A hook that fires when an Emacs frame gains focus.
          counsel-grep-post-action
          imenu-after-jump
          evil-jumps-post-jump
          xref-after-jump
          org-follow-link
          xref-after-return) . my/maybe-nav-flash)

  :init

  ;; Seems I could just add advice to set-buffer to remove some of these, but it
  ;; didn't work for me.
  (advice-add #'ivy-switch-buffer :after #'my/maybe-nav-flash)
  (advice-add #'next-buffer :after #'my/maybe-nav-flash)
  (advice-add #'previous-buffer :after #'my/maybe-nav-flash)
  (advice-add #'switch-to-buffer :after #'my/maybe-nav-flash)
  (advice-add #'other-window :after #'my/maybe-nav-flash)
  (advice-add #'recenter :after #'my/maybe-nav-flash)
  ;; `winum--switch-to-window' is called by the `winum-select-window-*' functions
  (advice-add #'winum--switch-to-window :after #'my/maybe-nav-flash)

  ;; `avy'
  (advice-add #'evil-avy-goto-char-timer :after #'my/maybe-nav-flash)

  ;; `saveplace'
  (advice-add #'save-place-find-file-hook :after #'my/maybe-nav-flash)

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

(provide 'init-visuals)
