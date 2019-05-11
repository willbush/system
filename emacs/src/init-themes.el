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

;; enable prettify symbols when using GUI emacs
(when window-system (global-prettify-symbols-mode t))

(use-package doom-themes
  :config
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (load-theme 'doom-Iosvkem t))

;; doom modeline requires M-x all-the-icons-install-fonts
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-root))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package fill-column-indicator
  :commands fci-mode)

(provide 'init-visuals)
