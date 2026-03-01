;;; -*- lexical-binding: t; -*-

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
;; intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;; `scroll-bar-mode' because their manipulation of frame parameters can
;; trigger/queue a superfluous (and expensive, depending on the window system)
;; frame redraw at startup.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Less visual noise
(setq inhibit-startup-message t
      inhibit-default-init t
      ;; Avoid pulling in many packages by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode'.
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      ring-bell-function 'ignore)

;; Do not use `custom-set-faces` because it's slow.
(set-face-attribute 'default nil
                    :family "IBM Plex Mono"
                    :weight 'regular
                    ;; height = point size * 10
                    :height 130)
(set-face-attribute 'fixed-pitch nil :family "IBM Plex Mono")
(set-face-attribute 'fixed-pitch-serif nil :family "IBM Plex Mono")
(set-face-attribute 'variable-pitch nil :family "DejaVu Serif")

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. I handle package initialization, so we
;; must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
