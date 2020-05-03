;;; -*- lexical-binding: t; -*-

;;
;;; Global Constants

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-INTERACTIVE (not noninteractive))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Set default font
(set-face-attribute 'default nil
                    :family "Hack"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

(add-to-list 'load-path (expand-file-name "src/" user-emacs-directory))

(require 'init-package)

;; Set `doom-themes' early to prevent non-stylized UI flash.
(use-package doom-themes
  :config
  ;; Apply `doom-theme'
  (load-theme 'doom-outrun-electric t))

;; Set `doom-modeline' early to prevent non-stylized UI flash.
;; Note: `doom-modeline' requires M-x all-the-icons-install-fonts.
(use-package doom-modeline
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-root)
  (doom-modeline-mode 1)
  (size-indication-mode 1)
  (column-number-mode 1))

(use-package dashboard
  :init
  (setq initial-buffer-choice
       (lambda ()
         (or (get-buffer "*dashboard*") (get-buffer "*scratch*"))))
  :config
  (setq dashboard-banner-logo-title nil
       dashboard-set-heading-icons t
       dashboard-set-file-icons t
       dashboard-items '((recents  . 5)
                          (projects . 5)))
  ;; This is the default icon, but it doesn't always show up when running Emacs
  ;; as a daemon. So I set it explicitly here to fix the issue.
  (setq dashboard-footer-icon
    #("" 0 1
      (rear-nonsticky t display
                      (raise -0.06)
                      font-lock-face #1=(:family "file-icons" :height 1.32 :inherit font-lock-keyword-face)
                      face #1#)))
 (dashboard-setup-startup-hook))
