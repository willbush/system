;;; -*- lexical-binding: t; -*-

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Work around to a crippling performance issue I reported affecting Emacs 28
;; after cario was made the default: https://debbugs.gnu.org/db/40/40733.html I
;; don't have this font on my system anymore, but I keep this around incase it
;; finds its way back somehow.
(add-to-list 'face-ignored-fonts "Adobe Blank")

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
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

;; Set the default face. The default face is the basis for most other faces used
;; in Emacs. A "face" is a configuration including font, font size, foreground
;; and background colors and other attributes. The fixed-pitch and
;; fixed-pitch-serif faces are monospace faces generally used as the default
;; face for code. The variable-pitch face is used when `variable-pitch-mode' is
;; turned on, generally whenever a non-monospace face is preferred.
(custom-set-faces
 `(default ((t (:font "Fira Mono 10"))))
 `(fixed-pitch ((t (:inherit (default)))))
 `(fixed-pitch-serif ((t (:inherit (default)))))
 `(variable-pitch ((t (:font "DejaVu Sans 10")))))

;; Loads a nice blue theme, avoids the white screen flash on startup.
(load-theme 'deeper-blue t)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. I handle package initialization, so we
;; must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
