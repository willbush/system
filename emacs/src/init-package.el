;;; -*- lexical-binding: t; -*-

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package initialization, so we
;; must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; When on Windows I use `package.el' to install packages. Otherwise, I I'm on
;; Linux and use an external tool called home-manager.
(when IS-WINDOWS
  (require 'package)

  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/"))
  (add-to-list 'package-archives
               '("gnu" . "https://elpa.gnu.org/packages/"))

  (package-initialize)

  ;; bootstrap use-package with package.el
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  ;; ensure everything is installed
  (setq use-package-always-ensure t))

;; Despite the `use-package' README suggesting that this be wrapped in an
;; `eval-when-compile', that causes issues for me when I byte compile my config.
(require 'use-package)

(provide 'init-package)
