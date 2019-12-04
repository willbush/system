;;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(if (eq system-type 'windows-nt)
    (progn

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

  ;; When on Linux I use use home-manager to install packages.
  (eval-when-compile
    (require 'package)
    (setq package-archives nil)
    (package-initialize)))

(require 'use-package)

;; Setting this variable reduces warnings with compiling my config.
;; see https://github.com/jwiegley/use-package/issues/590
(eval-when-compile
  (setq use-package-expand-minimally t))

(provide 'init-package)
