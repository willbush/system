;;; -*- lexical-binding: t; -*-

(if (eq system-type 'windows-nt)
    (progn
      (require 'package)

      (add-to-list 'package-archives
                   '("melpa" . "http://melpa.org/packages/"))
      (add-to-list 'package-archives
                   '("melpa-stable" . "https://stable.melpa.org/packages/"))
      (add-to-list 'package-archives
                   '("gnu" . "https://elpa.gnu.org/packages/"))

      (setq packages-enable-at-startup nil)
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
    (setq packages-enable-at-startup nil)
    (setq package-archives nil)
    (package-initialize)))

(require 'use-package)

(provide 'init-package)
