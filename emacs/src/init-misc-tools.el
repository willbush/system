;;; -*- lexical-binding: t; -*-

(use-package swiper :bind (("C-s" . swiper)))

(use-package deadgrep :commands deadgrep)

(use-package esup :commands esup)

(use-package flyspell
  :ensure nil ;; no reason to try to ensure because it's built in
  :init (setq ispell-program-name "aspell")
  :config
  ;; improve perf per wiki: https://www.emacswiki.org/emacs/FlySpell
  (setq flyspell-issue-message-flag nil)
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package define-word
  :commands define-word-at-point)

(use-package flyspell-correct-ivy :after flyspell)

;; A frontend for weather web service wttr.in
(use-package wttrin
  :commands wttrin
  :config
  (setq wttrin-default-cities '("Dallas")
        wttrin-default-accept-language '("Accept-Language" . "en-US")))

(use-package ranger :commands ranger)

(use-package disk-usage :commands disk-usage)

(use-package dired-narrow
  :commands
  (dired-narrow dired-narrow-regexp dired-narrow-fuzzy)

  :init
  (general-def
    :prefix ","
    :states '(normal visual)
    :keymaps 'dired-mode-map
    "n" '(:ignore t :which-key "narrow"))

  (general-def
    :prefix ", n"
    :states '(normal visual)
    :keymaps 'dired-mode-map
    "n" 'dired-narrow
    "f" 'dired-narrow-fuzzy
    "r" 'dired-narrow-regexp))

(use-package direnv
  ;; I rather enable this mode manually when I want it
  :commands direnv-mode
  :config

  (general-def
    :prefix ","
    :keymaps 'dired-mode-map ;; only want these keys available in dired mode
    :states '(normal visual)
    "e" 'direnv-edit
    "u" 'direnv-update-environment
    "U" 'direnv-update-directory-environment))

(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode)))

(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "http://planet.emacsen.org/atom.xml")))

(provide 'init-misc-tools)
