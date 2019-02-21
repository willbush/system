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

(use-package flyspell-correct-ivy :after flyspell)

;; A frontend for weather web service wttr.in
(use-package wttrin
  :commands wttrin
  :config
  (setq wttrin-default-cities '("Dallas")
        wttrin-default-accept-language '("Accept-Language" . "en-US")))

(use-package ranger :commands ranger)

(use-package disk-usage :commands disk-usage)

(provide 'init-misc-tools)
