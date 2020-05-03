;;; -*- lexical-binding: t; -*-

(use-package swiper
  :commands
  (swiper
   swiper-backward
   swiper-multi)
  :config
  (setq swiper-goto-start-of-match t))

(use-package deadgrep
  :commands deadgrep
  :config
  (general-def
    :states 'normal
    :keymaps 'deadgrep-mode-map
    "RET" 'deadgrep-visit-result
    "S-RET" 'deadgrep-visit-result-other-window
    "go" 'deadgrep-visit-result-other-window
    "gr" 'deadgrep-restart
    "C-n" 'deadgrep-forward
    "C-e" 'deadgrep-backward
    "TAB" 'deadgrep-toggle-file-results
    "l" 'deadgrep-edit-mode ;; similar to how dired works
    "q" 'quit-window)

  (general-def
    :states 'normal
    :keymaps 'deadgrep-edit-mode-map
    "RET" 'deadgrep-visit-result
    "<escape>" 'deadgrep-mode))

(use-package esup :commands esup)

;; Adopt Doom's sneaky garbage collection strategy of waiting until idle time to
;; collect; staving off the collector while the user is working.
(use-package gcmh
  :hook (after-init . gcmh-mode)
  :commands gcmh-idle-garbage-collect
  :config
  (setq gcmh-idle-delay 10
        gcmh-high-cons-threshold 16777216)
  (add-function :after after-focus-change-function #'gcmh-idle-garbage-collect))

(use-package flyspell
  :ensure nil ;; Flyspell is included in Emacs.
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :init (setq ispell-program-name "aspell")
  :config
  ;; improve perf per wiki: https://www.emacswiki.org/emacs/FlySpell
  (setq flyspell-issue-message-flag nil))

(use-package define-word
  :commands define-word-at-point)

(use-package flyspell-correct-ivy
  :commands
  (flyspell-correct-wrapper
   flyspell-correct-next
   flyspell-correct-previous)
  :init
  (declare-function flyspell-correct-ivy "flyspell-correct-ivy")
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

;; A frontend for weather web service wttr.in
(use-package wttrin
  :commands wttrin
  :config
  (setq wttrin-default-cities '("Dallas")
        wttrin-default-accept-language '("Accept-Language" . "en-US")))

(use-package ranger
  :commands
  (ranger
   deer
   deer-jump-other-window)
  :config
  ;; swap out Qwerty to Colemak-DHm
  (general-swap-key nil '(ranger-mode-map ranger-normal-mode-map)
    "C-j" "C-n"
    "C-k" "C-e"
    "H" "M"
    "J" "N"
    "K" "E"
    "L" "I"
    "h" "m"
    "j" "n"
    "k" "e"
    "l" "i"))

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

;; lorri makes this as a global minor-mode tolerable.
(use-package direnv
  :hook (after-init . direnv-mode))

(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode)))

(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "http://planet.emacsen.org/atom.xml")))

(provide 'init-misc-tools)
