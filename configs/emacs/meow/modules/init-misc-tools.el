;;; -*- lexical-binding: t; -*-

(use-package swiper
  :commands
  (swiper
   swiper-backward
   swiper-multi)
  :config
  (setq swiper-goto-start-of-match t))


(use-package zoxide
  :commands zoxide-travel
  :hook
  ((find-file
    dired-after-readin) . zoxide-add))


(use-package deadgrep
  :commands deadgrep
  :config
  ;; (general-def
  ;;   :states 'normal
  ;;   :keymaps 'deadgrep-mode-map
  ;;   "RET" 'deadgrep-visit-result
  ;;   "S-RET" 'deadgrep-visit-result-other-window

  ;;   "gs" 'deadgrep-search-term

  ;;   "go" 'deadgrep-visit-result-other-window
  ;;   "gr" 'deadgrep-restart

  ;;   "gc" 'deadgrep-cycle-search-case
  ;;   "gt" 'deadgrep-cycle-search-type

  ;;   "C-n" 'deadgrep-forward
  ;;   "C-e" 'deadgrep-backward
  ;;   "TAB" 'deadgrep-toggle-file-results
  ;;   "l" 'deadgrep-edit-mode ;; similar to how dired works
  ;;   ;; not using q because often define macros here
  ;;   "Q" 'quit-window)

  ;; (general-def
  ;;   :states 'normal
  ;;   :keymaps 'deadgrep-edit-mode-map
  ;;   "RET" 'deadgrep-visit-result
  ;;   "<escape>" 'deadgrep-mode)
  )


;; Adopt Doom's sneaky garbage collection strategy of waiting until idle time to
;; collect; staving off the collector while the user is working.
(use-package gcmh
  :hook (after-startup . gcmh-mode)
  :commands gcmh-idle-garbage-collect
  :config
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold GC-CONS-THRESHOLD)
  (add-function :after after-focus-change-function #'gcmh-idle-garbage-collect))


(use-package direnv
    :hook (after-startup . direnv-mode))


(use-package password-store
  :commands
  (password-store-url
   password-store-copy
   password-store-copy-field)
  :config
  (setq password-store-executable "gopass"))


(use-package password-generator
  :commands
  (password-generator-phonetic
   password-generator-strong
   password-generator-paranoid))


(use-package exec-path-from-shell
  :commands exec-path-from-shell-copy-env)


(use-package keycast
  :commands
  (keycast-header-line-mode
   keycast-log-mode
   keycast-tab-bar-mode))

(use-package atomic-chrome
  :commands (atomic-chrome-start-server))


;; (use-package zoom
;;   :commands zoom zoom-mode
;;   :config
;;   (custom-set-variables
;;    ;; golden ratio
;;   '(zoom-size '(0.618 . 0.618))))


;; (use-package adaptive-wrap
;;   :commands adaptive-wrap-prefix-mode)


;; (use-package visual-fill-column
;;   :commands visual-fill-column-mode)

;; (use-package sudo-edit :commands sudo-edit)

;; (use-package flyspell
;;   :commands (flyspell-mode flyspell-prog-mode)
;;   :init (setq ispell-program-name "aspell")
;;   :config
;;   ;; improve perf per wiki: https://www.emacswiki.org/emacs/FlySpell
;;   (setq flyspell-issue-message-flag nil))


;; (use-package define-word
;;   :commands define-word-at-point)


(provide 'init-misc-tools)
