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


(use-package gptel
  :defer 3
  :config
  (require 'gptel-curl)
  ;; (require 'gptel-anthropic)

  ;; https://github.com/karthink/gptel/issues/302
  ;; hard wrapping doesn't work well because it applies to source blocks
  (add-hook 'gptel-mode-hook #'visual-line-mode)
  (add-hook 'gptel-post-response-functions 'whitespace-cleanup)

  (setq gptel-default-mode 'org-mode)

  (setq gptel-model "anthropic/claude-3.5-sonnet"
        gptel-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key (lambda () (nth 3 (process-lines "gopass" "show" "will/websites/general/openrouter.ai")))
          :models '("anthropic/claude-3.5-sonnet"
                    "anthropic/claude-3-haiku"
                    "openai/gpt-4o-mini-2024-07-18"
                    "openai/gpt-4o"
                    "mistralai/mixtral-8x22b-instruct"))))


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
