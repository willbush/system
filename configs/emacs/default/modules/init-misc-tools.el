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
  (general-def
    :states 'normal
    :keymaps 'deadgrep-mode-map
    "RET" 'deadgrep-visit-result
    "S-RET" 'deadgrep-visit-result-other-window

    "gs" 'deadgrep-search-term

    "go" 'deadgrep-visit-result-other-window
    "gr" 'deadgrep-restart

    "gc" 'deadgrep-cycle-search-case
    "gt" 'deadgrep-cycle-search-type

    "C-n" 'deadgrep-forward
    "C-e" 'deadgrep-backward
    "TAB" 'deadgrep-toggle-file-results
    "l" 'deadgrep-edit-mode ;; similar to how dired works
    ;; not using q because often define macros here
    "Q" 'quit-window)

  (general-def
    :states 'normal
    :keymaps 'deadgrep-edit-mode-map
    "RET" 'deadgrep-visit-result
    "<escape>" 'deadgrep-mode))


;; Adopt Doom's sneaky garbage collection strategy of waiting until idle time to
;; collect; staving off the collector while the user is working.
(use-package gcmh
  :hook (after-init . gcmh-mode)
  :commands gcmh-idle-garbage-collect
  :config
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold GC-CONS-THRESHOLD)
  (add-function :after after-focus-change-function #'gcmh-idle-garbage-collect))


(use-package jinx
  :commands (jinx-correct jinx-mode))

(use-package define-word
  :commands define-word-at-point)


(use-package direnv
    :hook (after-init . direnv-mode))


(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode)))


(use-package woman
  :defer t
  :config
  (evil-set-initial-state 'woman-mode 'normal)
  (general-def
    :states 'normal
    :keymaps 'woman-mode-map
    "]]" 'WoMan-next-manpage
    "[[" 'WoMan-previous-manpage
    "gr" 'woman-reformat-last-file))


(use-package cus-edit
  :config
  (evil-set-initial-state 'Custom-mode 'normal)
  (general-def
    :states 'normal
    :keymaps 'custom-mode-map
    "RET" 'Custom-newline
    "<tab>" 'widget-forward
    "<backtab>" 'widget-backward
    "S-<tab>" 'widget-backward
    "C-e" 'Custom-goto-parent
    "C-o" 'Custom-goto-parent
    "q" 'Custom-buffer-done))


;; Despite using helpful, I might find myself in help-mode so I'm going to keep
;; these settings.
(use-package help-mode
  :config
  (evil-set-initial-state 'help-mode 'normal)
  (evil-collection-inhibit-insert-state 'help-mode-map)

  (general-def
    :states 'normal
    :keymaps 'help-mode-map
    "<backtab>" 'backward-button
    "<tab>" 'forward-button
    "C-i" 'help-go-forward
    "C-o" 'help-go-back
    "RET" 'help-follow
    "gr" 'revert-buffer
    "q" 'quit-window))


;; A better *help* buffer.
(use-package helpful
  :commands
  (helpful-callable
   helpful-command
   helpful-variable
   helpful-key
   helpful-symbol
   helpful-at-point)

  :init
  (global-set-key (kbd "C-h k") #'helpful-key)

  :config
  (evil-collection-inhibit-insert-state 'helpful-mode-map)
  (general-def
    :states 'normal
    :keymaps 'helpful-mode-map
    "<backtab>" 'backward-button
    "<tab>" 'forward-button
    "gr" 'helpful-update
    "q" 'quit-window))


(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000
        vterm-buffer-name-string "vterm %s"))


(use-package exec-path-from-shell
  :commands exec-path-from-shell-copy-env)


(use-package gptel
  :commands (gptel gptel-send gptel-abort gptel-end-of-response)
  :config
  (require 'gptel-curl)
  (require 'gptel-gemini)

  ;; https://github.com/karthink/gptel/issues/302
  ;; hard wrapping doesn't work well because it applies to source blocks
  (add-hook 'gptel-mode-hook #'visual-line-mode)
  (add-hook 'gptel-post-response-functions (lambda (&rest _) (whitespace-cleanup)))

  (setq gptel-default-mode 'markdown-mode)

  (setq
   gptel-model 'gemini-2.5-pro-preview-05-06
   gptel-backend
   (gptel-make-gemini "Gemini"
                      :key (lambda () (nth 0 (process-lines "rbw" "get" "gemini" "--field" "api key")))
                      :stream t)))


(use-package keycast
  :commands
  (keycast-header-line-mode
   keycast-log-mode
   keycast-tab-bar-mode))


(use-package zoom
  :commands zoom zoom-mode
  :config
  (custom-set-variables
   ;; golden ratio
  '(zoom-size '(0.618 . 0.618))))


(use-package adaptive-wrap
  :commands adaptive-wrap-prefix-mode)


(use-package visual-fill-column
  :commands visual-fill-column-mode)

(use-package sudo-edit :commands sudo-edit)


(provide 'init-misc-tools)
