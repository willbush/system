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
  (flyspell-correct-at-point
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

(use-package disk-usage
  :commands (disk-usage disk-usage-by-types)
  :config
  (evil-collection-inhibit-insert-state 'disk-usage-mode-map)
  (evil-collection-inhibit-insert-state 'disk-usage-by-types-mode-map)

  (general-def
    :states 'normal
    :keymaps 'disk-usage-mode-map
    "A" 'disk-usage-remove-filters
    "C-e" 'disk-usage-up
    "S" 'tabulated-list-sort
    "a" 'disk-usage-add-filters ;; hit tab to see the completion list.
    "d" 'disk-usage-dired-at-point
    "gr" 'revert-buffer
    "k" 'disk-usage-mark
    "q" 'quit-window
    "u" 'disk-usage-unmark
    "x" 'disk-usage-delete-marked-files)

  (general-def
    :states 'normal
    :keymaps 'disk-usage-by-types-mode-map
    "A" 'disk-usage-remove-filters
    "RET" 'disk-usage-files
    "S" 'tabulated-list-sort
    "a" 'disk-usage-add-filters ;; hit tab to see the completion list.
    "gr" 'revert-buffer
    "q" 'quit-window
    "zh" 'disk-usage-toggle-human-readable)

  (general-def
    :prefix ","
    :states 'normal
    :keymaps '(disk-usage-mode-map disk-usage-by-types-mode-map)
    "R" 'disk-usage-reset-cache
    "t" '(:ignore t :which-key "toggle")
    "tf" 'disk-usage-toggle-full-path
    "th" 'disk-usage-toggle-human-readable
    "tr" 'disk-usage-toggle-recursive))

(unless IS-WINDOWS
  ;; lorri makes this as a global minor-mode tolerable.
  (use-package direnv
    :hook (after-init . direnv-mode)))

(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode)))

(use-package woman
  :ensure nil ;; included in Emacs.
  :after evil
  :config
  (evil-set-initial-state 'woman-mode 'normal)
  (general-def
    :states 'normal
    :keymaps 'woman-mode-map
    "]]" 'WoMan-next-manpage
    "[[" 'WoMan-previous-manpage
    "gr" 'woman-reformat-last-file))

(use-package cus-edit
  :ensure nil ;; included in Emacs.
  :after evil
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
  :ensure nil ;; included in Emacs.
  :after evil
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
  ;; Note counsel settings take care of "C-h f" and "C-h v"
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

(use-package password-store
  :commands
  ;; use ive-pass for everything else
  (password-store-url
   password-store-copy-field)
  :config
  (setq password-store-executable "gopass"))

(use-package ivy-pass :commands ivy-pass)

(use-package password-generator
  :commands
  (password-generator-phonetic
   password-generator-strong
   password-generator-paranoid))

(provide 'init-misc-tools)
