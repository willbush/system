;;; -*- lexical-binding: t; -*-

(use-package which-key
  :defer 1
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (which-key-mode +1))

(use-package general)

;; Global Bindings
(general-def
  "M-x" 'counsel-M-x
  ;; zoom in and out
  "C-+" 'text-scale-increase
  "C--" 'text-scale-decrease
  ;; increment and decrement number at point.
  "M-+" 'evil-numbers/inc-at-pt
  "M--" 'evil-numbers/dec-at-pt)

;; pressing r again after going into range mode (visual mode) will enter a hydra
;; for expand-region usage
(general-def
  :keymaps 'evil-visual-state-map
  "r" 'hydra-expand-region/body)

;; Leader key bindings
(general-def
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :states '(normal visual emacs)
  :keymaps 'override
  "TAB" 'mode-line-other-buffer
  "!" 'shell-command
  "'" 'my/open-shell
  "?" 'counsel-descbinds
  "F" '(:ignore t :wk "frame")
  "FD" 'delete-other-frames
  "Fd" 'delete-frame
  "Fi" 'iconify-frame
  "Fm" 'toggle-frame-maximized
  "Fn" 'make-frame
  "Fo" 'other-frame
  "S" '(:ignore t :wk "spell-checking")
  "SPC" 'counsel-M-x
  "Sb" 'flyspell-buffer
  "Sc" 'flyspell-correct-wrapper
  "Sn" 'flyspell-correct-next
  "Sp" 'flyspell-correct-previous
  "Sr" 'flyspell-region
  "a" '(:ignore t :wk "apps")
  "aD" 'deer-jump-other-window
  "aE" 'esup
  "ad" 'deer ;; minimal ranger dired
  "ae" '(:ignore t :wk "direnv")
  "aeU" 'direnv-update-directory-environment
  "aea" 'direnv-allow
  "aem" 'direnv-mode
  "aeu" 'direnv-update-environment
  "af" 'elfeed
  "ap" '(:ignore t :wk "profiler")
  "apk" 'profiler-stop
  "apr" 'profiler-report
  "aps" 'profiler-start
  "apw" 'profiler-report-write-profile
  "ar" 'ranger
  "as" 'speedbar
  "au" 'disk-usage
  "aw" 'wttrin
  "b" '(:ignore t :wk "buffer")
  "bD" 'my/kill-all-buffers
  "bH" 'my/kill-all-buffers-then-switch-to-dashboard
  "bI" 'ibuffer
  "bb" 'ivy-switch-buffer
  "bd" 'my/kill-this-buffer
  "bh" 'my/switch-to-dashboard
  "bi" 'counsel-ibuffer
  "bk" 'kill-buffer ;; requests buffer to kill
  "bm" 'my/switch-to-messages
  "br" 'my/revert-buffer-no-confirm
  "bs" 'my/switch-to-scratch
  "c" '(:ignore t :wk "comment")
  "cl" 'comment-line
  "cr" 'comment-or-uncomment-region
  "f" '(:ignore t :wk "file")
  "fd" 'fd-dired
  "ff" 'counsel-find-file
  "fs" 'save-buffer
  "fy" 'my/yank-and-show-buffer-full-path
  "fz" 'counsel-fzf
  "g" '(:ignore t :wk "go")
  "gf" 'find-function
  "gg" 'avy-goto-char-timer
  "gv" 'find-variable
  "h" '(:ignore t :wk "help")
  "hI" 'info-display-manual
  "hd" '(:ignore t :wk "describe")
  ;; "hdB" 'evil-collection-describe-bindings
  "hdK" 'my/describe-keymap
  "hdb" 'counsel-descbinds
  "hdc" 'describe-char
  "hdf" 'counsel-describe-function
  "hdg" 'general-describe-keybindings
  "hdk" 'describe-key
  "hdm" 'describe-mode
  "hdp" 'describe-package
  "hds" 'counsel-info-lookup-symbol
  "hdt" 'describe-theme
  "hdu" 'counsel-unicode-char
  "hdv" 'counsel-describe-variable
  "hi" 'info
  "hl" 'counsel-find-library
  "hn"  'view-emacs-news
  "ht" 'evil-tutor-start
  "hw"  'woman
  "m" '(:ignore t :wk "magit")
  "mG" 'hydra-git-gutter/body
  "mI" 'magit-init
  "mc" 'magit-clone
  "md" 'magit-dispatch
  "mg" 'counsel-git-grep
  "mi" 'magit-gitignore-globally
  "ml" 'counsel-git-log
  "mm" 'magit-status
  "mt" 'git-timemachine
  "n" '(:ignore t :wk "narrow")
  "nf" 'narrow-to-defun
  "np" 'narrow-to-page
  "nr" 'narrow-to-region
  "nw" 'widen
  "o" '(:ignore t :wk "org")
  "oS" 'org-store-link
  "oa" 'org-agenda
  "oc" 'org-capture
  "oi" 'org-indent-mode
  "or" 'org-refile
  "os" 'org-save-all-org-buffers
  "ot" 'org-toggle-link-display
  "p" 'projectile-command-map
  "q" '(:ignore t :wk "quit")
  "qQ" 'save-buffers-kill-emacs
  "qq" 'save-buffers-kill-terminal
  "r" '(:ignore t :wk "rapid") ;; emphasize easy access over mnemonics
  "rl" 'counsel-load-theme
  "rs" 'save-buffer
  "s" '(:ignore t :wk "search")
  "sD" '(counsel-rg :wk "counsel-rg-directory")
  "sc" 'evil-ex-nohighlight ;; mnemonic is search clear
  "sd" 'deadgrep
  "so" 'counsel-outline
  "sr" 'counsel-rg
  "ss" 'swiper-multi
  "sz" 'counsel-fzf
  "t" '(:ignore t :wk "toggle")
  "tc" 'fci-mode
  "tf" 'auto-fill-mode
  "tg" 'my/toggle-golden-ratio
  "tl" 'toggle-truncate-lines
  "tm" 'counsel-major ;; switches major mode
  "tn" 'display-line-numbers-mode
  "ts" 'flyspell-mode
  "tt" 'display-time-mode
  "tv" 'my/toggle-adaptive-visual-fill-column
  "w" '(:ignore t :wk "window")
  "w-" 'split-window-vertically
  "w/" 'split-window-horizontally
  "wB" 'balance-windows-area
  "wE" 'evil-window-move-very-top
  "wI" 'evil-window-move-far-right
  "wM" 'evil-window-move-far-left
  "wN" 'evil-window-move-very-bottom
  "wb" 'balance-windows
  "wd" 'delete-window
  "we" 'evil-window-up
  "wg" 'golden-ratio
  "wi" 'evil-window-right
  "wk" 'kill-buffer-and-window
  "wm" 'evil-window-left
  "wn" 'evil-window-down
  "wo" 'delete-other-windows
  "wx" 'my/toggle-maximize-window
  "x" '(:ignore t :wk "text manipulation")
  "xC" 'my/sort-lines-by-column-reverse
  "xL" 'my/sort-lines-reverse
  "xc" 'my/sort-lines-by-column
  "xd" 'define-word-at-point
  "xl" 'my/sort-lines
  "xu" 'my/uniquify-lines)

;; evil-collection will give a warning if the following setting is not set
;; before loading evil and evil-collection. Note that evil-leader loads evil
;; see: https://github.com/emacs-evil/evil-collection/issues/215 Also even if
;; this is in the :init block it will still given the warning when lazy loading
;; evil.
(setq evil-want-keybinding nil)

;; (defun my/custom-evil-collection-bindings (mode mode-keymaps &rest _rest)
;;   (cond ((eq mode 'dired)
;;          ;; dired key bindings
;;          (general-def
;;            :states 'normal
;;            :keymaps 'dired-mode-map
;;            ;; remove evil mode shadows
;;            "i" nil ;; was 'dired-toggle-read-only
;;            "m" nil ;; was 'dired-mark
;;            "j" nil ;; was 'dired-next-line
;;            "^" nil ;; was 'dired-up-directory
;;            "r" nil ;; was 'dired-do-redisplay
;;            "R" nil ;; was 'dired-do-rename
;;            ;; rebind things better to my custom evil keys
;;            "l" 'dired-toggle-read-only
;;            "k" 'dired-mark
;;            "n" 'dired-next-line
;;            "e" 'dired-previous-line
;;            "C-e" 'dired-up-directory
;;            "v" 'dired-do-rename))
;;         ;; default case make some blind key swaps for my custom evil keys.
;;         ;;
;;         ;; Note this does not work for `ediff-mode' because evil-collection
;;         ;; doesn't apply key bindings until after `ediff-startup-hook'.
;;         (t (evil-collection-swap-key 'normal mode-keymaps
;;              "m" "h" ;; left
;;              "n" "j" ;; down
;;              "e" "k" ;; up
;;              "i" "l" ;; right
;;              "r" "v" ;; range (old name visual)
;;              (kbd "C-n") (kbd "C-j")
;;              (kbd "C-e") (kbd "C-k")))))

;; ;; A collection of evil key bindings for various modes
;; (use-package evil-collection
;;   :commands evil-collection-init
;;   :custom (evil-collection-setup-minibuffer t)
;;   :init
;;   (setq evil-collection-mode-list
;;         '(calendar
;;           (package-menu package)
;;           (pdf pdf-view)
;;           (term term ansi-term multi-term)
;;           compile
;;           cus-theme
;;           custom
;;           deadgrep
;;           debug
;;           dired
;;           disk-usage
;;           elfeed
;;           help
;;           info
;;           ivy
;;           man
;;           minibuffer
;;           woman))

;;   ;; called after evil-collection makes its keybindings
;;   ;; https://github.com/emacs-evil/evil-collection#key-translation
;;   (add-hook 'evil-collection-setup-hook #'my/custom-evil-collection-bindings))

(provide 'init-keys)
