;;; -*- lexical-binding: t; -*-

;; Global Bindings
(general-def
  "M-x" 'counsel-M-x
  ;; zoom in and out
  "C-+" 'text-scale-increase
  "C--" 'text-scale-decrease
  "C-s" 'swiper)

;; pressing r again after going into range mode (visual mode) will enter a hydra
;; for expand-region usage
(general-def
  :keymaps 'evil-visual-state-map
  "r" 'hydra-expand-region/body)

;;
;;; Leader key bindings

;; This should come after evil due to performance issues see:
;; https://github.com/noctuid/general.el/issues/180. Note that simply wrapping
;; this in a `evil-after-load-hook' doesn't work.
(general-def
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :states '(normal visual emacs)
  :keymaps 'override
  "TAB" 'mode-line-other-buffer
  "!" 'shell-command
  "'" 'vterm
  "?" 'counsel-descbinds
  "SPC" 'counsel-M-x

  "F" '(:ignore t :wk "frame")
  "FD" 'delete-other-frames
  "Fd" 'delete-frame
  "Fi" 'iconify-frame
  "Fm" 'toggle-frame-maximized
  "Fn" 'make-frame
  "Fo" 'other-frame

  "S" '(:ignore t :wk "spell-checking")
  "Sb" 'flyspell-buffer
  "Sc" 'flyspell-correct-at-point
  "Sn" 'flyspell-correct-next
  "Sp" 'flyspell-correct-previous
  "Sr" 'flyspell-region

  "a" '(:ignore t :wk "apps")

  "ac" '(:ignore t :wk "chapgpt")
  "acC" '(chatgpt-shell-ctrl-c-ctrl-c :wk "ctrl-c-ctrl-c")
  "acR" '(chatgpt-shell-restore-session-from-transcript :wk "restore-session-from-transcript")
  "acS" '(chatgpt-shell-send-region :wk "send-region")
  "acT" '(chatgpt-shell-save-session-transcript :wk "save-session-transcript")
  "acc" 'chatgpt-shell
  "acd" '(chatgpt-shell-describe-code :wk "describe-code")
  "ace" '(chatgpt-shell-explain-code :wk "explain-code")
  "acg" '(chatgpt-shell-generate-unit-test :wk "generate-unit-test")
  "acl" '(chatgpt-shell-clear-buffer :wk "clear-buffer")
  "acm" '(chatgpt-shell-mark-at-point-dwim :wk "mark-at-point-dwim")
  "acp" '(chatgpt-shell-proofreading-doc :wk "proofreading-doc")
  "acr" '(chatgpt-shell-refactory-code :wk "refactory-code")
  "acs" '(chatgpt-shell-send-and-review-region :wk "send-and-review-region")
  "act" '(my/toggle-chatgpt-shell-model-version :wk "toggle model version")
  "acv" '(chatgpt-shell-view-at-point :wk "view-at-point")

  "aD" 'deer-jump-other-window
  "ad" 'deer ;; minimal ranger dired

  "ae" '(:ignore t :wk "direnv")
  "aeU" 'direnv-update-directory-environment
  "aea" 'direnv-allow
  "aem" 'direnv-mode
  "aeu" 'direnv-update-environment

  "ap" '(:ignore t :wk "profiler")
  "apk" 'profiler-stop
  "apr" 'profiler-report
  "aps" 'profiler-start
  "apw" 'profiler-report-write-profile
  "ar" 'ranger
  "as" 'speedbar
  "au" 'disk-usage
  "aU" 'disk-usage-by-types
  "aw" 'wttrin

  "b" '(:ignore t :wk "buffer")
  "bB" 'bury-buffer
  "bD" 'my/kill-all-buffers
  "bH" 'my/kill-all-buffers-then-switch-to-dashboard
  "bI" 'ibuffer
  "bO" 'my/kill-other-windows-buffers
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
  "fz" 'zoxide-find-file
  "ft" 'zoxide-travel
  "fs" 'sudo-edit
  "fy" 'my/yank-and-show-buffer-full-path

  "g" '(:ignore t :wk "go")
  "gf" 'find-function
  "gg" 'avy-goto-char-timer
  "gv" 'find-variable
  "gb" '(browse-at-remote :wk "browse remote url")
  "gy" '(browse-at-remote-kill :wk "yank remote url")

  "gp" '(:ignore t :wk "pass")
  "gpf" 'password-store-copy-field
  "gpp" 'ivy-pass
  "gpu" 'password-store-url

  "gpg" '(:ignore t :wk "generate")
  "gpgh" 'password-generator-phonetic
  "gpgp" 'password-generator-paranoid
  "gpgs" 'password-generator-strong
  "gpgx" '(my/gopass-generate-xkcd-passwords :wk "xkcd passwords")
  "gpgm" '(my/gopass-generate-passwords :wk "many passwords")

  "h" '(:ignore t :wk "help")
  "hI" 'info-display-manual
  "hi" 'info
  "hl" 'counsel-find-library
  "hn"  'view-emacs-news
  "ht" 'evil-tutor-start
  "hw"  'woman

  "hd" '(:ignore t :wk "describe")
  "hdB" 'evil-collection-describe-bindings
  "hdK" 'my/describe-keymap
  "hdM" 'helpful-macro
  "hdP" 'describe-package
  "hdS" 'counsel-info-lookup-symbol
  "hdb" 'counsel-descbinds
  "hdc" 'describe-char
  "hdf" 'counsel-describe-function
  "hdg" 'general-describe-keybindings
  "hdk" 'helpful-key
  "hdm" 'describe-mode
  "hdp" 'helpful-at-point
  "hds" 'helpful-symbol
  "hdt" 'describe-theme
  "hdu" 'counsel-unicode-char
  "hdv" 'counsel-describe-variable

  "m" '(:ignore t :wk "magit")
  "mG" 'hydra-git-gutter/body
  "mI" 'magit-init
  "mb" 'magit-blame
  "mc" 'magit-clone
  "mg" 'counsel-git-grep
  "ml" 'counsel-git-log
  "mm" 'magit-status
  "mt" 'git-timemachine

  "n" '(:ignore t :wk "narrow")
  "nf" 'narrow-to-defun
  "np" 'narrow-to-page
  "nr" 'narrow-to-region
  "nw" 'widen

  "o" '(:ignore t :wk "org")
  "oA" 'org-archive-to-archive-sibling
  "oS" 'org-store-link
  "oa" 'org-agenda
  "oc" 'org-capture
  "oi" 'org-indent-mode
  "om" 'org-modern-mode
  "op" 'org-pomodoro
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
  "tg" '(zoom-mode :wk "zoom-mode golden ratio")
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
  "wc" 'my/center-horizontal-split
  "wd" 'delete-window
  "we" 'evil-window-up
  "wg" '(zoom :wk "zoom golden ratio")
  "wi" 'evil-window-right
  "wk" 'kill-buffer-and-window
  "wm" 'evil-window-left
  "wn" 'evil-window-down
  "wo" 'delete-other-windows
  "wx" 'my/toggle-maximize-window

  "x" '(:ignore t :wk "text manipulation")
  "xC" 'my/sort-lines-by-column-reverse
  "xL" 'my/sort-lines-reverse
  "xR" 'reverse-region
  "xa" 'my/analyze-word-count
  "xc" 'my/sort-lines-by-column
  "xd" 'define-word-at-point
  "xl" 'my/sort-lines
  "xu" 'my/uniquify-lines
  ;; binding this to an easy key because I use it a lot.
  "xx" 'fill-paragraph

  "xr" '(:ignore t :wk "randomize")
  "xrl" 'my/randomize-lines
  "xrw" 'my/randomize-words

  "xe" '(:ignore t :wk "line endings")
  "xec" 'my/delete-carrage-returns
  "xed" 'my/unix2dos
  "xeu" 'my/dos2unix)

(provide 'init-keys)
