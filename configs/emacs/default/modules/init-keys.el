;;; -*- lexical-binding: t; -*-

;; Global Bindings
(general-def
  ;; zoom in and out
  "C-+" 'text-scale-increase
  "C--" 'text-scale-decrease
  "C-." 'embark-act
  "C-/" 'dabbrev-completion
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
  :states '(normal visual emacs)
  :keymaps 'override
  "TAB" 'mode-line-other-buffer
  "!" 'shell-command
  "'" 'vterm
  "?" 'embark-bindings
  "SPC" 'execute-extended-command

  "F" '(:ignore t :wk "frame")
  "FD" 'delete-other-frames
  "Fd" 'delete-frame
  "Fi" 'iconify-frame
  "Fm" 'toggle-frame-maximized
  "Fn" 'make-frame
  "Fo" 'other-frame

  "S" '(:ignore t :wk "spell-checking")
  "Sn" 'jinx-next
  "Sp" 'jinx-previous
  "Ss" 'jinx-correct

  "a" '(:ignore t :wk "apps")

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
  "as" 'speedbar

  "b" '(:ignore t :wk "buffer")
  "bB" 'consult-buffer-other-window
  "bD" 'my/kill-all-buffers
  "bO" 'my/kill-other-windows-buffers
  "bb" 'consult-buffer
  "bd" 'my/kill-this-buffer
  "bm" 'my/switch-to-messages

  "bn" '(:ignore t :wk "narrow")
  "bnf" 'narrow-to-defun
  "bnp" 'narrow-to-page
  "bnr" 'narrow-to-region
  "bnw" 'widen

  "br" 'my/revert-buffer-no-confirm
  "bs" 'my/switch-to-scratch

  "c" '(:ignore t :wk "comment")
  "cl" 'comment-line
  "cr" 'comment-or-uncomment-region

  "d" '(:ignore t :wk "dape")
  "dd" 'dape
  "dh" 'hydra-dape-dispatch/body

  "f" '(:ignore t :wk "file")
  "ff" 'find-file
  "ft" 'zoxide-travel
  "fs" 'sudo-edit
  "fr" 'consult-recent-file
  "fy" 'my/yank-and-show-buffer-full-path

  "g" '(:ignore t :wk "go")
  "gf" 'find-function
  "gg" 'avy-goto-char-timer
  "gv" 'find-variable
  "gb" '(browse-at-remote :wk "browse remote url")
  "gy" '(browse-at-remote-kill :wk "yank remote url")

  "h" '(:ignore t :wk "help")
  "hI" 'info-display-manual
  "hi" 'info
  "hl" 'find-library
  "hn"  'view-emacs-news
  "hw"  'woman

  "hd" '(:ignore t :wk "describe")
  "hdb" 'embark-bindings
  "hdB" 'evil-collection-describe-bindings
  "hdK" 'my/describe-keymap
  "hdM" 'helpful-macro
  "hdP" 'describe-package
  "hdc" 'describe-char
  "hdf" 'describe-function
  "hdg" 'general-describe-keybindings
  "hdk" 'helpful-key
  "hdm" 'describe-mode
  "hdp" 'helpful-at-point
  "hds" 'helpful-symbol
  "hdt" 'describe-theme
  "hdv" 'describe-variable

  "l" '(:ignore t :wk "llms")
  "ll" 'gptel
  "ls" 'gptel-send
  "la" 'gptel-abort
  "le" 'gptel-end-of-response

  "k" '(consult-bookmark :wk "bookmark")

  "m" '(:ignore t :wk "magit")
  "mG" 'hydra-git-gutter/body
  "mI" 'magit-init
  "mb" 'magit-blame
  "mc" 'magit-clone
  "mm" 'magit-status
  "mt" 'git-timemachine

  "n" '(:ignore t :wk "org-roam node")
  "nt" 'org-roam-buffer-toggle
  "nf" 'org-roam-node-find
  "ng" 'org-roam-graph
  "ni" 'org-roam-node-insert
  "nc" 'org-roam-capture
  "nd" 'org-roam-dailies-capture-today

  "o" '(:ignore t :wk "org")
  "oA" 'org-archive-to-archive-sibling
  "oS" 'org-store-link
  "oa" 'org-agenda
  "oc" 'org-capture
  "oi" 'org-indent-mode
  "om" 'org-modern-mode
  "or" 'org-refile
  "os" 'org-save-all-org-buffers
  "ot" 'org-toggle-link-display

  "p" '(:ignore t :wk "project")
  "pb" 'consult-project-buffer
  "pd" 'project-dired
  "pf" 'project-find-file
  "pk" 'project-kill-buffers
  "pp" 'my/zoxide-travel-and-project-find

  "q" '(:ignore t :wk "quit")
  "qQ" 'save-buffers-kill-emacs
  "qq" 'save-buffers-kill-terminal

  "r" '(:ignore t :wk "rapid") ;; emphasize easy access over mnemonics
  "rl" 'consult-theme
  "rs" 'save-buffer

  "s" '(:ignore t :wk "search")
  "sc" 'evil-ex-nohighlight ;; mnemonic is search clear
  "sd" 'deadgrep
  "so" 'consult-outline
  "sr" 'consult-ripgrep
  "ss" '(zoxide-travel :wk "zoxide search directory")

  "t" '(:ignore t :wk "toggle")
  "tc" 'display-fill-column-indicator-mode
  "tf" 'auto-fill-mode
  "tg" '(zoom-mode :wk "zoom-mode golden ratio")
  "tl" 'toggle-truncate-lines
  "tn" 'display-line-numbers-mode
  "ts" 'jinx-mode
  "tt" 'display-time-mode
  "tv" 'my/toggle-adaptive-visual-fill-column

  "w" '(:ignore t :wk "window")
  "ww" 'evil-window-rotate-downwards
  "ws" 'split-window-vertically ;; vim like hsplit
  "wv" 'split-window-horizontally ;; vim like vsplit
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
