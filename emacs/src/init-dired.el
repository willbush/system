;;; -*- lexical-binding: t; -*-

;;
;;; Dired Settings

;; dired attempts to guess the default target for copy/rename etc.
(setq-default dired-dwim-target t)

;; ‘always’ means to copy recursively without asking.
(setq-default dired-recursive-copies 'always)

;; better dired sorting defaults
;; https://www.emacswiki.org/emacs/DiredSorting
(setq dired-listing-switches "-lah  --group-directories-first")


;;;###autoload
(defun my/dired-xdg-open ()
  "Open marked files or file at point in dired with xdg-open."
  (interactive)
  (if IS-LINUX
      (mapc
       (lambda (file-path) (start-process "" nil "xdg-open" file-path))
       (dired-get-marked-files))
    (message "xdg-open not available outside of Linux.")))

;;
;;; Dired Keybindings

;; I bind only commands I deem worthy enough to be on the main normal state
;; keymap. Everything use goes under the "," prefix (below).
(general-def
  :states 'normal
  :keymaps 'dired-mode-map
  ;; movement
  "n" 'dired-next-line
  "e" 'dired-previous-line
  "<" 'dired-prev-dirline
  ">" 'dired-next-dirline
  "]]" 'dired-next-dirline
  "[[" 'dired-prev-dirline

  ;; going places
  "RET" 'dired-find-file ;; basically opposite of `dired-up-directory'
  "C-e" 'dired-up-directory
  "S-<return>" 'dired-find-file-other-window
  "M-<return>" 'dired-display-file

  ;; marking / flagging
  "k" 'dired-mark
  "u" 'dired-unmark
  "t" 'dired-toggle-marks
  "U" 'dired-unmark-all-marks
  "d" 'dired-flag-file-deletion
  "#" 'dired-flag-auto-save-files
  "~" 'dired-flag-backup-files
  "x" 'dired-do-flagged-delete

  "!" 'dired-do-shell-command
  "&" 'dired-do-async-shell-command
  "+" 'dired-create-directory
  "=" 'dired-diff
  "C" 'dired-do-copy
  "D" 'dired-do-delete
  "S" 'dired-do-symlink
  "Z" 'dired-do-compress
  "c" 'dired-do-compress-to
  "gr" 'revert-buffer
  "l" 'dired-toggle-read-only
  "v" 'dired-do-rename ;; mnemonic is the v in the mv bash command
  "<delete>" 'dired-unmark-backward)

(general-def
  :prefix ","
  :states 'normal
  :keymaps 'dired-mode-map
  "q" 'quit-window

  "c" '(:ignore t :which-key "change file bits")
  "cg" 'dired-do-chgrp
  "cm" 'dired-do-chmod
  "co" 'dired-do-chown
  "ct" 'dired-do-touch ;; changes file timestamp
  "d" '(:ignore t :which-key "dired do extra")
  "db" 'dired-do-byte-compile
  "dc" 'dired-clean-directory
  "dh" 'dired-do-hardlink
  "dl" 'dired-do-load
  "dp" 'dired-do-print
  "dt" 'dired-show-file-type
  "f" '(:ignore t :which-key "flag extra")
  "fg" 'dired-flag-garbage-files
  "fr" 'dired-flag-files-regexp
  "g" '(:ignore t :which-key "go")
  "gO" 'dired-find-file-other-window
  "gf" 'dired-find-file
  "go" 'dired-view-file
  "gr" 'dired-do-redisplay ;; Not sure when I would want to use this
  "gu" 'browse-url-of-dired-file
  "gx" 'my/dired-xdg-open
  "k" '(:ignore t :which-key "mark extra")
  "k(" 'dired-mark-sexp
  "kD" 'dired-downcase
  "kO" 'dired-mark-omitted
  "kS" 'dired-mark-subdir-files
  "kU" 'dired-upcase
  "kc" 'dired-change-marks
  "kd" 'dired-mark-directories
  "kr" 'dired-mark-files-regexp
  "ks" 'dired-mark-symlinks
  "kx" 'dired-mark-executables
  "r" '(:ignore t :which-key "regexp extra")
  "rF" 'dired-do-find-regexp-and-replace
  "rc" 'dired-do-copy-regexp
  "rf" 'dired-do-find-regexp
  "rg" 'dired-mark-files-containing-regexp
  "rh" 'dired-do-hardlink-regexp
  "rs" 'dired-do-symlink-regexp
  "rv" 'dired-do-rename-regexp
  "s" '(:ignore t :which-key "subdir extra")
  "sH" 'dired-hide-all
  "sh" 'dired-hide-subdir
  "si" 'dired-maybe-insert-subdir
  "sk" 'dired-kill-subdir ;; seems to be the opposite of in `dired-maybe-insert-subdir'
  "t" '(:ignore t :which-key "toggle")
  "td" 'dired-hide-details-mode
  "tk" 'dired-toggle-marks
  "ts" 'dired-sort-toggle-or-edit)

;;
;;; Dired Packages

(use-package dired-narrow
  :commands
  (dired-narrow dired-narrow-regexp dired-narrow-fuzzy)

  :init
  (general-def
    :prefix ","
    :states 'normal
    :keymaps 'dired-mode-map
    :major-modes t
    "n" '(:ignore t :which-key "dired-narrow")
    "nn" 'dired-narrow
    "nf" 'dired-narrow-fuzzy
    "nr" 'dired-narrow-regexp))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(unless IS-WINDOWS ;; Windows can't handle this for some reason.
  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode)))

(provide 'init-dired)
