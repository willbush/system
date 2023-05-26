;;; -*- lexical-binding: t; -*-
;; Default settings inspired by Doom and Crafted Emacs.

;;
;;; Emacs Core Configuration

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode +1)

;; Longer max number of message buffer line.
(setq message-log-max 5000)

;; Make UTF-8 the default coding system:
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; yes-or-no-p uses shorter answers "y" or "n".
(setq use-short-answers t)

;; When Emacs tags get regenerated I want it to reload them without prompting me
;; y or n.
(setq-default tags-revert-without-query 1)

;; When opening help such as `describe-variable', I want the point to
;; be focused on the help buffer so I can quickly exit it.
(setq help-window-select t)

;;
;;; Scrolling
(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; More performant rapid scrolling over unfontified regions. May cause
      ;; brief spells of inaccurate syntax highlighting right after scrolling,
      ;; which should quickly self-correct.
      fast-but-imprecise-scrolling t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(6 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2
      mouse-wheel-progressive-speed nil)

;; keeps a faint highlight on the line of the point. Note I found there is a
;; cost to this being on where every vertical movement causes emacs takes up
;; ~15% CPU. Edit: after changing some performance related settings, this is now
;; closer to ~8 CPU. I find it worth having.
(global-hl-line-mode 1)

;; follows symlinks without prompt when set to t
(setq vc-follow-symlinks t)

;; allow narrow to region
(put 'narrow-to-region 'disabled nil)

;; tramp:
(setq password-cache-expiry nil)

;; warnings
(when (fboundp 'native-compile)
  ;; Suppress numerous gccemacs async native compilation warnings.
  (with-eval-after-load 'warnings
    (add-to-list 'warning-suppress-types '(comp))))


;;
;;; Windows / Frames

;; Favor vertical splits over horizontal ones. Without this something as simple
;; as decreasing the font size can make `split-window-sensibly' favor splitting
;; windows below.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Choice.html
(setq split-width-threshold 160
      split-height-threshold nil)

;;
;;; Backup Settings

;; Put all backups in one directory (Emacs auto makes this directory as needed)
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

(defconst my/auto-save-dir (expand-file-name "backups/auto-saves" user-emacs-directory))

;; Put all auto-save files into one directory (will get an error on auto-save if
;; this directory doesn't exist)
(unless (file-exists-p my/auto-save-dir)
  (make-directory my/auto-save-dir t))

(setq auto-save-file-name-transforms
  `((".*" ,my/auto-save-dir t)))

;; backup / file related settings
(setq make-backup-files t
      ;; enable file backup even if the file is under source control
      vc-make-backup-files t
      ;; Always use copying to create backup files.
      backup-by-copying t
      ;; newest versions to keep when a new numbered backup is made
      kept-new-versions 10
      ;; oldest versions to keep when a new numbered backup is made
      kept-old-versions 10
      ;; use version numbers in backup files
      version-control t
      ;; delete excess backup files silently
      delete-old-versions t
      ;; file and directory deletions will move to trash instead of outright
      ;; deletion.
      delete-by-moving-to-trash t
      auto-save-default t
      ;; number of seconds idle time before auto-save (default: 30)
      auto-save-timeout 20
      ;; number of keystrokes between auto-saves (default: 300)
      auto-save-interval 200)

(defvar my/backup-ignore-regexps (list "\\.gpg$" "\\.asc$")
  "*List of filename regexps to not backup")

(defun my/backup-enable-p (name)
  "Filter certain file backups"
  (when (normal-backup-enable-predicate name)
    (let ((backup t))
      (mapc (lambda (re)
              (setq backup (and backup (not (string-match re name)))))
            my/backup-ignore-regexps)
      backup)))

(setq backup-enable-predicate 'my/backup-enable-p)

;;
;;; Security

;; Emacs stores authinfo in $HOME and in plaintext. Let's not do that, mkay?
;; This file stores usernames, passwords, and other such treasures for the
;; aspiring malicious third party.
(setq auth-sources (list "~/.authinfo.gpg"))

;;
;;; Optimizations

;; Emacs "updates" its UI more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setq idle-update-delay 1)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates miss-configuration (or that the user needs to stop relying on case
;; insensitivity).
(setq auto-mode-case-fold nil)

;; Better support for files with long lines
(global-so-long-mode 1)

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether it has a noteable affect on Linux and Mac hasn't
;; been determined, but we inhibit it there anyway.
(setq inhibit-compacting-font-caches t)

;; A lsp performance tweak https://emacs-lsp.github.io/lsp-mode/page/performance/
;; "Increase the amount of data which Emacs reads from the process. Again the
;; emacs default is too low 4k considering that the some of the language server
;; responses are in 800k - 3M range."
(setq read-process-output-max 1048576) ;; 1 MiB

;; Remove command line options that aren't relevant to Linux; means slightly
;; less to process at startup.
(setq command-line-ns-option-alist nil)

;;
;;; Editing Settings

(setq electric-pair-pairs
  '(
    (?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\})
   ))

(electric-pair-mode t)

;; I don't want two spaces after my periods. This the affects behavior
;; of `fill-paragraph' (among other things).
(setq sentence-end-double-space nil)

;; Prevent indention inserting tabs by default.
(setq-default indent-tabs-mode nil
              tab-width 2)

;; fill-paragraph uses fill-column for the width at which to break lines
(setq-default fill-column 80)

;; Continue wrapped words at white-space, rather than in the middle of
;; a word.
(setq-default word-wrap t)

;; ...but don't do any wrapping by default. It's expensive. Enable
;; `visual-line-mode' if you want soft line-wrapping. `auto-fill-mode'
;; for hard line-wrapping.
(setq-default truncate-lines t)

;; If enabled (and `truncate-lines' was disabled), soft wrapping no
;; longer occurs when that window is less than
;; `truncate-partial-width-windows' characters wide. We don't need
;; this, and it's extra work for Emacs otherwise, so off it goes.
(setq truncate-partial-width-windows nil)

;; The POSIX standard defines a line is "a sequence of zero or more
;; non-newline characters followed by a terminating newline", so files
;; should end in a newline. Windows doesn't respect this (because it's
;; Windows), but we should, since programmers' tools tend to be POSIX
;; compliant.
(setq require-final-newline t)

;; Cull duplicates in the kill ring to reduce bloat and make the kill
;; ring easier to peruse (with `counsel-yank-pop').
(setq kill-do-not-save-duplicates t)

;; Allow UTF or composed text from the clipboard, even in the terminal
;; or on non-X systems (like Windows or macOS), where only `STRING' is
;; used.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;
;;; Extra file extensions to support

(push '("/LICENSE\\'" . text-mode) auto-mode-alist)
(push '("\\.log\\'" . text-mode) auto-mode-alist)

(provide 'init-defaults)
