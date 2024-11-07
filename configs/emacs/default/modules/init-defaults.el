;;; -*- lexical-binding: t; -*-

;;
;;; Optimizations

;;; Runtime optimizations
;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; PERF: Disable bidirectional text scanning for a modest performance boost.
;;   I've set this to `nil' in the past, but the `bidi-display-reordering's docs
;;   say that is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses (and other
;;   bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Emacs "updates" its UI more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setq idle-update-delay 1.0)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; PGTK builds only: this timeout adds latency to frame operations, like
;; `make-frame-invisible', which are frequently called without a guard because
;; it's inexpensive in non-PGTK builds. Lowering the timeout from the default
;; 0.1 should make childframes and packages that manipulate them (like `lsp-ui',
;; `company-box', and `posframe') feel much snappier. See emacs-lsp/lsp-ui#613.
(setq pgtk-wait-for-event-timeout 0.001)

;; A lsp performance tweak https://emacs-lsp.github.io/lsp-mode/page/performance/
;; "Increase the amount of data which Emacs reads from the process. Again the
;; emacs default is too low 4k considering that the some of the language server
;; responses are in 800k - 3M range."
(setq read-process-output-max (* 1024 1024)) ;; 1 MiB

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Remove command line options that aren't relevant to Linux; means slightly
;; less to process at startup.
(setq command-line-ns-option-alist nil)

;; PERF: Resizing the Emacs frame (to accommodate fonts that are smaller or
;;   larger than the system font) appears to impact startup time dramatically.
;;   The larger the delta, the greater the delay. Even trivial deltas can yield
;;   up to a ~1000ms loss, depending on font size and `window-system'. PGTK
;;   seems least affected and NS/MAC the most.
(setq frame-inhibit-implied-resize t)

;; PERF,UX: Prevent "For information about GNU Emacs..." line in *Messages*.
;; For some reason this saves ~10ms startup time
(advice-add #'display-startup-echo-area-message :override #'ignore)
;; PERF: Suppress the vanilla startup screen completely. We've disabled it
;;   with `inhibit-startup-screen', but it would still initialize anyway.
;;   This involves file IO and/or bitmap work (depending on the frame type).
(advice-add #'display-startup-screen :override #'ignore)

(add-hook
 'emacs-startup-hook
 (lambda ()
   ;; Better support for files with long lines
   (global-so-long-mode 1)))


;;
;;; Emacs Core Configuration

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; PERF: enabling these modes adds 8-10 ms startup delay.
;; So do it in a hook.
(add-hook
 'emacs-startup-hook
 (lambda ()
   ;; Revert buffers when the underlying file has changed
   (global-auto-revert-mode 1)))

;; yes-or-no-p uses shorter answers "y" or "n".
(setq use-short-answers t)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

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
      pixel-scroll-precision-mode t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;;
;;; Cursor

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;; follows symlinks without prompt when set to t
(setq vc-follow-symlinks t)

;; allow narrow to region
(put 'narrow-to-region 'disabled nil)

;; PERF: enabling these modes adds very small startup delay.
;; So do it in a hook.
(add-hook
 'emacs-startup-hook
 (lambda ()
   ;; The blinking cursor is distracting, but also interferes with cursor settings
   ;; in some minor modes that try to change it buffer-locally (like treemacs) and
   ;; can cause freezing for folks (esp on macOS) with customized & color cursors.
   (blink-cursor-mode -1)

   ;; keeps a faint highlight on the line of the point.
   (global-hl-line-mode 1)))

;;
;;; Tramp

;; Note from Tramp manual:
;;
;; ‘password-cache-expiry’ sets the duration (in seconds) the passwords are
;; remembered. Passwords are never saved permanently nor can they extend beyond
;; the lifetime of the current Emacs session. Set ‘password-cache-expiry’ to
;; ‘nil’ to disable expiration.
(setq password-cache-expiry nil)

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

;; By default, Emacs stores `authinfo' in $HOME and in plain-text. Let's not do
;; that, mkay? This file stores usernames, passwords, and other treasures for
;; the aspiring malicious third party. Need a GPG setup though.
(setq auth-sources (list "~/.authinfo.gpg"))

;;
;;; Editing Settings

;; I don't want two spaces after my periods. This the affects behavior
;; of `fill-paragraph' (among other things).
(setq sentence-end-double-space nil)

;; Prevent indention inserting tabs by default.
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
;; middle of a line.
(setq tabify-regexp "^\t* [ \t]+")

;; fill-paragraph uses fill-column for the width at which to break lines
(setq-default fill-column 80)

;; Continue wrapped words at white-space, rather than in the middle of
;; a word.
(setq-default word-wrap t)

;; ...but don't do any wrapping by default. It's expensive. Enable
;; `visual-line-mode' if you want soft line-wrapping. `auto-fill-mode' for hard
;; line-wrapping.
(setq-default truncate-lines t)

;; If enabled (and `truncate-lines' was disabled), soft wrapping no longer
;; occurs when that window is less than `truncate-partial-width-windows'
;; characters wide. We don't need this, and it's extra work for Emacs otherwise,
;; so off it goes.
(setq truncate-partial-width-windows nil)

;; The POSIX standard defines a line is "a sequence of zero or more
;; non-newline characters followed by a terminating newline", so files
;; should end in a newline. Windows doesn't respect this (because it's
;; Windows), but we should, since programmers' tools tend to be POSIX
;; compliant.
(setq require-final-newline t)

;; Cull duplicates in the kill ring to reduce bloat
(setq kill-do-not-save-duplicates t)

;;
;;; Completion Settings

(add-hook
 'emacs-startup-hook
 (lambda ()
   ;; This is a must for completion
   (savehist-mode 1)))

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Hide commands in M-x which do not apply to the current mode. Corfu commands
;; are hidden, since they are not supposed to be used via M-x.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Emacs 30 and newer: Disable Ispell completion function. Using `cape-dict` as
;; an alternative.
(setq text-mode-ispell-word-completion nil)

;;
;;; Extra file extensions to support

(nconc
 auto-mode-alist
 '(("/LICENSE\\'" . text-mode)
   ("\\.log\\'" . text-mode)
   ("rc\\'" . conf-mode)
   ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)))

;;
;;; Warnings

;; Not really liking that warnings create a buffer at the bottom.
(setq warning-display-at-bottom nil)

(provide 'init-defaults)
