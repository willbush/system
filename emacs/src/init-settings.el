;;; -*- lexical-binding: t; -*-
;; Most settings here are taken from or at least inspired by Doom Emacs.

;;
;;; Support Validation

(defconst MIN-EMACS-VERSION "27")

(when (version< emacs-version MIN-EMACS-VERSION)
  (error "Detected Emacs %s. This config supports Emacs %s and higher."
         emacs-version MIN-EMACS-VERSION))

;;
;;; Emacs Core Configuration

;; Longer max number of message buffer line.
(setq message-log-max 5000)

;; Make UTF-8 the default coding system:
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
;; The clipboard's on Windows could be in an encoding that's wider (or thinner)
;; than utf-8, so let Emacs/the OS decide what encoding to use there.
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8))

;; Emacs on Windows frequently confuses HOME (C:\Users\<NAME>) and %APPDATA%,
;; causing `abbreviate-home-dir' to produce incorrect paths.
(when IS-WINDOWS
  (setq abbreviated-home-dir "\\`'"))

;; switches (yes or no) prompts to (y or n)
(defalias 'yes-or-no-p 'y-or-n-p)

;; When Emacs tags get regenerated I want it to reload them without prompting me
;; y or n.
(setq-default tags-revert-without-query 1)

;; When opening help such as `describe-variable', I want the point to
;; be focused on the help buffer so I can quickly exit it.
(setq help-window-select t)

;; scrolling settings
(setq
  scroll-conservatively 1000
  scroll-margin 4
  scroll-step 1
  mouse-wheel-scroll-amount '(6 ((shift) . 1))
  mouse-wheel-progressive-speed nil
  jit-lock-defer-time 0)

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
(setq password-cache-expiry nil
      tramp-default-method (if IS-WINDOWS "plink" "ssh"))

;; warnings
(when (fboundp 'native-compile)
  ;; Suppress numerous gccemacs async native compilation warnings.
  (with-eval-after-load 'warnings
    (add-to-list 'warning-suppress-types '(comp))))

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

;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be at
;; least a little more discerning.
(setq gnutls-verify-error (not (getenv "INSECURE"))
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL:+VERS-TLS1.2"
                (if (and (not IS-WINDOWS)
                         (not (version< emacs-version "26.3"))
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      ;; Emacs is built with `gnutls' by default, so `tls-program' would not
      ;; be used in that case. Otherwise, people have reasons to not go with
      ;; `gnutls', we use `openssl' instead.
      ;; For more details, see https://redd.it/8sykl1
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fall-backs
                    "gnutls-cli -p %p %h"))

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

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

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

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

(provide 'init-settings)
