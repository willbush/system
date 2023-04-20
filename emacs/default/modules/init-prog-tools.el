;;; -*- lexical-binding: t; -*-

;; Taken from: https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html
;;;###autoload
(defun my/magit--with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
  (let ((process-environment
         (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                       (number-to-string (frame-width)))
               process-environment)))
    ;; Clear the result buffer (we might regenerate a diff, e.g., for
    ;; the current changes in our working directory).
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Now spawn a process calling the git COMMAND.
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     ;; Don't query for running processes when emacs is quit.
     :noquery t
     ;; Show the result buffer once the process has finished.
     :sentinel
     (lambda (proc event)
       (when (eq (process-status proc) 'exit)
         (with-current-buffer (process-buffer proc)
           (goto-char (point-min))
           (ansi-color-apply-on-region (point-min) (point-max))
           (setq buffer-read-only t)
           (view-mode)
           (end-of-line)
           ;; difftastic diffs are usually 2-column side-by-side,
           ;; so ensure our window is wide enough.
           (let ((width (current-column)))
             (while (zerop (forward-line 1))
               (end-of-line)
               (setq width (max (current-column) width)))
             ;; Add column size of fringes
             (setq width (+ width
                            (fringe-columns 'left)
                            (fringe-columns 'right)))
             (goto-char (point-min))
             (pop-to-buffer
              (current-buffer)
              `(;; If the buffer is that wide that splitting the frame in
                ;; two side-by-side windows would result in less than
                ;; 80 columns left, ensure it's shown at the bottom.
                ,(when (> 80 (- (frame-width) width))
                   #'display-buffer-at-bottom)
                (window-width
                 . ,(min width (frame-width))))))))))))

;;;###autoload
(defun my/magit-show-with-difftastic (rev)
  "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If REV is given, just use it.
          (when (boundp 'rev) rev)
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (not rev)
      (error "No revision specified")
    (my/magit--with-difftastic
     (get-buffer-create (concat "*git show difftastic " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))

;;;###autoload
(defun my/magit-diff-with-difftastic (arg)
  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If RANGE is given, just use it.
          (when (boundp 'range) range)
          ;; If prefix arg is given, query the user.
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          ;; Otherwise, auto-guess based on position of point, e.g., based on
          ;; if we are in the Staged or Unstaged section.
          (pcase (magit-diff--dwim)
            ('unmerged (error "unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (error "stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let ((name (concat "*git diff difftastic"
                      (if arg (concat " " arg) "")
                      "*")))
    (my/magit--with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))


(use-package magit
  :commands
  (magit-clone
   magit-init
   magit-blame
   magit-status)
  :init
  (evil-set-initial-state 'magit-mode 'normal)
  (evil-set-initial-state 'magit-diff-mode 'normal)
  (evil-set-initial-state 'magit-log-mode 'normal)
  (evil-set-initial-state 'magit-revision-mode 'normal)
  (evil-set-initial-state 'magit-status-mode 'normal)
  (evil-set-initial-state 'magit-stash-mode 'normal)
  (evil-set-initial-state 'magit-process-mode 'normal)
  :config
  (setq magit-completing-read-function 'ivy-completing-read)

  ;; when using ssh for git authentication and this environmental variable is
  ;; not set, magit complains about being unable to read or have access rights
  ;; while git works fine in the terminal.
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

  ;; I bind my own magit dispatch commands here.
  (transient-define-prefix my/magit-dispatch ()
    "Invoke a Magit command from a list of available commands."
    ["Transient and dwim commands"
     [("A" " Apply"         magit-cherry-pick)
      ("gb" "Branch"        magit-branch)
      ("B" " Bisect"        magit-bisect)
      ("c" " Commit"        magit-commit)
      ("d" " Diff"          magit-diff)
      ("D" " Diff (change)" magit-diff-refresh)]
     [("F" "Fetch"          magit-fetch)
      ("P" "Pull"           magit-pull)
      ("l" "Log"            magit-log)
      ("L" "Log (change)"   magit-log-refresh)
      ("o" "Submodule"      magit-submodule)
      ("O" "Subtree"        magit-subtree)]
     [("p" " Push"          magit-push)
      ("_" " Rebase"        magit-rebase)
      ("gt" "Tag"           magit-tag)
      ("gT" "Note"          magit-notes)
      ("V" " Revert"        magit-revert)
      ("X" " Reset"         magit-reset)]
     [("C" "Cherries"       magit-cherry)
      ("z" "Stash"          magit-stash)
      ("!" "Run"            magit-run)
      ("W" "Worktree"       magit-worktree)]]
    ["Applying changes"
     :if-derived magit-mode
     [("a" "Apply"          magit-apply)
      ("v" "Reverse"        magit-reverse)
      ("x" "Discard"        magit-discard)]
     [("s" "Stage"          magit-stage)
      ("u" "Unstage"        magit-unstage)]
     [("S" "Stage all"      magit-stage-modified)
      ("U" "Unstage all"    magit-unstage-all)]]
    ["Essential commands"
     :if-derived magit-mode
     [("gr" "      refresh current buffer"  magit-refresh)
      ("<tab>" "   toggle section at point" magit-section-toggle)
      ("<return>" "visit thing at point"    magit-visit-thing)
      ("C-h m" "   show all key bindings"   describe-mode)]
     [("<escape>" "Quit"        keyboard-quit)
      ("C-g"      "     Quit"   keyboard-quit)
      ("q"        "       Quit" keyboard-quit)]])

  (general-def
    :states '(normal visual)
    :keymaps 'magit-mode-map
    "<C-tab>" 'magit-section-cycle
    "<M-tab>" 'magit-section-cycle-diffs
    "?" 'my/magit-dispatch
    "TAB" 'magit-section-toggle
    "q" 'magit-mode-bury-buffer)

  (general-def
    :states 'normal
    :keymaps 'magit-mode-map
    "!" 'magit-run
    "+" 'magit-diff-more-context
    "-" 'magit-diff-less-context
    "A" 'magit-cherry-pick
    "C" 'magit-cherry
    "D" 'magit-diff-refresh
    "L" 'magit-log-refresh
    "O" 'magit-subtree
    "P" 'magit-pull
    "RET" 'magit-visit-thing
    "S" 'magit-stage-modified
    "gT" 'magit-notes
    "U" 'magit-unstage-all
    "V" 'magit-revert
    "W" 'magit-worktree
    "X" 'magit-reset
    "_" 'magit-rebase
    "a" 'magit-cherry-apply
    "gb" 'magit-branch
    "c" 'magit-commit
    "d" 'magit-diff
    "F" 'magit-fetch
    "gr" 'magit-refresh
    "l" 'magit-log
    "o" 'magit-submodule
    "p" 'magit-push
    "s" 'magit-stage
    "gt" 'magit-tag
    "u" 'magit-unstage
    "v" 'magit-reverse
    "x" 'magit-discard
    "z" 'magit-stash)

  (general-def
    :prefix ","
    :states 'normal
    :keymaps 'magit-mode-map
    "?" 'my/magit-dispatch
    "B" 'magit-bisect
    "E" 'magit-ediff
    "G" 'magit-refresh-all
    "R" 'magit-remote
    "b" 'magit-process-buffer
    "c" 'magit-git-command
    "d" '(:ignore t :wk "difftastic")
    "dd" 'my/magit-diff-with-difftastic
    "ds" 'my/magit-show-with-difftastic
    "e" 'magit-ediff-dwim
    "f" '(:ignore t :wk "file")
    "fr" 'magit-file-rename
    "fu" 'magit-file-untrack
    "i" 'magit-gitignore
    "m" 'magit-merge
    "p" '(:ignore t :wk "patches")
    "pa" '(magit-am :wk "apply patches")
    "pf" '(magit-patch :wk "format patches")
    "r" '(:ignore t :wk "refs")
    "ra" 'magit-show-refs-arguments
    "rc" 'magit-show-refs-current
    "rh" 'magit-show-refs-head
    "ro" 'magit-show-refs-other
    "rr" 'magit-show-refs)

  (defhydra hydra-magit-blame (:hint nil)
    "
magit-blame-mode:

  _RET_ show commit
  _C-n_ next chunk     _gn_ next chunk same commit
  _C-p_ previous chunk _gp_ previous chunk same commit
  _gb_ blame          - Show the commits that added or removed lines in the visited file.
  _ga_ blame addition - For each line show the revision in which it was added.
  _gr_ blame removal  - For each line show the revision in which it was removed.
  _gv_ blame reverse  - For each line show the last revision in which it still exists.
  _q_ quit             _gc_ cycle style

"
    ("C-n" magit-blame-next-chunk)
    ("C-p" magit-blame-previous-chunk)
    ("RET" magit-show-commit)
    ("ga" magit-blame-addition)
    ("gb" magit-blame)
    ("gc" magit-blame-cycle-style)
    ("gn" magit-blame-next-chunk-same-commit)
    ("gp" magit-blame-previous-chunk-same-commit)
    ("gr" magit-blame-removal)
    ("gv" magit-blame-reverse)
    ("q" magit-blame-quit "quit" :color blue))

  (general-def
    :states '(normal visual)
    :keymaps '(magit-blame-mode-map
               magit-blame-read-only-mode-map)
    "?" 'hydra-magit-blame/body
    "C-n" 'magit-blame-next-chunk
    "C-p" 'magit-blame-previous-chunk
    "RET" 'magit-show-commit
    "ga" 'magit-blame-addition
    "gb" 'magit-blame
    "gc" 'magit-blame-cycle-style
    "gn" 'magit-blame-next-chunk-same-commit
    "gp" 'magit-blame-previous-chunk-same-commit
    "gr" 'magit-blame-removal
    "gv" 'magit-blame-reverse
    "q" 'magit-blame-quit)

  ;; Ran into this issue on the magit-blame key bindings:
  ;; https://github.com/noctuid/evil-guide#why-dont-keys-defined-with-evil-define-key-work-immediately
  (add-hook 'magit-blame-mode-hook #'evil-normalize-keymaps))

(use-package magit-delta
  ;; This package does a require on magit which takes ~500ms, so definitely want to lazy load it.
  :commands
  (magit-delta-mode
   my/magit-delta-toggle)
  :if (executable-find "delta")
  ;; Default to off for now due to crippling performance when magit has a large
  ;; number of diffs https://github.com/dandavison/magit-delta/issues/9 :hook
  ;;:hook (magit-mode . magit-delta-mode)
  :init

  (defun my/magit-delta-toggle ()
    "Toggle magit-delta-mode and refresh magit."
    (interactive)
    (progn
      (call-interactively 'magit-delta-mode)
      (magit-refresh)))

  (general-def
    :prefix ","
    :states 'normal
    :keymaps 'magit-mode-map
    "t" '(my/magit-delta-toggle :wk "toggle magit-delta")))

(use-package git-timemachine
  :commands git-timemachine
  :config
  ;; Silence warnings
  (declare-function git-timemachine-show-next-revision "git-timemachine")
  (declare-function git-timemachine-show-next-revision "git-timemachine")
  (declare-function git-timemachine-show-previous-revision "git-timemachine")
  (declare-function git-timemachine-show-revision-fuzzy "git-timemachine")
  (declare-function git-timemachine-show-commit "git-timemachine")
  (declare-function git-timemachine-show-current-revision "git-timemachine")
  (declare-function git-timemachine-show-latest-revision-in-branch "git-timemachine")
  (declare-function git-timemachine-show-nth-revision "git-timemachine")
  (declare-function git-timemachine-blame "git-timemachine")
  (declare-function git-timemachine-kill-abbreviated-revision "git-timemachine")
  (declare-function git-timemachine-kill-revision "git-timemachine")
  (declare-function git-timemachine-switch-branch "git-timemachine")
  (declare-function git-timemachine-quit "git-timemachine")

  (defhydra hydra-git-timemachine (:hint nil)
    "
git-timemachine-mode:

  _C-n_ next revision      _gc_ show commit                     _gb_ blame
  _C-p_ previous revision  _gr_ show current revision           _gy_ yank abbreviated revision
  _C-f_ fuzzy search       _gl_ show latest revision in branch  _gY_ yank full revision
  _q_ quit                 _gn_ show nth revision               _gs_ switch branch

"
    ("C-n" git-timemachine-show-next-revision)
    ("C-p" git-timemachine-show-previous-revision)
    ("C-f" git-timemachine-show-revision-fuzzy)

    ("gc" git-timemachine-show-commit :color blue)
    ("gr" git-timemachine-show-current-revision)
    ("gl" git-timemachine-show-latest-revision-in-branch)
    ("gn" git-timemachine-show-nth-revision)

    ("gb" git-timemachine-blame)
    ("gy" git-timemachine-kill-abbreviated-revision)
    ("gY" git-timemachine-kill-revision)
    ("gs" git-timemachine-switch-branch)

    ("q" git-timemachine-quit "quit" :color blue))

  (general-def
    :definer 'minor-mode
    :states 'normal
    :keymaps 'git-timemachine-mode
    "," 'hydra-git-timemachine/body
    "?" 'hydra-git-timemachine/body

    "C-n" 'git-timemachine-show-next-revision
    "C-p" 'git-timemachine-show-previous-revision
    "C-f" 'git-timemachine-show-revision-fuzzy

    "gc" 'git-timemachine-show-commit
    "gr" 'git-timemachine-show-current-revision
    "gl" 'git-timemachine-show-latest-revision-in-branch
    "gn" 'git-timemachine-show-nth-revision

    "gb" 'git-timemachine-blame
    "gy" 'git-timemachine-kill-abbreviated-revision
    "gY" 'git-timemachine-kill-revision
    "gs" 'git-timemachine-switch-branch

    "q" 'git-timemachine-quit))

(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

(use-package format-all :commands (format-all-buffer))

(use-package eglot
  :ensure nil ;; included in Emacs.
  :config
  (setq eglot-autoreconnect nil)
  (add-to-list 'eglot-server-programs
               '(csharp-mode . ("OmniSharp" "-lsp"))))

(use-package git-gutter
  :hook ((markdown-mode
         org-mode
         prog-mode
         conf-mode) . git-gutter-mode)
  :config
  ;; Silence warnings
  (declare-function git-gutter:next-hunk "git-gutter")
  (declare-function git-gutter:previous-hunk "git-gutter")
  (declare-function git-gutter:stage-hunk "git-gutter")
  (declare-function git-gutter:revert-hunk "git-gutter")
  (declare-function git-gutter:popup-hunk "git-gutter")

  (defhydra hydra-git-gutter
    (:body-pre (git-gutter-mode 1) :hint nil)
   "
 Git gutter:
   _n_: next hunk        _s_tage hunk     _q_uit
   _e_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
   ^ ^                   _p_opup hunk
   _f_: first hunk
   _l_: last hunk
 "
 ("n" git-gutter:next-hunk)
 ("e" git-gutter:previous-hunk)
 ("f" (progn (goto-char (point-min))
             (git-gutter:next-hunk 1)))
 ("l" (progn (goto-char (point-min))
             (git-gutter:previous-hunk 1)))
 ("s" git-gutter:stage-hunk)
 ("r" git-gutter:revert-hunk)
 ("p" git-gutter:popup-hunk)
 ("q" nil :color blue)
 ("Q" (git-gutter-mode -1) :color blue)))

(use-package git-gutter-fringe
  :after git-gutter
  :config
  ;; subtle diff indicators in the fringe places the git gutter outside the
  ;; margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added
    [192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center))

(use-package debug
  :defer
  :ensure nil ;; debugger-mode is included in Emacs.
  :config
  (evil-set-initial-state 'debugger-mode 'normal)

  (general-def
    :states 'normal
    :keymaps 'debugger-mode-map
    "<backtab>" 'backward-button
    "<tab>" 'forward-button
    "RET" 'backtrace-help-follow-symbol
    "q" 'quit-window)

  (general-def
    :prefix ","
    :states 'normal
    :keymaps 'debugger-mode-map
    "C" 'debugger-frame-clear
    "R" 'debugger-record-expression
    "E" 'debugger-reenable
    "c" 'debugger-continue
    "e" 'debugger-eval-expression
    "f" 'debugger-frame
    "j" 'debugger-jump
    "l" 'debugger-list-functions
    "l" 'debugger-toggle-locals
    "q" 'debugger-quit
    "r" 'debugger-return-value
    "t" 'debugger-step-through))

(use-package compile
  :defer
  :ensure nil ;; debugger-mode is included in Emacs.
  :config
  (evil-set-initial-state 'compilation-mode 'normal)

  (general-def
    :states 'normal
    :keymaps 'compilation-mode-map
    "RET" 'compile-goto-error)

  (general-def
    :prefix ","
    :states 'normal
    :keymaps 'compilation-mode-map
    "N" 'compilation-next-file
    "P" 'compilation-previous-file
    "d" 'compilation-display-error
    "n" 'compilation-next-error
    "p" 'compilation-previous-error
    "r" 'recompile))

(use-package browse-at-remote
  :commands browse-at-remote browse-at-remote-kill)

(use-package web-mode
  :mode ("\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.css\\'"
         "\\.djhtml\\'"
         "\\.erb\\'"
         "\\.html?\\'"
         "\\.jsx?\\'"
         "\\.mustache\\'"
         "\\.phtml\\'"
         "\\.scss\\'"
         "\\.tpl\\.php\\'"
         "\\.tsx?\\'"
         "\\.xaml\\'"
         "\\.xml\\'")
  :custom
  (web-mode-css-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-block-face t)
  (web-mode-enable-comment-keywords t)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package ediff
  :ensure nil ;; included in Emacs.
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(provide 'init-prog-tools)
