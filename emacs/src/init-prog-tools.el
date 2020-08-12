;;; -*- lexical-binding: t; -*-

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

(use-package flycheck
  :commands flycheck-add-next-checker
  :hook (lsp-mode . flycheck-mode))

(add-hook 'prog-mode-hook
          '(lambda ()
             (setq show-trailing-whitespace t)))

(use-package format-all :commands (format-all-buffer))

(use-package aggressive-indent
  :hook ((css-mode
          elisp-mode
          rustic-mode
          java-mode) . aggressive-indent-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((rustic-mode
          haskell-mode) . lsp-deferred)
  :config

  ;; I don't want lsp to prompt me to restart it when I close its buffer.
  (setq lsp-restart 'ignore)
  ;; This is the default, but I want to set it explicitly here.
  (setq lsp-rust-server 'rust-analyzer)

  (general-def
    :states 'normal
    :keymaps 'lsp-browser-mode-map
    "q" 'quit-window))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; The position of the popup documentation on hover. I don't like this
  ;; obstructing my view of the code.
  (setq lsp-ui-doc-position 'bottom
        lsp-ui-doc-border "purple4"
        lsp-ui-flycheck-list-position 'right)

  ;; Setup the keybindings for `lsp-ui-imenu'. Note that evil-collection
  ;; provides evil bindings for this, but I want mine different enough that I am
  ;; just doing it myself here.
  (general-def
    :states 'normal
    :keymaps 'lsp-ui-imenu-mode-map
    "RET" 'lsp-ui-imenu--view
    "gd" 'lsp-ui-imenu--visit
    "i" 'lsp-ui-imenu--next-kind
    "m" 'lsp-ui-imenu--prev-kind
    "q" 'lsp-ui-imenu--kill)

  ;; Setup the keybindings for `lsp-ui-flycheck-list-mode'.
  (general-def
    :states 'normal
    :keymaps 'lsp-ui-flycheck-list-mode-map
    "RET" 'lsp-ui-flycheck-list--view
    "gd" 'lsp-ui-flycheck-list--visit
    "q" 'lsp-ui-flycheck-list--quit))

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

(provide 'init-prog-tools)
