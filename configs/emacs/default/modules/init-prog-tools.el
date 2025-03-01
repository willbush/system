;;; -*- lexical-binding: t; -*-


(use-package dape
  :commands (dape hydra-dape-dispatch/body)
  :init
  (defhydra hydra-dape-dispatch (:color pink :hint nil)
    "
^Stepping^                ^Breakpoints^             ^Info^                    ^Quit

_n_: Next                 _B_: Remove all           _<_: Select Stack up      _q_: Quit
_s_: Step in              _b_: Toggle               _>_: Select Stack down    _k_: Kill
_o_: Step out             _e_: Expression           _R_: Repl
_c_: Continue             _l_: Log                  _i_: Info
_p_: Pause                _w_: Toggle Watch Expr    _m_: Memory
_r_: Restart              _x_: Eval Expression      _s_: Select Stack
^ ^                       ^ ^                       _t_: Select Thread
"
    ("n" dape-next)
    ("s" dape-step-in)
    ("o" dape-step-out)
    ("c" dape-continue)
    ("p" dape-pause)
    ("r" dape-restart)
    ("B" dape-breakpoint-remove-all)
    ("b" dape-breakpoint-toggle)
    ("e" dape-breakpoint-expression)
    ("l" dape-breakpoint-log)
    ("w" dape-watch-dwim)
    ("x" dape-evaluate-expression)
    ("<" dape-stack-select-up)
    (">" dape-stack-select-down)
    ("R" dape-repl)
    ("i" dape-info)
    ("m" dape-read-memory)
    ("s" dape-select-stack)
    ("t" dape-select-thread)
    ("q" dape-quit :color blue)
    ("k" dape-kill :color blue))

  ;; Display hydra on startup
  ;; (add-hook 'dape-on-start-hooks #'hydra-dape-dispatch/body)

  :config
  (setq dape-key-prefix nil)
  (setq dape-buffer-window-arrangement 'right))


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

  (defhydra hydra-magit-blame (:color pink :hint nil)
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
  (defhydra hydra-git-timemachine (:color pink :hint nil)
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


(use-package format-all
  :commands (format-all-mode format-all-buffer)
  :hook ((nix-mode
          bash-mode
          bash-ts-mode
          yaml-mode
          yaml-ts-mode) . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("Nix" nixfmt)
                  ("Shell" (shfmt "--indent" "2" "--case-indent" "--space-redirects"))
                  ("YAML" prettier))))


(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c C-i" . eglot-inlay-hints-mode)
              ("C-c C-r" . eglot-reconnect)
              ("C-c C-s" . eglot-shutdown)
              ("C-c a" . eglot-code-actions)
              ("C-c d" . eglot-find-declaration)
              ("C-c f" . eglot-format-buffer)
              ("C-c h" . eldoc)
              ("C-c i" . eglot-find-implementation)
              ("C-c o" . eglot-code-action-organize-imports)
              ("C-c r" . eglot-rename)
              ("C-c t" . eglot-find-typeDefinition))

  :hook ((rust-mode nix-mode) . eglot-ensure)
  :config
  (setq eglot-autoreconnect nil
        eldoc-echo-area-prefer-doc-buffer t
        ;; I don't look at the events buffer so disable it for modest
        ;; performance increase - https://joaotavora.github.io/eglot/#Performance-1
        eglot-events-buffer-config '(:size 0 :format full)
        eglot-inlay-hints-mode nil)

  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

  (add-to-list 'eglot-server-programs
               '(nix-mode . ("nil"))))


(use-package git-gutter
  :hook ((markdown-mode
         org-mode
         prog-mode
         conf-mode) . git-gutter-mode)
  :config
  (defhydra hydra-git-gutter
    (:body-pre (git-gutter-mode 1) :color pink :hint nil)
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
         "\\.xml\\'"
         ;; C# .NET stuff
         "\\.xaml\\'"
         "\\.csproj\\'"
         "\\.props\\'")
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
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(provide 'init-prog-tools)
