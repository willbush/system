;;; -*- lexical-binding: t; -*-

(use-package magit
  :commands
  (magit-clone
   magit-gitignore-globally
   magit-init
   magit-status
   magit-dispatch)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit
  :after magit
  :config

  (general-unbind
    :states '(normal visual)
    :keymaps 'magit-mode-map
    "C-w")

  (general-swap-key '(normal visual) 'magit-mode-map
    "C-j" "C-n"
    "C-k" "C-e"
    "g j" "g n"
    "g k" "g e"
    "j" "n"
    "k" "e"
    "v" "r"
    "V" "R"))

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

  ;; Ghcide doesn't have the hlint plugin enabled. Haskell language server will,
  ;; but it's not ready to use (as far as I can tell). So I'm chaining
  ;; haskell-hlint to the lsp flycheck checker. My concern was that it would be
  ;; enabled in other lsp enabled modes such as rustic-mode. However, it seems
  ;; flycheck doesn't enable hlint there because it's only enabled for
  ;; haskell-mode. One can see that with `M-x flycheck-describe-checker
  ;; haskell-hlint'
  (add-hook 'lsp-after-initialize-hook
            (lambda ()
              (flycheck-add-next-checker 'lsp-ui '(warning . haskell-hlint))))

  (general-def
    :states 'normal
    :keymaps 'lsp-browser-mode-map
    "?" 'describe-mode
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
  ;; provides evil bindings for this, but I want mine diff enough that I am just
  ;; doing it myself here.
  (general-def
    :states 'normal
    :keymaps 'lsp-ui-imenu-mode-map
    "?" 'describe-mode
    "RET" 'lsp-ui-imenu--view
    "gd" 'lsp-ui-imenu--visit
    "i" 'lsp-ui-imenu--next-kind
    "m" 'lsp-ui-imenu--prev-kind
    "q" 'lsp-ui-imenu--kill)

  ;; Setup the keybindings for `lsp-ui-flycheck-list-mode'.
  (general-def
    :states 'normal
    :keymaps 'lsp-ui-flycheck-list-mode-map
    "?" 'describe-mode
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

(provide 'init-prog-tools)
