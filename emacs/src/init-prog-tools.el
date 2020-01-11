;;; -*- lexical-binding: t; -*-

(use-package ediff)

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
  :hook (haskell-mode . flycheck-mode))

(add-hook 'prog-mode-hook
          '(lambda ()
             (setq show-trailing-whitespace t)))

(use-package attrap :commands attrap-attrap)

(use-package aggressive-indent
  :hook ((css-mode
          elisp-mode
          rustic-mode
          java-mode) . aggressive-indent-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (rustic-mode . lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

(provide 'init-prog-tools)
