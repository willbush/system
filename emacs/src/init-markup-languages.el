;;; -*- lexical-binding: t; -*-

;;
;;; Org mode

(use-package org
  :ensure nil ;; Org is included in Emacs.
  :commands (org-mode
             org-agenda
             org-capture)
  :config

  (add-hook 'org-mode-hook
            '(lambda ()
              (setq show-trailing-whitespace t)))

  (general-unbind
    :keymaps 'org-mode-map
    ;; collides with a edwina keybindings
    "<M-S-return>"
    "<M-return>"
    "M-RET"
    "M-S-RET")

  (general-def
    :prefix ","
    :states 'normal
    :keymaps 'org-mode-map
    :major-modes t
    "RET" 'org-meta-return
    "t" 'org-insert-todo-heading
    "e" 'org-move-subtree-up
    "i" 'org-do-demote
    "m" 'org-do-promote
    "n" 'org-move-subtree-down
    "d" '(:ignore t :wk "org-download")
    "dc" 'org-download-clipboard
    "dd" 'org-download-delete
    "de" 'org-download-edit
    "di" 'org-download-image
    "dr" 'org-download-rename-at-point
    "dR" 'org-download-rename-last-file
    "ds" 'org-download-screenshot
    "dy" 'org-download-yank)

  ;; Allows me to set the width of an inline image.
  ;; #+ATTR_ORG: :width 100
  ;; [[~/images/example.jpg]]
  (setq org-image-actual-width nil)

  (setq org-catch-invisible-edits 'show-and-error)

  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))

  (setq org-src-window-setup 'current-window
        org-log-done 'time)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

  (setq org-agenda-span 30
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-3d")

  ;; Set my agenda files to includes all my org files that are not hidden,;
  ;; auto-save, or backups.
  (let ((my-org-notes "~/org"))
    (when (file-directory-p my-org-notes)
      (setq org-agenda-files
            (directory-files-recursively
             my-org-notes
             ;; ^ match beginning of string
             ;; [^.#] do not match . or # characters
             ;; .* match anything
             ;; \\.org\\' match .org literally at the end of the string
             "^[^.#].*\\.org\\'"
             nil
             ;; A predicate that signals to `directory-files-recursively' whether to
             ;; recursively search a directory or not.
             (lambda (dir)
               (let ((name (file-name-nondirectory dir)))
                 (not (or
                       ;; These are the directories I want to exclude:
                       (string= ".git" name)
                       (string= "archive" name)
                       (string= "docs" name)))))))))

  (setq org-capture-templates
        '(("w" "work project")
          ("wt" "Tasks [work]" entry (file+headline "~/org/work/tasks.org" "Tasks [Work Project]") "* TODO %i%?")
          ("ws" "Someday [work]" entry (file+headline "~/org/work/someday.org" "Someday [Work Project]") "* TODO %i%?")
          ("wT" "Tickler [work]" entry (file+headline "~/org/work/tickler.org" "Tickler [Work Project]") "* TODO %i%? \n %U")

          ("p" "play project")
          ("pt" "Tasks [play]" entry (file+headline "~/org/play/tasks.org" "Tasks [Play Project]") "* TODO %i%?")
          ("ps" "Someday [play]" entry (file+headline "~/org/play/someday.org" "Someday [Play Project]") "* TODO %i%?")
          ("pT" "Tickler [play]" entry (file+headline "~/org/play/tickler.org" "Tickler [Play Project]") "* TODO %i%? \n %U")

          ("r" "rest project")
          ("rt" "Tasks [rest]" entry (file+headline "~/org/rest/tasks.org" "Tasks [Rest Project]") "* TODO %i%?")
          ("rs" "Someday [rest]" entry (file+headline "~/org/rest/someday.org" "Someday [Rest Project]") "* TODO %i%?")
          ("rT" "Tickler [rest]" entry (file+headline "~/org/tasks/tickler.org" "Tickler [Rest Project]") "* TODO %i%? \n %U")))

  (setq org-refile-targets '((nil :maxlevel . 3)
                            (org-agenda-files :maxlevel . 3)))

  (setq org-outline-path-complete-in-steps nil) ; Refile in a single go
  (setq org-refile-use-outline-path t) ;; Show full paths for refiling

  ;; Puts archive files into a relative path to an archive folder with
  ;; the year in the file name. See doc string for info on special
  ;; format string syntax
  (setq org-archive-location
        (concat "archive/"
                (format-time-string "%Y" (current-time)) "-%s_archive::")))

(use-package org-download
  ;; without deferring like this package adds 0.1 sec to startup time Ideally
  ;; I'd like to add `(add-hook 'dired-mode-hook 'org-download-enable)' to
  ;; enable dragging and dropping an image directly into dired. However, I want
  ;; `dired' to start up as fast as possible and loading this package on initial
  ;; opening of dired creates a noticeable delay.
  :commands (org-download-enable
             org-download-edit
             org-download-yank
             org-download-image
             org-download-delete
             org-download-clipboard ;; depends on xclip
             org-download-screenshot ;; depends on gnome-screenshot
             org-download-rename-at-point
             org-download-rename-last-file))

(use-package org-pomodoro
  :commands (org-pomodoro
             org-pomodoro-extend-last-clock)
  :custom
  (org-pomodoro-audio-player "mpv")

  (org-pomodoro-start-sound-p t)
  (org-pomodoro-start-sound "~/sync/sounds/mario/powerup.wav")
  (org-pomodoro-start-sound-args "-volume 80")

  (org-pomodoro-finished-sound-p t)
  (org-pomodoro-finished-sound "~/sync/sounds/mario/1up.wav")
  (org-pomodoro-finished-sound-args "-volume 80"))

;;
;;; Markdown

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command '("pandoc" "--from=markdown" "--to=html5"))
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (setq show-trailing-whitespace t))))

(use-package markdown-toc
  :after markdown-mode)

;;
;;; Org and Markdown related tools

(use-package pandoc-mode
  :hook
  ((markdown-mode . pandoc-mode)
   (org-mode . pandoc-mode)
   (pandoc-mode . pandoc-load-default-settings))

  :config
  (general-def
    :prefix ","
    :states 'normal
    :keymaps 'pandoc-mode-map
    "p" 'pandoc-main-hydra/body))

(provide 'init-markup-languages)
