;;; -*- lexical-binding: t; -*-

;;
;;; Org mode

(use-package org
  :ensure nil ;; Org is included in Emacs.
  :defer 2
  :commands (org-mode
             org-agenda
             org-capture)
  :mode ("\\.org$" . org-mode)
  :config

  (add-hook 'org-mode-hook
            (lambda ()
              (setq show-trailing-whitespace t)))

  (general-unbind
    :keymaps 'org-mode-map
    "<M-S-return>"
    "<M-return>"
    "M-RET"
    "M-S-RET")

  (general-def
    :states '(normal visual)
    :keymaps 'org-mode-map
    :major-modes t
    "TAB" 'org-cycle
    "RET" 'org-return)

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
    "n" 'org-move-subtree-down)

  ;; Allows me to set the width of an inline image.
  ;; #+ATTR_ORG: :width 100
  ;; [[~/images/example.jpg]]
  (setq org-image-actual-width nil)

  (setq org-export-with-toc nil)

  (setq org-catch-invisible-edits 'show-and-error)

  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))

  (setq org-src-window-setup 'current-window
        org-log-done 'time)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda
   org-agenda-span 30
   org-agenda-start-on-weekday nil
   org-agenda-start-day "-3d"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

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

;; Github Flavored Markdown exporter for Org Mode
(use-package ox-gfm :after org)

(use-package org-modern
  :commands (global-org-modern-mode org-modern-mode)
  :defer 2 ;; load if not loaded already because it takes ~300 ms to load
  :init
  (with-eval-after-load 'org (global-org-modern-mode))
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka"))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

;;
;;; Markdown

(use-package markdown-mode
  :defer 2 ;; load if not loaded already because eldoc uses it in rust-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq show-trailing-whitespace t))))

;; markdown preview
(use-package grip-mode
  :commands grip-mode
  :init
  (general-def
    :keymaps 'markdown-mode-command-map
    "g" 'grip-mode)
  :config
  (setq grip-github-user "willbush")
  (setq grip-github-password
        (funcall
         (lambda ()
           (nth 0 (process-lines "gopass" "show" "will/websites/general/github-pat-grip"))))))

(use-package yaml-mode
  :mode ("\\.yaml$" . yaml-mode)
  :init
  (add-hook 'yaml-mode-hook
            (lambda ()
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
