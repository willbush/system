;;; -*- lexical-binding: t; -*-

(add-hook 'org-mode-hook
          '(lambda ()
             (setq show-trailing-whitespace t)))

(general-def
  :states 'normal
  :keymaps 'org-mode-map
  "M-n" 'org-move-subtree-down
  "M-e" 'org-move-subtree-up
  "M-m" 'org-do-promote
  "M-i" 'org-do-demote
  "C-n" 'org-forward-heading-same-level
  "C-e" 'org-backward-heading-same-level)

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

(setq org-agenda-files
      '("~/org/work/tasks.org"
        "~/org/work/someday.org"
        "~/org/work/tickler.org"

        "~/org/play/tasks.org"
        "~/org/play/someday.org"
        "~/org/play/tickler.org"

        "~/org/rest/tasks.org"
        "~/org/rest/someday.org"
        "~/org/rest/tickler.org"))

(setq org-capture-templates
      '(("w" "work project")
        ("wt" "Tasks [work]"
         entry
         (file+headline "~/org/work/tasks.org" "Tasks [Work Project]")
         "* TODO %i%?")
        ("ws" "Someday [work]"
         entry
         (file+headline "~/org/work/someday.org" "Someday [Work Project]")
         "* TODO %i%?")
        ("wT" "Tickler [work]"
         entry
         (file+headline "~/org/work/tickler.org" "Tickler [Work Project]")
         "* TODO %i%? \n %U")

        ("p" "play project")
        ("pt" "Tasks [play]"
         entry
         (file+headline "~/org/play/tasks.org" "Tasks [Play Project]")
         "* TODO %i%?")
        ("ps" "Someday [play]"
         entry
         (file+headline "~/org/play/someday.org" "Someday [Play Project]")
         "* TODO %i%?")
        ("pT" "Tickler [play]"
         entry
         (file+headline "~/org/play/tickler.org" "Tickler [Play Project]")
         "* TODO %i%? \n %U")

        ("r" "rest project")
        ("rt" "Tasks [rest]"
         entry
         (file+headline "~/org/rest/tasks.org" "Tasks [Rest Project]")
         "* TODO %i%?")
        ("rs" "Someday [rest]"
         entry
         (file+headline "~/org/rest/someday.org" "Someday [Rest Project]")
         "* TODO %i%?")
        ("rT" "Tickler [rest]"
         entry
         (file+headline "~/org/tasks/tickler.org" "Tickler [Rest Project]")
         "* TODO %i%? \n %U")))

(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3)))

(setq org-outline-path-complete-in-steps nil) ; Refile in a single go
(setq org-refile-use-outline-path t) ; Show full paths for refiling

;; Puts archive files into a relative path to an archive folder with
;; the year in the file name. See doc string for info on special
;; format string syntax
(setq org-archive-location
      (concat "archive/"
              (format-time-string "%Y" (current-time)) "-%s_archive::"))

;; Lets you pull text into an org buffer for editing. Useful for editing
;; comments or doc strings in org mode.
(use-package poporg :commands poporg-dwim)

(provide 'init-org)
