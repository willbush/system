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
      '("~/org/personal/inbox.org"
        "~/org/personal/tickler.org"))

(setq org-capture-templates
      '(("t" "Todo [inbox]" entry
        (file+headline "~/org/personal/inbox.org" "Inbox Tasks")
        "* TODO %i%?") ("T" "Tickler"
        entry (file+headline "~/org/personal/tickler.org" "Tickler") "* %i%? \n %U")))

(setq org-refile-targets
      '(("~/org/personal/gtd.org" :level . 1)
        ("~/org/personal/someday.org" :level . 1)
        ("~/org/personal/tickler.org" :level . 1)))

;; Puts archive files into a relative path to an archive folder with
;; the year in the file name. See doc string for info on special
;; format string syntax
(setq org-archive-location
      (concat "archive/"
              (format-time-string "%Y" (current-time)) "-%s_archive::"))

;; Lets you pull text into an org buffer for editing. Useful for editing
;; comments or doc strings in org mode.
(use-package poporg
  :commands poporg-dwim)

(provide 'init-org)
