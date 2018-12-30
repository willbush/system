;;; -*- lexical-binding: t; -*-

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(add-hook 'org-mode-hook
          '(lambda ()
             (setq show-trailing-whitespace t)
             (define-key org-mode-map (kbd "C-c C-S-t")
               'my/org-todo-force-notes)
             (local-set-key "\M-k" 'org-move-subtree-up)
             (local-set-key "\M-j" 'org-move-subtree-down)
             (local-set-key "\M-h" 'org-do-promote)
             (local-set-key "\M-l" 'org-do-demote)))

(setq org-src-window-setup 'current-window
      org-log-done 'time)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

(setq org-agenda-files
      '("~/Dropbox/org/personal/inbox.org"
        "~/Dropbox/org/personal/tickler.org"))

(setq org-capture-templates
      '(("t" "Todo [inbox]" entry
        (file+headline "~/Dropbox/org/personal/inbox.org" "Inbox Tasks")
        "* TODO %i%?") ("T" "Tickler"
        entry (file+headline "~/Dropbox/org/personal/tickler.org" "Tickler") "* %i%? \n %U")))

(setq org-refile-targets
      '(("~/Dropbox/org/personal/gtd.org" :level . 1)
        ("~/Dropbox/org/personal/someday.org" :level . 1)
        ("~/Dropbox/org/personal/tickler.org" :level . 1)))

;; Puts archive files into a relative path to an archive folder with
;; the year in the file name. See doc string for info on special
;; format string syntax
(setq org-archive-location
      (concat "archive/"
              (format-time-string "%Y" (current-time)) "-%s_archive::"))

;; Lets you pull text into an org buffer for editing. Useful for editing
;; comments or doc strings in org mode.
(use-package poporg
  :commands 'poporg-dwim)

(defun my/org-todo-force-notes ()
  "calls 'org-todo and makes it so that it will prompt for a note."
  (interactive)
  (let ((org-todo-log-states
         (mapcar (lambda (state) (list state 'note 'time))
                 (apply 'append org-todo-sets))))
    (call-interactively 'org-todo)))

(provide 'init-org)
