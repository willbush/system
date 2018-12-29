;;; -*- lexical-binding: t; -*-

(defun my/open-shell ()
  "Opens my prefered shell for the current operating system."
  (interactive)
  (if (eq system-type 'windows-nt)
      (call-interactively 'eshell)
    (call-interactively 'ansi-term)))

(defun my/kill-all-buffers ()
  "kill all buffers"
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun my/switch-to-dashboard ()
  "Switch to *dashboard* (creates if needed)"
  (interactive)
  (let ((buffer "*dashboard*"))
    (when (not (get-buffer buffer))
      (dashboard-insert-startupify-lists))
    (switch-to-buffer buffer)
    (dashboard-refresh-buffer)))

(defun my/switch-to-messages ()
  "Switch to *Messages* buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun my/switch-to-scratch ()
  "Switch to *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun my/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (kill-buffer)))

(defun my/toggle-maximize-window ()
  "Toggle between maximizing the window and restoring previous window setup."
  (interactive)
  (if (and (= 1 (length (window-list))) (assoc ?_ register-alist))
      (jump-to-register ?_)

    (window-configuration-to-register ?_)
    (delete-other-windows)))

(defun my/org-todo-force-notes ()
  "calls 'org-todo and makes it so that it will prompt for a note."
  (interactive)
  (let ((org-todo-log-states
         (mapcar (lambda (state) (list state 'note 'time))
                 (apply 'append org-todo-sets))))
    (call-interactively 'org-todo)))

(defun my/toggle-golden-ratio ()
  "Toggles golden ratio mode on and off"
  (interactive)
  (if (bound-and-true-p golden-ratio-mode)
      (progn
        (golden-ratio-mode -1)
        (balance-windows)
        (message "Golden-Ratio mode disabled"))

    (golden-ratio-mode)
    (golden-ratio)
    (message "Golden-Ratio mode enabled")))

(provide 'funcs)
