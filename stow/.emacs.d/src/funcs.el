;;; -*- lexical-binding: t; -*-

;; text manipulation related functions:

(defun my/sort-lines-by-column (&optional reverse)
  "Sort lines by the selected column,using a visual
block/rectangle selection. A non-nil argument sorts in REVERSE
order."
  (interactive "P")
  (if (and
       ;; is there an active selection
       (or (region-active-p) (evil-visual-state-p))
       ;; is it a block or rectangle selection
       (or (eq evil-visual-selection 'block) (eq rectangle-mark-mode t))
       ;; is the selection height 2 or more lines
       (>= (1+ (- (line-number-at-pos (region-end))
                  (line-number-at-pos (region-beginning)))) 2))
      (sort-columns reverse (region-beginning) (region-end))
    (error "Sorting by column requires a block/rect selection on 2 or more lines.")))

(defun my/sort-lines-by-column-reverse ()
  "Sort lines by the selected column in reverse order,using a
visual block/rectangle selection."
  (interactive)
  (my/sort-lines-by-column -1))

(defun my/sort-lines (&optional reverse)
  "Sort lines in a region or the current buffer. A non-nil argument sorts in reverse order."
  (interactive "P")
  (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
         (beg (if region-active (region-beginning) (point-min)))
         (end (if region-active (region-end) (point-max))))
    (sort-lines reverse beg end)))

(defun my/sort-lines-reverse ()
  "Sort lines in reverse order, in a region or the current buffer."
  (interactive)
  (my/sort-lines -1))

(defun my/uniquify-lines ()
  "Remove duplicate adjacent lines in a region or the current buffer"
  (interactive)
  (save-excursion
    (save-restriction
      (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
             (beg (if region-active (region-beginning) (point-min)))
             (end (if region-active (region-end) (point-max))))
        (goto-char beg)
        (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
          (replace-match "\\1"))))))

;; window / buffer related functions:

(defun my/kill-all-buffers ()
  "kill all buffers"
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

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

(defun my/switch-to-dashboard ()
  "Switch to *dashboard* (creates if needed)"
  (interactive)
  (let ((buffer "*dashboard*"))
    (when (not (get-buffer buffer))
      (dashboard-insert-startupify-lists))
    (switch-to-buffer buffer)
    (dashboard-refresh-buffer)))

(provide 'funcs)
