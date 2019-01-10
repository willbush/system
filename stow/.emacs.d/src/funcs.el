;;; -*- lexical-binding: t; -*-

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
