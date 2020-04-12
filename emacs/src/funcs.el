;;; -*- lexical-binding: t; -*-

;; silence warning
(declare-function evil-visual-state-p "evil-states")

;; text manipulation related functions:

(defun my/sort-lines-by-column (&optional reverse)
  "Sort lines by the selected column.
A non-nil argument sorts in reverse order."
  (interactive "P")
  (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
         (beg (if region-active (region-beginning) (point-min)))
         (end (if region-active (region-end) (point-max))))
    (sort-columns reverse beg end)))

(defun my/sort-lines-by-column-reverse ()
  "Sort lines by the selected column in reverse order."
  (interactive)
  (my/sort-lines-by-column -1))

(defun my/sort-lines (&optional reverse)
  "Sort lines in a region or the current buffer. A non-nil
argument sorts in reverse order."
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

(defun my/switch-to-dashboard ()
  "Switch to *dashboard* (creates if needed)"
  (interactive)
  (when (not (get-buffer dashboard-buffer-name))
    (generate-new-buffer dashboard-buffer-name))
  (dashboard-refresh-buffer))

(defun my/kill-all-buffers-then-switch-to-dashboard ()
  "Kills all buffers then switches to *dashboard* (creates if needed)"
  (interactive)
  (progn
    (my/kill-all-buffers)
    (my/switch-to-dashboard)))

(defun my/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (kill-buffer)))

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun my/yank-and-show-buffer-full-path ()
  "Yank (i.e. copy) and show the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

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

    (golden-ratio-mode 1)
    (golden-ratio 1)
    (message "Golden-Ratio mode enabled")))

;; depends on https://elpa.gnu.org/packages/adaptive-wrap.html
(defun my/toggle-adaptive-visual-fill-column ()
  "Toggles visual-fill-column-mode and adaptive-wrap-prefix-mode on or off"
  (interactive)
  (if (bound-and-true-p visual-fill-column-mode)
      (progn
        (visual-fill-column-mode -1)
        (adaptive-wrap-prefix-mode -1)
        (message "visual-fill-column and adaptive-wrap-prefix mode disabled"))

    (visual-fill-column-mode 1)
    (adaptive-wrap-prefix-mode 1)
    (message "visual-fill-column and adaptive-wrap-prefix mode enabled")))

(defun my/open-shell ()
  "Opens my prefered shell for the current operating system."
  (interactive)
  (if IS-WINDOWS
      (call-interactively 'eshell)
    (ansi-term "zsh")))

;; MISC functions:

;; https://stackoverflow.com/questions/3480173/show-keys-in-emacs-keymap-value
(defun my/describe-keymap (keymap)
  "Describe a keymap."
  (interactive
   (list (completing-read
          "Keymap: " (let (maps)
                       (mapatoms (lambda (sym)
                                   (and (boundp sym)
                                        (keymapp (symbol-value sym))
                                        (push sym maps))))
                       maps)
          nil t)))
  (with-output-to-temp-buffer (format "*keymap: %s*" keymap)
    (princ (format "%s\n\n" keymap))
    (princ (substitute-command-keys (format "\\{%s}" keymap)))
    (with-current-buffer standard-output ;; temp buffer
      (setq help-xref-stack-item (list #'my/describe-keymap keymap)))))

(defun my/revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

(provide 'funcs)
