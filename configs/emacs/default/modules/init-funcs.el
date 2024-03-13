;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

;; silence warning
(declare-function evil-visual-state-p "evil-states")

;;
;;; Text manipulation related functions:

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

(defun my/randomize-words (beg end)
  "Randomize the order of words in region."
  (interactive "*r")
  (let ((all (mapcar
              (lambda (w) (if (string-match "\\w" w)
                              ;; Randomize words,
                              (cons (random) w)
                            ;; keep everything else in order.
                            (cons -1 w)))
              (split-string
               (delete-and-extract-region beg end) "\\b")))
        words sorted)
    (mapc (lambda (x)
            ;; Words are numbers >= 0.
            (unless (> 0 (car x))
              (setq words (cons x words))))
          all)
    ;; Random sort!
    (setq sorted (sort words
                       (lambda (a b) (< (car a) (car b)))))
    (mapc
     'insert
     ;; Insert using original list, `all',
     ;; but pull *words* from randomly-sorted list, `sorted'.
     (mapcar (lambda (x)
               (if (> 0 (car x))
                   (cdr x)
                 (prog1 (cdar sorted)
                   (setq sorted (cdr sorted)))))
             all))))

(defun my/randomize-lines (beg end)
  "Randomize lines in region from BEG to END."
  (interactive "*r")
  (let ((lines (split-string
                (delete-and-extract-region beg end) "\n")))
    (when (string-equal "" (car (last lines 1)))
      (setq lines (butlast lines 1)))
    (apply 'insert
           (mapcar 'cdr
                   (sort (mapcar (lambda (x) (cons (random) (concat x "\n"))) lines)
                         (lambda (a b) (< (car a) (car b))))))))

;; from Spacemacs which took from http://www.emacswiki.org/emacs/WordCount
(defun my/analyze-word-count (start end)
  "Count how many times each word is used in the region.
Punctuation is ignored."
  (interactive "r")
  (let (words
        (formatted "")
        (overview (call-interactively 'count-words)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
        (let* ((word (intern (match-string 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
            (setq words (cons (cons word 1) words))))))
    (defun alist_words_compare (a b)
      "Compare elements from an associative list of words count.
Compare them on count first,and in case of tie sort them alphabetically."
      (let ((a_key (car a))
            (a_val (cdr a))
            (b_key (car b))
            (b_val (cdr b)))
        (if (eq a_val b_val)
            (string-lessp a_key b_key)
          (> a_val b_val))))
    (setq words (cl-sort words 'alist_words_compare))
    (while words
      (let* ((word (pop words))
             (name (car word))
             (count (cdr word)))
        (setq formatted (concat formatted (format "[%s: %d], " name count)))))
    (when (called-interactively-p 'interactive)
      (if (> (length formatted) 2)
          (message (format "%s\nWord count: %s"
                           overview
                           (substring formatted 0 -2)))
        (message "No words.")))
    words))

(defun my/dos2unix ()
  "Convert the current buffer to a Unix file encoding."
  (interactive)
  (progn
    (set-buffer-file-coding-system 'utf-8-unix nil)
    (my/delete-carrage-returns)))

(defun my/delete-carrage-returns ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))

(defun my/unix2dos ()
  "Convert the current buffer to a DOS file encoding."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos nil))

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
    (my/switch-to-dashboard)
    (cd "~/")))

(defun my/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (kill-buffer)))

(defun my/kill-other-windows-buffers ()
  "Kill all other windows and buffers"
  (interactive)
  (progn
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (delete-other-windows)))

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
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)

    (window-configuration-to-register ?_)
    (delete-other-windows)))

(defun my/center-horizontal-split ()
  "Nice for ultra-wide screen when you have a buffer taking up the
entire screen. This function splits the window horizontally,
switches the left window to the dashboard and the right window
gets zoomed to make it center."
  (interactive)
  (progn
    (split-window-horizontally)
    (my/switch-to-dashboard)
    (evil-window-right 1)
    (zoom)))

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

;;
;;; MISC functions:

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

(defun my/gopass-generate-xkcd-passwords ()
  "Use gopass to generate xkcd style passwords to a shell buffer"
  (interactive)
  (my/gopass--generate-passwords "gopass pwgen --xkcd --lang en --sep ' '"))

(defun my/gopass-generate-passwords ()
  "Use gopass to generate passwords to a shell buffer"
  (interactive)
  (my/gopass--generate-passwords "gopass pwgen --one-per-line"))

(defun my/gopass--generate-passwords (command)
  (let* ((buf-name "*gopass passwords*")
         (buf (get-buffer-create buf-name)))
    (async-shell-command command buf)

    (when (not (string-equal buf-name (buffer-name (current-buffer))))
      (switch-to-buffer-other-window buf))))

(defun my/rsync-diff-home ()
  "Diff persistent ~ to ephemeral ~"
  (interactive)
  (let* ((buffer-name "*rsync-diff-home*")
         (command
          (concat
           "rsync -amvxx "
           "--dry-run "
           "--no-links "
           "--exclude '/tmp/*' "
           "--exclude '/root/*' "
           "--exclude '.config/emacs/*' "
           "--exclude '.cache/chromium/*' "
           "--exclude '.config/chromium/*' "
           "~/ /nix/persist/home/will/ "
           "| rg -v '^skipping|/$'"))
         (process (start-process-shell-command "rsync" buffer-name command)))
    (set-process-sentinel
     process
     (lambda (_process _event)
       (with-current-buffer buffer-name
         (switch-to-buffer buffer-name)
         (beginning-of-buffer))))))

(defun my/rsync-diff-root ()
  "Diff persistent / to ephemeral /"
  (interactive)
  (let* ((buffer-name "*rsync-diff-root*")
         (password (password-read "Enter sudo password: "))
         (command
          (concat
           "echo " (shell-quote-argument password) " | sudo -S "
           "sudo rsync -amvxx "
           "--dry-run "
           "--no-links "
           "--exclude '/tmp/*' "
           "--exclude '/root/*' "
           "--exclude '.config/emacs/*' "
           "--exclude '.cache/chromium/*' "
           "--exclude '.config/chromium/*' "
           "/ /nix/persist/ "
           "| rg -v '^skipping|/$'"))
         (process (start-process-shell-command "rsync" buffer-name command)))
    (set-process-sentinel
     process
     (lambda (_process _event)
       (with-current-buffer buffer-name
         (switch-to-buffer buffer-name)
         (beginning-of-buffer))))))

(defun my/rsync-find-orphaned-files ()
  "Find orphaned files in /nix/persist"
  (interactive)
  (let* ((buffer-name "*rsync-orphaned-files*")
         (password (password-read "Enter sudo password: "))
         (command
          (concat
           "echo " (shell-quote-argument password) " | sudo -S "
           "sudo rsync -amvxx "
           "--dry-run "
           "--no-links "
           "/nix/persist/ /"
           "| rg -v '^skipping|/$'"))
         (process (start-process-shell-command "rsync" buffer-name command)))
    (set-process-sentinel
     process
     (lambda (_process _event)
       (with-current-buffer buffer-name
         (switch-to-buffer buffer-name)
         (beginning-of-buffer))))))

(defun my/zoxide-travel-and-project-find ()
  "Travel to a directory using zoxide, then find a file using consult-project-extra-find."
  (interactive)
  (call-interactively #'zoxide-travel)
  (call-interactively #'consult-project-extra-find))

(provide 'init-funcs)
