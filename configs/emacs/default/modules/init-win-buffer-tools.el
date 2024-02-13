;;; -*- lexical-binding: t; -*-

(use-package zoom
  :commands zoom zoom-mode
  :config
  (custom-set-variables
   ;; golden ratio
  '(zoom-size '(0.618 . 0.618))))

(use-package adaptive-wrap
  :commands adaptive-wrap-prefix-mode)

(use-package visual-fill-column
  :commands visual-fill-column-mode)

(use-package sudo-edit
  :commands sudo-edit)

(provide 'init-win-buffer-tools)
