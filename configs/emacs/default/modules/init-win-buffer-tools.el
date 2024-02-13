;;; -*- lexical-binding: t; -*-

(use-package eyebrowse
  :hook (after-init . eyebrowse-mode)
  :config
  (setq eyebrowse-wrap-around t
        eyebrowse-new-workspace t
        eyebrowse-default-workspace-slot 0)

  ;; When these keys are easy to get reach all the other functions provided by
  ;; eyebrowse become way less useful. Therefore, this is all I feel I need to
  ;; bind.
  (general-def
    :states '(normal visual emacs)
    :keymaps 'override
    ;; I'm binding these like I do in Xmonad
    "M-q" 'eyebrowse-switch-to-window-config-0
    "M-w" 'eyebrowse-switch-to-window-config-1
    "M-f" 'eyebrowse-switch-to-window-config-2
    "M-p" 'eyebrowse-switch-to-window-config-3
    "M-b" 'eyebrowse-switch-to-window-config-4
    "M-a" 'eyebrowse-switch-to-window-config-5
    "M-r" 'eyebrowse-switch-to-window-config-6
    "M-s" 'eyebrowse-switch-to-window-config-7
    "M-t" 'eyebrowse-switch-to-window-config-8
    "M-g" 'eyebrowse-switch-to-window-config-9))

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
