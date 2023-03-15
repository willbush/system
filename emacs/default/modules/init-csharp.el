;;; -*- lexical-binding: t; -*-

(use-package csharp-mode
  :mode "\\.cs\\'"
  :init
  (add-hook 'csharp-mode-hook
            (lambda ()
              (setq indent-tabs-mode t
                    c-syntactic-indentation t
                    fill-column 100
                    truncate-lines t
                    tab-width 4
                    evil-shift-width 4
                    c-basic-offset 4)
              (c-set-offset 'substatement-open 0))))

(add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xaml\\'" . nxml-mode))

(add-hook 'nxml-mode-hook
          (lambda ()
            (when (or (string-match "\\.csproj\\'" (buffer-file-name))
                      (string-match "\\.xaml\\'" (buffer-file-name)))
              (setq indent-tabs-mode t))))

(provide 'init-csharp)
