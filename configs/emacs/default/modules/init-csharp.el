;;; -*- lexical-binding: t; -*-

(use-package csharp-mode
  :ensure nil ;; included in Emacs.
  :init

  (defun csharp-mode-setup ()
    (setq indent-tabs-mode t
          c-syntactic-indentation t
          fill-column 100
          truncate-lines t
          tab-width 4
          evil-shift-width 4
          c-basic-offset 4)

    (c-set-offset 'substatement-open 0))

  (add-hook 'csharp-mode-hook 'csharp-mode-setup)
  (add-hook 'csharp-ts-mode-hook 'csharp-mode-setup)

  (add-hook 'web-mode-hook
            (lambda ()
              (when (or (string-match "\\.csproj\\'" (buffer-file-name))
                        (string-match "\\.xaml\\'" (buffer-file-name))
                        (string-match "\\.props\\'" (buffer-file-name)))
                (progn
                  (setq indent-tabs-mode t)
                  (web-mode-use-tabs))))))

(provide 'init-csharp)
