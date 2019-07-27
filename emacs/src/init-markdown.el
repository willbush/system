;;; -*- lexical-binding: t; -*-

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (setq show-trailing-whitespace t))))

(use-package markdown-toc :defer t)

(provide 'init-markdown)
