;;; -*- lexical-binding: t; -*-

;;
;;; Markdown

(use-package markdown-mode
  :defer 2 ;; load if not loaded already because eldoc uses it in rust-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq show-trailing-whitespace t))))


(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode))
  :init
  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq show-trailing-whitespace t))))


(use-package markdown-toc
  :after markdown-mode)


(provide 'init-markup-languages)
