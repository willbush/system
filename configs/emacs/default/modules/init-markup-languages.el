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


;; markdown preview
(use-package grip-mode
  :commands grip-mode
  :init
  (general-def
    :keymaps 'markdown-mode-command-map
    "g" 'grip-mode)
  :config
  (setq grip-github-user "willbush")
  (setq grip-github-password
        (funcall
         (lambda ()
           (nth 0 (process-lines "gopass" "show" "will/websites/general/github-pat-grip"))))))


(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode))
  :init
  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq show-trailing-whitespace t))))


(use-package markdown-toc
  :after markdown-mode)

;;
;;; Org and Markdown related tools

(use-package pandoc-mode
  :hook
  ((markdown-mode . pandoc-mode)
   (org-mode . pandoc-mode)
   (pandoc-mode . pandoc-load-default-settings))

  :config
  (general-def
    :prefix ","
    :states 'normal
    :keymaps 'pandoc-mode-map
    "p" 'pandoc-main-hydra/body))

(provide 'init-markup-languages)
