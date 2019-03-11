;;; -*- lexical-binding: t; -*-

(use-package powershell
  :pin melpa-stable
  :mode ("\\.ps[dm]?1\\'" . powershell-mode)
  :commands (powershell-mode powershell))

(use-package yaml-mode
  :mode ("\\.yaml\\'" . yaml-mode))

(provide 'init-scripting)
