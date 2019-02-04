;;; -*- lexical-binding: t; -*-

(use-package powershell
  :mode ("\\.ps[dm]?1\\'" . powershell-mode)
  :commands (powershell-mode powershell))

(provide 'init-scripting)
