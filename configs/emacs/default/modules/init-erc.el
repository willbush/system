;;; -*- lexical-binding: t; -*-

(use-package erc
  :commands erc
  :init
  (setq erc-server "irc.libera.chat"
        erc-nick "willbush"
        erc-port 6697 ;; TLS port number

        erc-autojoin-channels-alist
        '(("libera.chat"
           "##rust"
           "#emacs"
           "#haskell"
           "#haskell-beginners"
           "#nixos"
           "#nixos-emacs"))))

(provide 'init-erc)
