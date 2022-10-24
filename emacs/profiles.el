(
 ("default" . ((user-emacs-directory . "~/.config/emacs.default")))
 ;; Mutable is for when I want to iterate fast on changes to my config without
 ;; having to 'nixos-rebuild switch'.
 ("mutable" . ((user-emacs-directory . "~/system/emacs/default")))
 ("rewrite" . ((user-emacs-directory . "~/.config/emacs.rewrite")))
 ("crafted" . ((user-emacs-directory . "~/.config/crafted-emacs")
               (env . (("CRAFTED_EMACS_HOME" . "~/.config/emacs.crafted")))))
)
