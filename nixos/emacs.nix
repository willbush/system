{ pkgs, ... }:

let emacs-overlay = import (import ./nix/sources.nix)."emacs-overlay";
in {

  nixpkgs.overlays = [ emacs-overlay ];

  services.emacs = {
    enable = true;
    # temporarily disable to work around a desktop file collision issue
    # https://github.com/nix-community/emacs-overlay/issues/58
    # client.enable = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
    extraPackages = (epkgs:
      (with epkgs; [
        adaptive-wrap
        aggressive-indent
        all-the-icons-dired
        all-the-icons-ivy
        avy
        browse-at-remote
        company
        company-tabnine
        counsel
        counsel-projectile
        csharp-mode
        dashboard
        deadgrep
        define-word
        dired-narrow
        diredfl
        direnv
        disk-usage
        doom-modeline
        doom-themes
        edwina
        esup
        evil
        evil-collection
        evil-exchange
        evil-matchit
        evil-numbers
        evil-surround
        evil-traces
        evil-tutor
        evil-visualstar
        exec-path-from-shell
        expand-region
        eyebrowse
        fd-dired
        fill-column-indicator
        flycheck
        flyspell-correct-ivy
        format-all
        gcmh
        general
        git-gutter
        git-gutter-fringe
        git-timemachine
        haskell-mode
        helpful
        hydra
        ivy
        ivy-pass
        lsp-haskell
        lsp-mode
        lsp-ui
        magit
        markdown-mode
        markdown-toc
        nav-flash
        nix-mode
        nix-sandbox
        nix-update
        omnisharp
        password-generator
        password-store
        pdf-tools
        powershell
        projectile
        rainbow-delimiters
        ranger
        rustic
        smex
        sudo-edit
        swiper
        use-package
        vimrc-mode
        visual-fill-column
        vterm
        which-key
        wttrin
        yaml-mode
        yasnippet
        zoom
      ]));
  };

  home.file = {
    ".emacs.d" = {
      source = ../emacs;
      recursive = true;
    };
  };
}
