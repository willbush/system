{ inputs, pkgs, ... }:
{
  home.file = {
    ".config/chemacs-repo" = {
      source = inputs.chemacs;
      # Work around for home-manager not being able to copy instead of link.
      # see: https://github.com/nix-community/home-manager/issues/3090
      onChange = ''
        rm -rf ~/.config/emacs
        cp -a ~/.config/chemacs-repo/ ~/.config/emacs/
        chmod u+w ~/.config/emacs/ -R
      '';
    };
    ".config/chemacs/profiles.el" = {
      source = ../../configs/emacs/profiles.el;
    };
    ".config/emacs.default" = {
      source = ../../configs/emacs/default;
      recursive = true;
    };
  };

  home.packages = with pkgs; [
    hunspellDicts.en_US # used by jinx
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-unstable-pgtk;
    extraPackages = (
      epkgs:
      (with epkgs; [
        adaptive-wrap
        avy
        benchmark-init
        browse-at-remote
        cape
        consult
        consult-project-extra
        corfu
        dape
        dart-mode
        deadgrep
        define-word
        dired-narrow
        diredfl
        direnv
        dockerfile-mode
        doom-modeline
        doom-themes
        embark
        embark-consult
        evil
        evil-collection
        evil-exchange
        evil-numbers
        evil-surround
        evil-traces
        evil-visualstar
        exec-path-from-shell
        expand-region
        format-all
        gcmh
        general
        git-gutter
        git-gutter-fringe
        git-timemachine
        haskell-mode
        helpful
        hydra
        jinx
        just-mode
        keycast
        magit
        marginalia
        markdown-mode
        markdown-toc
        meow
        moody
        nerd-icons
        nerd-icons-completion
        nix-mode
        orderless
        org-modern
        org-roam
        ox-gfm
        pdf-tools
        powershell
        protobuf-mode
        pulsar
        rainbow-delimiters
        rust-mode
        smartparens
        sudo-edit
        swiper
        terraform-mode
        toc-org
        treesit-auto
        vertico
        vimrc-mode
        visual-fill-column
        vterm
        web-mode
        yaml-mode
        zoom
        zoxide
      ])
    );
  };
}
