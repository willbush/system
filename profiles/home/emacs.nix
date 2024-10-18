{ inputs, pkgs, ... }:
{
  home = {
    sessionVariables = {
      EDITOR = "emacs";
    };
    file = {
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
  };

  # services.emacs = {
  #   enable = true;
  #   package = pkgs.emacs-pgtk;
  #   client.enable = true;
  #   defaultEditor = true;
  # };

  home.packages = with pkgs; [
    hunspellDicts.en_US # used by jinx
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages = (
      epkgs:
      (with epkgs; [
        adaptive-wrap
        all-the-icons-completion
        avy
        benchmark-init
        browse-at-remote
        consult
        consult-project-extra
        corfu
        dape
        dart-mode
        dashboard
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
        gptel
        grip-mode
        haskell-mode
        helpful
        hydra
        jinx
        just-mode
        keycast
        magit
        magit-delta
        marginalia
        markdown-mode
        markdown-toc
        moody
        nix-mode
        orderless
        org-modern
        org-roam
        ox-gfm
        page-break-lines # emacs-dashboard dependency
        password-generator
        password-store
        pdf-tools
        powershell
        protobuf-mode
        pulsar
        rainbow-delimiters
        ranger
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
