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
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    overrides = _self: _super: {
      # I install the packages below by hand because they're not in MELPA, and I
      # don't want to incur the startup cost of using straight.el.
      copilot =
        let
          rev = inputs.copilot-el.shortRev;
        in
        with pkgs;
        with pkgs.emacsPackages;
        melpaBuild {
          pname = "copilot";
          ename = "copilot";
          version = inputs.copilot-el.lastModifiedDate;
          commit = rev;
          packageRequires = [
            dash
            editorconfig
            s
            jsonrpc
          ];

          src = fetchFromGitHub {
            inherit rev;
            owner = "zerolfx";
            repo = "copilot.el";
            sha256 = inputs.copilot-el.narHash;
          };

          recipe = writeText "recipe" ''
            (copilot
              :repo "zerolfx/copilot.el"
              :fetcher github
              :files ("*.el" "dist"))
          '';
          meta.description = "Emacs plugin for GitHub Copilot";
        };
      eglot-x =
        let
          rev = inputs.eglot-x.shortRev;
        in
        with pkgs;
        with pkgs.emacsPackages;
        melpaBuild {
          pname = "eglot-x";
          ename = "eglot-x";
          version = inputs.eglot-x.lastModifiedDate;
          commit = rev;

          src = fetchFromGitHub {
            inherit rev;
            owner = "nemethf";
            repo = "eglot-x";
            sha256 = inputs.eglot-x.narHash;
          };

          recipe = writeText "recipe" ''
            (eglot-x
              :repo "nemethf/eglot-x"
              :fetcher github
              :files ("*.el" "dist"))
          '';
          meta.description = "Protocol extensions for Eglot ";
        };
    };
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
        copilot
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
        eglot-x
        embark
        embark-consult
        evil
        evil-collection
        evil-exchange
        evil-numbers
        evil-surround
        evil-traces
        evil-tutor
        evil-visualstar
        exec-path-from-shell
        expand-region
        fill-column-indicator
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
        keycast
        magit
        magit-delta
        marginalia
        markdown-mode
        markdown-toc
        nav-flash
        nix-mode
        orderless
        org-modern
        org-roam
        ox-gfm
        page-break-lines # emacs-dashboard dependency
        pandoc-mode
        password-generator
        password-store
        pdf-tools
        powershell
        protobuf-mode
        rainbow-delimiters
        ranger
        rust-mode
        sudo-edit
        swiper
        terraform-mode
        toc-org
        treesit-auto
        typescript-mode
        vertico
        vimrc-mode
        visual-fill-column
        vterm
        web-mode
        which-key
        yaml-mode
        zoom
        zoxide
      ])
    );
  };
}
