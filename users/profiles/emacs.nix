{ inputs, pkgs, emacsPackage ? pkgs.emacsNativeComp, ... }:
{
  home = {
    sessionVariables = {
      NODEJS_16_X = "${pkgs.nodejs-16_x}/bin/node";
    };
    file = {
      ".config/chemacs-repo" = {
        source = inputs.chemacs;
        # Work around for home-manager not being able to copy instead of link.
        # see: https://github.com/nix-community/home-manager/issues/3090
        onChange = ''
          trash-put -f ~/.config/emacs
          cp -a ~/.config/chemacs-repo/ ~/.config/emacs/
          chmod u+w ~/.config/emacs/ -R
        '';
      };
      ".config/crafted-emacs-repo" = {
        source = inputs.crafted-emacs;
        onChange = ''
          trash-put -f ~/.config/crafted-emacs
          cp -a ~/.config/crafted-emacs-repo/ ~/.config/crafted-emacs/
          chmod u+w ~/.config/crafted-emacs/ -R
        '';
      };
      ".config/chemacs/profiles.el" = {
        source = ../../emacs/profiles.el;
      };
      ".config/emacs.default" = {
        source = ../../emacs/default;
        recursive = true;
      };
      ".config/emacs.crafted" = {
        source = ../../emacs/crafted;
        recursive = true;
      };
    };
  };

  services.emacs = {
    enable = true;
    client.enable = true;
  };

  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    nodejs-16_x # copilot.el only supports 12.x to 17.x, limited by upstream
  ];

  programs.emacs = {
    enable = true;
    package = emacsPackage;
    overrides = self: super: {
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
          packageRequires = [ dash editorconfig s ];

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
    };
    extraPackages = (epkgs:
      (with epkgs; [
        adaptive-wrap
        all-the-icons-ivy
        avy
        benchmark-init
        browse-at-remote
        company
        company-box
        copilot
        counsel
        counsel-projectile
        csharp-mode
        dart-mode
        dashboard
        deadgrep
        define-word
        dired-narrow
        diredfl
        direnv
        disk-usage
        dockerfile-mode
        doom-modeline
        doom-themes
        edwina
        eglot # TODO remove when Emacs 29 hits master because it's included.
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
        ivy-prescient
        magit
        magit-delta
        markdown-mode
        markdown-toc
        meson-mode
        mips-mode
        nav-flash
        nix-mode
        nix-sandbox
        nix-update
        nixpkgs-fmt
        org-download
        org-pomodoro
        page-break-lines # emacs-dashboard dependency
        pandoc-mode
        password-generator
        password-store
        pdf-tools
        powershell
        projectile
        racket-mode
        rainbow-delimiters
        ranger
        rustic
        sudo-edit
        swiper
        terraform-mode
        toc-org
        tree-sitter
        tree-sitter-langs
        typescript-mode
        use-package
        vimrc-mode
        visual-fill-column
        vterm
        web-mode
        which-key
        wttrin
        yaml-mode
        yasnippet
        zoom
        zoxide
      ]));
  };
}
