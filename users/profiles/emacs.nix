{ inputs, pkgs, emacsPackage ? pkgs.emacsNativeComp, ... }: {

  home.file = {
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

  services.emacs = {
    enable = true;
    client.enable = true;
  };

  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
  ];

  programs.emacs = {
    enable = true;
    package = emacsPackage;
    extraPackages = (epkgs:
      (with epkgs; [
        adaptive-wrap
        all-the-icons-ivy
        avy
        benchmark-init
        browse-at-remote
        company
        company-tabnine
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
