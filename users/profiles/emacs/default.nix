{ inputs, pkgs, emacsPackage ? pkgs.emacsNativeComp, ... }: {

  home.file = {
    ".config/emacs" = {
      source = inputs.chemacs;
    };
    ".config/chemacs/profiles.el" = {
      source = ../../../emacs/profiles.el;
    };
    ".config/emacs.default" = {
      source = ../../../emacs/default;
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
        browse-at-remote
        company
        company-tabnine
        counsel
        counsel-projectile
        csharp-mode
        dashboard
        dart-mode
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
        ivy-prescient
        lsp-haskell
        lsp-mode
        lsp-ui
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
