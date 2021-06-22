{ pkgs, emacsPackage ? pkgs.emacsGcc, ... }: {

  home.file = {
    ".emacs.d" = {
      source = ../../emacs;
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
        aggressive-indent
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
        mips-mode
        nav-flash
        nix-mode
        nix-sandbox
        nix-update
        omnisharp
        org-download
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
}
