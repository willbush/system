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
        golden-ratio
        haskell-mode
        helpful
        hydra
        ivy
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
        which-key
        wttrin
        yaml-mode
        yasnippet
      ]));
  };

  home.file = {
    ".emacs.d" = {
      source = ../emacs;
      recursive = true;
      # Since home-manager deploys my config and is immutable, I might as well
      # byte compile everything deployed this way. From my profiling with `esup`
      # I haven't been able to see much of any gain. The main advantage is an
      # opportunity to see compiler warnings.
      onChange = ''
        # Manually delete compiled files myself. There are flags that can be
        # passed to elisp functions to force it to always recompile, but I have had issues
        # resolved by deleting .elc files despite using those flags.
        # I'm using the force flag to ignore the files if any don't exist.
        rm ~/.emacs.d/*init.elc -fv
        rm ~/.emacs.d/src/*.elc -fv

        # This will incorrectly report warnings if .elc files exists before
        # running this. I did test that this is still correctly reporting
        # warnings when the .elc files are removed.
        emacs -Q -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el \
          -batch -f batch-byte-compile ~/.emacs.d/*init.el ~/.emacs.d/src/*.el
      '';
    };
  };
}
