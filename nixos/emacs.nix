{ pkgs, ... }:

{
  services.emacs.enable = true;
  programs.emacs = {
    enable = true;
    # Compile with imagemagick support so I can resize images.
    package = pkgs.emacs.override { inherit (pkgs) imagemagick; };
    extraPackages = (epkgs: (with epkgs; [
      attrap
      avy
      company
      company-nixos-options
      counsel
      counsel-projectile
      csharp-mode
      dante
      dashboard
      deadgrep
      define-word
      dired-narrow
      direnv
      disk-usage
      doom-modeline
      doom-themes
      esup
      evil
      evil-collection
      evil-exchange
      evil-magit
      evil-matchit
      evil-numbers
      evil-surround
      evil-tutor
      evil-visualstar
      expand-region
      fd-dired
      fill-column-indicator
      flyspell-correct-ivy
      general
      git-timemachine
      golden-ratio
      haskell-mode
      hindent
      hlint-refactor
      hydra
      ivy
      magit
      markdown-mode
      markdown-toc
      nav-flash
      nix-mode
      nix-sandbox
      nix-update
      org-re-reveal
      pdf-tools
      poporg
      powershell
      projectile
      rainbow-delimiters
      ranger
      smex
      use-package
      visual-fill-column
      which-key
      winum
      wttrin
      yaml-mode
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
        emacs -Q --load ~/.emacs.d/init.el --batch --eval '(byte-compile-file "~/.emacs.d/init.el")'
        emacs -Q --load ~/.emacs.d/init.el --batch --eval '(byte-recompile-directory "~/.emacs.d/src/" 0 t)'
      '';
    };
  };

  xresources.properties = {
    # Set some Emacs GUI properties in the .Xresources file because they are
    # expensive to set during initialization in Emacs lisp. This saves about
    # half a second on startup time. See the following link for more options:
    # https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html#Fonts
    "Emacs.menuBar" = false;
    "Emacs.toolBar" = false;
    "Emacs.verticalScrollBars" = false;
    "Emacs.Font" = "Hack:size=16";
  };

  # Home manager's emacs service doesn't provide a desktop entry for the emacs
  # client. Note the %F on the `Exec=` line passes any file name string to tell
  # emacs to open a file. I just use Albert to launch the emacs client so I
  # don't every really need that.
  xdg.dataFile."applications/emacsclient.desktop".text = ''
    [Desktop Entry]
    Name=Emacsclient
    GenericName=Text Editor
    Comment=Edit text
    MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
    Exec=emacsclient -c -a emacs %F
    Icon=emacs
    Type=Application
    Terminal=false
    Categories=Development;TextEditor;
    StartupWMClass=Emacs
    Keywords=Text;Editor;
  '';
}
